use super::elf64::Elf;
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub struct Function {
    pub name:  String,
    pub start: u64,
    pub size:  u64,
}

pub type FunctionList = Vec<Function>;
 
pub fn load_from_debug_info(file_buffer: &[u8], elf: &Elf) -> Result<Vec<Function>, gimli::Error> {
    let load_section = |id: gimli::SectionId| -> Result<Cow<[u8]>, gimli::Error> {
        match elf.section_by_name(id.name()) {
            Some(section) => {
                let start = section.file_offset;
                let end   = start + section.file_size;

                Ok(Cow::Borrowed(&file_buffer[start..end][..]))
            }
            _ => Ok(Cow::Borrowed(&[][..])),
        }
    };

    let load_section_sup = |_| Ok(Cow::Borrowed(&[][..]));

    let dwarf_cow = gimli::Dwarf::load(&load_section, &load_section_sup)?;

    let borrow_section: &dyn for<'a> Fn(&'a Cow<[u8]>,
    ) -> gimli::EndianSlice<'a, gimli::RunTimeEndian> =
        &|section| gimli::EndianSlice::new(&*section, gimli::RunTimeEndian::Little);

    let dwarf = dwarf_cow.borrow(&borrow_section);

    let mut iter = dwarf.units();

    let mut functions = Vec::new();

    while let Some(header) = iter.next()? {
        let unit = dwarf.unit(header)?;
        let mut entries = unit.entries();

        while let Some((_, entry)) = entries.next_dfs()? {
            use gimli::read::AttributeValue;

            if entry.tag() != gimli::constants::DW_TAG_subprogram {
                continue;
            }

            let name = match entry.attr(gimli::constants::DW_AT_name)? {
                Some(a) => a.value(),
                None    => continue, 
            };

            let name = dwarf.attr_string(&unit, name)?;
            let name = name.to_string_lossy();

            let low_pc  = entry.attr(gimli::constants::DW_AT_low_pc)?;
            let high_pc = entry.attr(gimli::constants::DW_AT_high_pc)?;

            let (low_pc, high_pc) = match (low_pc, high_pc) {
                (Some(a), Some(b)) => (a.value(), b.value()),
                _                  => continue,
            };

            let start = match low_pc {
                AttributeValue::Addr(addr) => addr,
                _                          => panic!(),
            };

            let size = match high_pc {
                AttributeValue::Addr(addr)  => start - addr,
                AttributeValue::Udata(size) => size,
                _                           => panic!(),
            };

            functions.push(Function {
                name: name.to_string(),
                start,
                size,
            });
        }
    }

    Ok(functions)
}
