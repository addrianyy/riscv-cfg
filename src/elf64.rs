const PT_LOAD: u32 = 1;

trait BinaryReadable {
    fn read(buffer: &[u8], offset: usize) -> Self;
}

macro_rules! impl_binary_readable {
    ($readable_type: ty) => {
        impl BinaryReadable for $readable_type {
            fn read(buffer: &[u8], offset: usize) -> Self {
                use std::convert::TryInto;

                let end = offset + std::mem::size_of::<Self>();

                Self::from_le_bytes(buffer[offset..end].try_into().unwrap())
            }
        }
    };
    ($($readable_type: ty),*) => {
        $( impl_binary_readable! { $readable_type } )*
    };
}

trait BinaryRead {
    fn read<T: BinaryReadable>(&self, offset: usize) -> T;
}

impl BinaryRead for [u8] {
    fn read<T: BinaryReadable>(&self, offset: usize) -> T {
        BinaryReadable::read(self, offset)
    }
}

impl_binary_readable! { u8, u16, u32, u64 }
impl_binary_readable! { i8, i16, i32, i64 }

#[derive(Clone, Debug)]
pub struct Section {
    pub name:        String,
    pub sec_type:    u32,
    pub flags:       u64,
    pub virt_addr:   u64,
    pub file_offset: usize,
    pub file_size:   usize,
}

#[derive(Clone, Debug)]
pub struct Segment {
    pub seg_type:    u32,
    pub flags:       u32,
    pub virt_addr:   u64,
    pub virt_size:   u64,
    pub file_offset: usize,
    pub file_size:   usize,
}

#[derive(Clone, Debug)]
pub struct Elf {
    pub base_address: u64,
    pub entrypoint:   u64,
    pub sections:     Vec<Section>,
    pub segments:     Vec<Segment>,
}

impl Elf {
    pub fn parse(buffer: &[u8]) -> Self {
        let segment_table: u64 = buffer.read(0x20);
        let section_table: u64 = buffer.read(0x28);

        let segment_header_size: u16 = buffer.read(0x36);
        let section_header_size: u16 = buffer.read(0x3A);

        let segment_count: u16 = buffer.read(0x38);
        let section_count: u16 = buffer.read(0x3C);

        let string_section_index: u16 = buffer.read(0x3E);

        let entrypoint: u64 = buffer.read(0x18);

        let mut section_name_offsets = Vec::with_capacity(section_count as usize);

        let mut sections = Vec::with_capacity(section_count as usize);
        let mut segments = Vec::with_capacity(segment_count as usize);

        for i in 0..segment_count {
            let segment = (segment_table + i as u64 * segment_header_size as u64) as usize;

            let seg_type: u32 = buffer.read(segment);
            let flags:    u32 = buffer.read(segment + 0x04);

            let file_offset: u64 = buffer.read(segment + 0x08);
            let virt_addr:   u64 = buffer.read(segment + 0x10);
            let file_size:   u64 = buffer.read(segment + 0x20);
            let virt_size:   u64 = buffer.read(segment + 0x28);

            segments.push(Segment {
                seg_type,
                flags,
                virt_addr,
                virt_size,
                file_offset: file_offset as usize,
                file_size:   file_size as usize,
            });
        }

        for i in 0..section_count {
            let section = (section_table + i as u64 * section_header_size as u64) as usize;

            let name_offset: u32 = buffer.read(section);
            let sec_type:    u32 = buffer.read(section + 0x04);
            let flags:       u64 = buffer.read(section + 0x08);

            let virt_addr:   u64 = buffer.read(section + 0x10);
            let file_offset: u64 = buffer.read(section + 0x18);
            let file_size:   u64 = buffer.read(section + 0x20);

            section_name_offsets.push(name_offset);

            sections.push(Section {
                name: String::new(),
                sec_type,
                flags,
                virt_addr,
                file_offset: file_offset as usize,
                file_size:   file_size as usize,
            });
        }

        let strings_offset = sections[string_section_index as usize].file_offset;

        for (i, section) in sections.iter_mut().enumerate() {
            let name = &buffer[strings_offset as usize + section_name_offsets[i] as usize..];
            let null_terminator = name.iter().position(|&v| v == 0).unwrap();
            let name = &name[..null_terminator];

            section.name = String::from_utf8_lossy(&name).to_string();
        }

        let base_address = segments.iter()
            .filter(|seg| seg.seg_type == PT_LOAD)
            .min_by_key(|seg| seg.virt_addr)
            .expect("ELF has no loadable sections.")
            .virt_addr;

        Self {
            base_address,
            entrypoint,
            sections,
            segments,
        }
    }

    pub fn section_by_name(&self, name: &str) -> Option<&Section> {
        self.sections.iter().find(|sec| sec.name == name)
    }

    pub fn map(&self, buffer: &[u8]) -> Vec<u8> {
        let mut mapped = Vec::with_capacity(buffer.len());

        macro_rules! pad {
            ($amount: expr) => { mapped.extend(vec![0u8; $amount]); }
        }

        for segment in self.segments.iter() {
            if segment.seg_type != PT_LOAD {
                continue;
            }

            let virt_offset = (segment.virt_addr - self.base_address) as usize;

            pad!(virt_offset.checked_sub(mapped.len()).unwrap());

            let size  = std::cmp::min(segment.file_size, segment.virt_size as usize);
            let start = segment.file_offset;
            let end   = start + size;
            mapped.extend_from_slice(&buffer[start..end]);

            pad!(segment.virt_size as usize - size);
        }

        pad!(((mapped.len() + 4095) & !4095) - mapped.len());

        mapped
    }
}

