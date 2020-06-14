trait BinaryRead {
    fn read<T: Copy>(&self, offset: u64) -> T;
}

impl BinaryRead for [u8] {
    fn read<T: Copy>(&self, offset: u64) -> T {
        assert!(offset as usize + std::mem::size_of::<T>() <= self.len(),
            "Tried to read out of bounds memory.");

        unsafe {
            let ptr = self.as_ptr().offset(offset as isize) as *const T;
            ptr.read_unaligned()
        }
    }
}

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
            let segment = segment_table + i as u64 * segment_header_size as u64;

            let seg_type: u32 = buffer.read(segment + 0x00);
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
            let section = section_table + i as u64 * section_header_size as u64;

            let name_offset: u32 = buffer.read(section + 0x00);
            let sec_type:    u32 = buffer.read(section + 0x04);
            let flags:       u64 = buffer.read(section + 0x08);

            let virt_addr:   u64 = buffer.read(section + 0x10);
            let file_offset: u64 = buffer.read(section + 0x18);
            let file_size:   u64 = buffer.read(section + 0x20);

            section_name_offsets.push(name_offset);

            sections.push(Section {
                name: String::from(""),
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

        const PT_LOAD: u32 = 1;

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
}

