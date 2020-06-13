trait Parse {
    fn parse<T: Copy>(&self, off: u32) -> T;
}

impl Parse for [u8] {
    fn parse<T: Copy>(&self, off: u32) -> T {
        assert!(
            off as usize + std::mem::size_of::<T>() <= self.len(),
            "Tried to read out of bounds memory."
        );

        unsafe {
            let ptr = self.as_ptr().offset(off as isize);
            (ptr as *const T).read()
        }
    }
}

pub fn map_elf64(buf: &[u8]) -> (u64, u64, Vec<u8>) {
    let segment_table: u64 = buf.parse(0x20); // e_shoff
    let segment_count: u16 = buf.parse(0x38); // e_phnum
    let segment_header_size: u16 = buf.parse(0x36); // e_phentsize
    let entrypoint: u64 = buf.parse(0x18); // e_entry

    assert_eq!(segment_header_size, 0x38, "Unexpected program header size.");

    let mut mapped = Vec::with_capacity(buf.len());
    let mut base = None;

    for i in 0..(segment_count as u64) {
        let segment = (segment_table + i * segment_header_size as u64) as u32;

        let segment_type: u32 = buf.parse(segment); // p_type
        if segment_type != 1 {
            continue;
        }

        let file_off: u64 = buf.parse(segment + 0x08); // p_offset
        let vaddr: u64 = buf.parse(segment + 0x10); // p_vaddr
        let file_size: u64 = buf.parse(segment + 0x20); // p_filesz
        let virt_size: u64 = buf.parse(segment + 0x28); // p_memsz

        if base == None {
            base = Some(vaddr);
        }

        let virt_offset = vaddr - base.unwrap();

        let pad = virt_offset as usize - mapped.len();
        mapped.extend(vec![0u8; pad]);

        let raw = file_off as usize;
        let len = std::cmp::min(file_size, virt_size);
        mapped.extend_from_slice(&buf[raw..raw + len as usize]);

        let pad = virt_size - file_size;
        mapped.extend(vec![0u8; pad as usize]);
    }

    let pad = (((mapped.len() + 4095) / 4096) * 4096) - mapped.len();
    mapped.extend(vec![0u8; pad as usize]);

    assert!(base.is_some(), "ELF has no loadable sections.");

    (base.unwrap(), entrypoint, mapped)
}
