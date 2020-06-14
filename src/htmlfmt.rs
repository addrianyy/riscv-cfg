use super::displayinst::InstructionFormatter;
use super::rv64::Register;

pub struct HTMLFormatter {
    pub reg_color:    String,
    pub mnem_color:   String,
    pub imm_color:    String,
    pub addr_color:   String,
    pub mnem_padding: usize,
}

impl InstructionFormatter for HTMLFormatter {
    fn fmt_reg(&self, reg: Register) -> String {
        format!(r#"<font color="{}">{}</font>"#, self.reg_color, reg)
    }
    
    fn fmt_mnem(&self, mnem: &str) -> String {
        let spacing    = self.mnem_padding.saturating_sub(mnem.len());
        let mut padded = format!(r#"<font color="{}">{}</font>"#, self.mnem_color, mnem);

        for _ in 0..spacing {
            padded.push_str("&nbsp;");
        }

        padded
    }

    fn fmt_imm(&self, imm: i64) -> String {
        format!(r#"<font color="{}">{}</font>"#, self.imm_color, imm)
    }

    fn fmt_addr(&self, addr: u64) -> String {
        format!(r#"<font color="{}">0x{:X}</font>"#, self.addr_color, addr)
    }
}
