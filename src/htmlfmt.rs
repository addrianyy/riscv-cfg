use super::displayinst::InstructionFormatter;
use super::rv64::Register;
use super::funclist::FunctionList;

pub struct HTMLFormatter<'a> {
    pub reg_color:    String,
    pub mnem_color:   String,
    pub imm_color:    String,
    pub addr_color:   String,
    pub mnem_padding: usize,
    pub funcs:        &'a FunctionList,
}

impl InstructionFormatter for HTMLFormatter<'_> {
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
        if let Some(func) = self.funcs.iter().find(|f| f.start == addr) {
            return func.name.clone();
        }

        format!(r#"<font color="{}">0x{:X}</font>"#, self.addr_color, addr)
    }
}
