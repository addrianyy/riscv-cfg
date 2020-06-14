use std::fmt;
use super::rv64::{Instruction, Register};

pub trait InstructionFormatter {
    fn fmt_mnem(&self, mnem: &str) -> String;
    fn fmt_addr(&self, addr: u64) -> String;
    fn fmt_imm(&self, imm: i64) -> String;
    fn fmt_reg(&self, reg: Register) -> String;
}

pub struct DisplayInstruction<'a, T: InstructionFormatter> {
    inst:      Instruction,
    pc:        u64,
    formatter: &'a T,
}

impl<'a, T: InstructionFormatter> DisplayInstruction<'a, T> {
    pub fn new(inst: Instruction, pc: u64, formatter: &'a T) -> Self {
        Self {
            inst,
            pc,
            formatter,
        }
    }

    fn fmt_mnem(&self, mnem: &str) -> String {
        self.formatter.fmt_mnem(mnem)
    }

    fn fmt_addr(&self, addr: u64) -> String {
        self.formatter.fmt_addr(addr)
    }

    fn fmt_imm(&self, imm: i64) -> String {
        self.formatter.fmt_imm(imm)
    }

    fn fmt_reg(&self, reg: Register) -> String {
        self.formatter.fmt_reg(reg)
    }

    fn display_pseudo(&self, f: &mut fmt::Formatter<'_>, handled: &mut bool) -> fmt::Result {
        use Instruction::*;

        *handled = true;

        match self.inst {
            Addi { rd: Register::Zero, rs1: Register::Zero, imm: 0 } => {
                write!(f, "{}", self.fmt_mnem("nop"))?;
            },
            Addi { rd, rs1: Register::Zero, imm } => {
                write!(f, "{} {}, {}", self.fmt_mnem("mv"), self.fmt_reg(rd),
                    self.fmt_imm(imm))?;
            },
            Addi { rd, rs1, imm: 0 } => {
                write!(f, "{} {}, {}", self.fmt_mnem("mv"), self.fmt_reg(rd),
                    self.fmt_reg(rs1))?;
            },
            Jal { rd: Register::Zero, imm } => {
                write!(f, "{} {}", self.fmt_mnem("j"),
                    self.fmt_addr(self.pc.wrapping_add(imm as u64)))?;
            }
            Jal { rd: Register::Ra, imm } => {
                write!(f, "{} {}", self.fmt_mnem("jal"), 
                    self.fmt_addr(self.pc.wrapping_add(imm as u64)))?;
            }
            Jalr { rd: Register::Zero, rs1: Register::Ra, imm: 0 } => {
                write!(f, "{}", self.fmt_mnem("ret"))?;
            }
            Jalr { rd: Register::Ra, rs1, imm: 0 } => {
                write!(f, "{} {}", self.fmt_mnem("jalr"), self.fmt_reg(rs1))?;
            }
            Jalr { rd: Register::Zero, rs1, imm: 0 } => {
                write!(f, "{} {}", self.fmt_mnem("jr"), self.fmt_reg(rs1))?;
            }
            _ => *handled = false,
        }

        Ok(())
    }
}

impl<T: InstructionFormatter> fmt::Display for DisplayInstruction<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Instruction::*;

        let mut handled = false;
        self.display_pseudo(f, &mut handled)?;

        if handled {
            return Ok(());
        }

        match self.inst {
            Lui { imm, rd } => {
                write!(f, "{} {}, {}", self.fmt_mnem("mv"), self.fmt_reg(rd),
                    self.fmt_imm(imm))?;
            }
            Jalr { imm, rs1, rd } => {
                write!(f, "{} {}, {}, {}", self.fmt_mnem("jalr"), self.fmt_reg(rd),
                    self.fmt_reg(rs1), self.fmt_imm(imm))?;
            }
            Auipc { imm, rd } |
            Jal   { imm, rd } => {
                let instruction_name = match self.inst {
                    Auipc { .. } => "auipc",
                    Jal   { .. } => "jal",
                    _            => unreachable!(),
                };
                
                write!(f, "{} {}, {}", self.fmt_mnem(instruction_name), self.fmt_reg(rd),
                    self.fmt_addr(self.pc.wrapping_add(imm as u64)))?;
            }
            Beq  { imm, rs1, rs2 } |
            Bne  { imm, rs1, rs2 } |
            Blt  { imm, rs1, rs2 } |
            Bge  { imm, rs1, rs2 } |
            Bltu { imm, rs1, rs2 } |
            Bgeu { imm, rs1, rs2 } => {
                let instruction_name = match self.inst {
                    Beq  { .. } => "beq",
                    Bne  { .. } => "bne",
                    Blt  { .. } => "blt",
                    Bge  { .. } => "bge",
                    Bltu { .. } => "bltu",
                    Bgeu { .. } => "bgeu",
                    _           => unreachable!(),
                };

                write!(f, "{} {}, {}, {}", self.fmt_mnem(instruction_name), self.fmt_reg(rs1),
                    self.fmt_reg(rs2), self.fmt_addr(self.pc.wrapping_add(imm as u64)))?;
            }
            Lb  { imm, rs1, rd } |
            Lh  { imm, rs1, rd } |
            Lw  { imm, rs1, rd } |
            Ld  { imm, rs1, rd } |
            Lbu { imm, rs1, rd } |
            Lhu { imm, rs1, rd } |
            Lwu { imm, rs1, rd } => {
                let instruction_name = match self.inst {
                    Lb  { .. } => "lb",
                    Lh  { .. } => "lh",
                    Lw  { .. } => "lw",
                    Ld  { .. } => "ld",
                    Lbu { .. } => "lbu",
                    Lhu { .. } => "lhu",
                    Lwu { .. } => "lwu",
                    _          => unreachable!(),
                };

                write!(f, "{} {}, ", self.fmt_mnem(instruction_name), self.fmt_reg(rd))?;

                if rs1 == Register::Zero {
                    write!(f, "({})", self.fmt_addr(imm as u64))?;
                } else {
                    match imm {
                        0          => write!(f, "({})", self.fmt_reg(rs1))?,
                        x if x > 0 => write!(f, "({}+{})", self.fmt_reg(rs1), self.fmt_imm(x))?,
                        x if x < 0 => write!(f, "({}-{})", self.fmt_reg(rs1), self.fmt_imm(-x))?,
                        _          => unreachable!(),
                    }
                }
            }
            Sb { imm, rs1, rs2 } |
            Sh { imm, rs1, rs2 } |
            Sw { imm, rs1, rs2 } |
            Sd { imm, rs1, rs2 } => {
                let instruction_name = match self.inst {
                    Sb { .. } => "sb",
                    Sh { .. } => "sh",
                    Sw { .. } => "sw",
                    Sd { .. } => "sd",
                    _         => unreachable!(),
                };

                write!(f, "{} ", self.fmt_mnem(instruction_name))?;

                if rs1 == Register::Zero {
                    write!(f, "({})", self.fmt_addr(imm as u64))?;
                } else {
                    match imm {
                        0          => write!(f, "({})", self.fmt_reg(rs1))?,
                        x if x > 0 => write!(f, "({}+{})", self.fmt_reg(rs1), self.fmt_imm(x))?,
                        x if x < 0 => write!(f, "({}-{})", self.fmt_reg(rs1), self.fmt_imm(-x))?,
                        _          => unreachable!(),
                    }
                }

                write!(f, ", {}", self.fmt_reg(rs2))?;
            }
            Addi  { imm, rs1, rd } |
            Xori  { imm, rs1, rd } |
            Ori   { imm, rs1, rd } |
            Andi  { imm, rs1, rd } |
            Addiw { imm, rs1, rd } => {
                let instruction_name = match self.inst {
                    Addi  { .. } => "addi",
                    Xori  { .. } => "xori",
                    Ori   { .. } => "ori",
                    Andi  { .. } => "andi",
                    Addiw { .. } => "addiw",
                    _            => unreachable!(),
                };

                write!(f, "{} {}, {}, {}", self.fmt_mnem(instruction_name), self.fmt_reg(rd),
                    self.fmt_reg(rs1), self.fmt_imm(imm))?;
            }
            Slli  { shamt, rs1, rd } |
            Srli  { shamt, rs1, rd } |
            Srai  { shamt, rs1, rd } |
            Slliw { shamt, rs1, rd } |
            Srliw { shamt, rs1, rd } |
            Sraiw { shamt, rs1, rd } => {
                let instruction_name = match self.inst {
                    Slli  { .. } => "slli",
                    Srli  { .. } => "srli",
                    Srai  { .. } => "srai",
                    Slliw { .. } => "slliw",
                    Srliw { .. } => "srliw",
                    Sraiw { .. } => "sraiw",
                    _            => unreachable!(),
                };

                write!(f, "{} {}, {}, {}", self.fmt_mnem(instruction_name), self.fmt_reg(rd),
                    self.fmt_reg(rs1), self.fmt_imm(shamt as i64))?;
            }
            Add    { rs1, rs2, rd } | 
            Sub    { rs1, rs2, rd } | 
            Sll    { rs1, rs2, rd } | 
            Slt    { rs1, rs2, rd } | 
            Sltu   { rs1, rs2, rd } | 
            Xor    { rs1, rs2, rd } | 
            Srl    { rs1, rs2, rd } | 
            Sra    { rs1, rs2, rd } | 
            Or     { rs1, rs2, rd } | 
            And    { rs1, rs2, rd } | 
            Addw   { rs1, rs2, rd } | 
            Subw   { rs1, rs2, rd } | 
            Sllw   { rs1, rs2, rd } | 
            Srlw   { rs1, rs2, rd } | 
            Sraw   { rs1, rs2, rd } |
            Mul    { rs1, rs2, rd } |
            Mulh   { rs1, rs2, rd } |
            Mulhsu { rs1, rs2, rd } |
            Mulhu  { rs1, rs2, rd } |
            Div    { rs1, rs2, rd } |
            Divu   { rs1, rs2, rd } |
            Rem    { rs1, rs2, rd } |
            Remu   { rs1, rs2, rd } |
            Mulw   { rs1, rs2, rd } |
            Divw   { rs1, rs2, rd } |
            Divuw  { rs1, rs2, rd } |
            Remw   { rs1, rs2, rd } |
            Remuw  { rs1, rs2, rd } => {
                let instruction_name = match self.inst {
                    Add    { .. } => "add", 
                    Sub    { .. } => "sub", 
                    Sll    { .. } => "sll", 
                    Slt    { .. } => "slt", 
                    Sltu   { .. } => "sltu", 
                    Xor    { .. } => "xor", 
                    Srl    { .. } => "srl", 
                    Sra    { .. } => "sra", 
                    Or     { .. } => "or", 
                    And    { .. } => "and", 
                    Addw   { .. } => "addw", 
                    Subw   { .. } => "subw", 
                    Sllw   { .. } => "sllw", 
                    Srlw   { .. } => "srlw", 
                    Sraw   { .. } => "sraw", 
                    Mul    { .. } => "mul",
                    Mulh   { .. } => "mulh",
                    Mulhsu { .. } => "mulhsu",
                    Mulhu  { .. } => "mulhu",
                    Div    { .. } => "div",
                    Divu   { .. } => "divu",
                    Rem    { .. } => "rem",
                    Remu   { .. } => "remu",
                    Mulw   { .. } => "mulw",
                    Divw   { .. } => "divw",
                    Divuw  { .. } => "divuw",
                    Remw   { .. } => "remw",
                    Remuw  { .. } => "remuw",
                    _           => unreachable!(),
                };

                write!(f, "{} {}, {}, {}", self.fmt_mnem(instruction_name), self.fmt_reg(rd),
                    self.fmt_reg(rs1), self.fmt_reg(rs2))?;
            }
            Slti  { imm, rs1, rd } |
            Sltiu { imm, rs1, rd } => {
                let instruction_name = match self.inst {
                    Slti  { .. } => "slti",
                    Sltiu { .. } => "sltiu",
                    _            => unreachable!(),
                };

                write!(f, "{} {}, {}, {}", self.fmt_mnem(instruction_name), self.fmt_reg(rd),
                    self.fmt_reg(rs1), self.fmt_imm(imm))?;
            }
            Fence { .. } => write!(f, "{}", self.fmt_mnem("mfence"))?,
            Ecall        => write!(f, "{}", self.fmt_mnem("ecall"))?,
            Ebreak       => write!(f, "{}", self.fmt_mnem("ebreak"))?,
            Undefined    => write!(f, "{}", self.fmt_mnem("ud"))?,
        }

        Ok(())
    }
}
