use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Register {
    Zero,
    Ra,
    Sp,
    Gp,
    Tp,
    T0,
    T1,
    T2,
    S0,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

impl Register {
    fn from_number(number: u32) -> Option<Register> {
        match number {
            0  => Some(Register::Zero),
            1  => Some(Register::Ra),
            2  => Some(Register::Sp),
            3  => Some(Register::Gp),
            4  => Some(Register::Tp),
            5  => Some(Register::T0),
            6  => Some(Register::T1),
            7  => Some(Register::T2),
            8  => Some(Register::S0),
            9  => Some(Register::S1),
            10 => Some(Register::A0),
            11 => Some(Register::A1),
            12 => Some(Register::A2),
            13 => Some(Register::A3),
            14 => Some(Register::A4),
            15 => Some(Register::A5),
            16 => Some(Register::A6),
            17 => Some(Register::A7),
            18 => Some(Register::S2),
            19 => Some(Register::S3),
            20 => Some(Register::S4),
            21 => Some(Register::S5),
            22 => Some(Register::S6),
            23 => Some(Register::S7),
            24 => Some(Register::S8),
            25 => Some(Register::S9),
            26 => Some(Register::S10),
            27 => Some(Register::S11),
            28 => Some(Register::T3),
            29 => Some(Register::T4),
            30 => Some(Register::T5),
            31 => Some(Register::T6),
            _  => None,
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let register_name = match self {
            Register::Zero => "zero",
            Register::Ra   => "ra",
            Register::Sp   => "sp",
            Register::Gp   => "gp",
            Register::Tp   => "tp",
            Register::T0   => "t0",
            Register::T1   => "t1",
            Register::T2   => "t2",
            Register::S0   => "s0",
            Register::S1   => "s1",
            Register::A0   => "a0",
            Register::A1   => "a1",
            Register::A2   => "a2",
            Register::A3   => "a3",
            Register::A4   => "a4",
            Register::A5   => "a5",
            Register::A6   => "a6",
            Register::A7   => "a7",
            Register::S2   => "s2",
            Register::S3   => "s3",
            Register::S4   => "s4",
            Register::S5   => "s5",
            Register::S6   => "s6",
            Register::S7   => "s7",
            Register::S8   => "s8",
            Register::S9   => "s9",
            Register::S10  => "s10",
            Register::S11  => "s11",
            Register::T3   => "t3",
            Register::T4   => "t4",
            Register::T5   => "t5",
            Register::T6   => "t6",
        };

        write!(f, "{}", register_name)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    Undefined,

    Lui { imm: i64, rd: Register },

    Auipc { imm: i64, rd: Register },
    Jal   { imm: i64, rd: Register },
    Jalr  { imm: i64, rs1: Register, rd: Register },

    Beq  { imm: i64, rs1: Register, rs2: Register },
    Bne  { imm: i64, rs1: Register, rs2: Register },
    Blt  { imm: i64, rs1: Register, rs2: Register },
    Bge  { imm: i64, rs1: Register, rs2: Register },
    Bltu { imm: i64, rs1: Register, rs2: Register },
    Bgeu { imm: i64, rs1: Register, rs2: Register },

    Lb  { imm: i64, rs1: Register, rd: Register },
    Lh  { imm: i64, rs1: Register, rd: Register },
    Lw  { imm: i64, rs1: Register, rd: Register },
    Ld  { imm: i64, rs1: Register, rd: Register },
    Lbu { imm: i64, rs1: Register, rd: Register },
    Lhu { imm: i64, rs1: Register, rd: Register },
    Lwu { imm: i64, rs1: Register, rd: Register },

    Sb { imm: i64, rs1: Register, rs2: Register },
    Sh { imm: i64, rs1: Register, rs2: Register },
    Sw { imm: i64, rs1: Register, rs2: Register },
    Sd { imm: i64, rs1: Register, rs2: Register },

    Addi  { imm: i64, rs1: Register, rd: Register },
    Xori  { imm: i64, rs1: Register, rd: Register },
    Ori   { imm: i64, rs1: Register, rd: Register },
    Andi  { imm: i64, rs1: Register, rd: Register },
    Addiw { imm: i64, rs1: Register, rd: Register },
    Slli  { shamt: u32, rs1: Register, rd: Register },
    Srli  { shamt: u32, rs1: Register, rd: Register },
    Srai  { shamt: u32, rs1: Register, rd: Register },
    Slliw { shamt: u32, rs1: Register, rd: Register },
    Srliw { shamt: u32, rs1: Register, rd: Register },
    Sraiw { shamt: u32, rs1: Register, rd: Register },

    Slti  { imm: i64, rs1: Register, rd: Register },
    Sltiu { imm: i64, rs1: Register, rd: Register },

    Add  { rs1: Register, rs2: Register, rd: Register },
    Sub  { rs1: Register, rs2: Register, rd: Register },
    Sll  { rs1: Register, rs2: Register, rd: Register },
    Slt  { rs1: Register, rs2: Register, rd: Register },
    Sltu { rs1: Register, rs2: Register, rd: Register },
    Xor  { rs1: Register, rs2: Register, rd: Register },
    Srl  { rs1: Register, rs2: Register, rd: Register },
    Sra  { rs1: Register, rs2: Register, rd: Register },
    Or   { rs1: Register, rs2: Register, rd: Register },
    And  { rs1: Register, rs2: Register, rd: Register },
    Addw { rs1: Register, rs2: Register, rd: Register },
    Subw { rs1: Register, rs2: Register, rd: Register },
    Sllw { rs1: Register, rs2: Register, rd: Register },
    Srlw { rs1: Register, rs2: Register, rd: Register },
    Sraw { rs1: Register, rs2: Register, rd: Register },

    Ebreak,
    Ecall,

    Fence { imm: i64, rs1: Register, rd: Register },

    Mul    { rs1: Register, rs2: Register, rd: Register },
    Mulh   { rs1: Register, rs2: Register, rd: Register },
    Mulhsu { rs1: Register, rs2: Register, rd: Register },
    Mulhu  { rs1: Register, rs2: Register, rd: Register },
    Div    { rs1: Register, rs2: Register, rd: Register },
    Divu   { rs1: Register, rs2: Register, rd: Register },
    Rem    { rs1: Register, rs2: Register, rd: Register },
    Remu   { rs1: Register, rs2: Register, rd: Register },
    Mulw   { rs1: Register, rs2: Register, rd: Register },
    Divw   { rs1: Register, rs2: Register, rd: Register },
    Divuw  { rs1: Register, rs2: Register, rd: Register },
    Remw   { rs1: Register, rs2: Register, rd: Register },
    Remuw  { rs1: Register, rs2: Register, rd: Register },
}

fn sign32(value: u32) -> u32 {
    (((value & 0x8000_0000) as i32) >> 31) as u32
}

fn decode_rtype(instr: u32, opcode: u32) -> Instruction {
    let rd  = Register::from_number((instr >>  7) & 0b11111).unwrap();
    let rs1 = Register::from_number((instr >> 15) & 0b11111).unwrap();
    let rs2 = Register::from_number((instr >> 20) & 0b11111).unwrap();

    let funct3 = (instr >> 12) & 0b111;
    let funct7 = (instr >> 25) & 0b111_1111;

    match opcode {
        0b011_0011 if funct7 == 0b000_0001 => match funct3 {
            0b000 => Instruction::Mul    { rs1, rs2, rd },
            0b001 => Instruction::Mulh   { rs1, rs2, rd },
            0b010 => Instruction::Mulhsu { rs1, rs2, rd },
            0b011 => Instruction::Mulhu  { rs1, rs2, rd },
            0b100 => Instruction::Div    { rs1, rs2, rd },
            0b101 => Instruction::Divu   { rs1, rs2, rd },
            0b110 => Instruction::Rem    { rs1, rs2, rd },
            0b111 => Instruction::Remu   { rs1, rs2, rd },
            _     => Instruction::Undefined,
        },
        0b011_0011 => match (funct7, funct3) {
            (0b000_0000, 0b000) => Instruction::Add  { rs1, rs2, rd },
            (0b010_0000, 0b000) => Instruction::Sub  { rs1, rs2, rd },
            (0b000_0000, 0b001) => Instruction::Sll  { rs1, rs2, rd },
            (0b000_0000, 0b010) => Instruction::Slt  { rs1, rs2, rd },
            (0b000_0000, 0b011) => Instruction::Sltu { rs1, rs2, rd },
            (0b000_0000, 0b100) => Instruction::Xor  { rs1, rs2, rd },
            (0b000_0000, 0b101) => Instruction::Srl  { rs1, rs2, rd },
            (0b010_0000, 0b101) => Instruction::Sra  { rs1, rs2, rd },
            (0b000_0000, 0b110) => Instruction::Or   { rs1, rs2, rd },
            (0b000_0000, 0b111) => Instruction::And  { rs1, rs2, rd },
            _                   => Instruction::Undefined,
        },
        0b011_1011 if funct7 == 0b000_0001 => match funct3 {
            0b000 => Instruction::Mulw  { rs1, rs2, rd },
            0b100 => Instruction::Divw  { rs1, rs2, rd },
            0b101 => Instruction::Divuw { rs1, rs2, rd },
            0b110 => Instruction::Remw  { rs1, rs2, rd },
            0b111 => Instruction::Remuw { rs1, rs2, rd },
            _     => Instruction::Undefined,
        },
        0b011_1011 => match (funct7, funct3) {
            (0b000_0000, 0b000) => Instruction::Addw { rs1, rs2, rd },
            (0b010_0000, 0b000) => Instruction::Subw { rs1, rs2, rd },
            (0b000_0000, 0b001) => Instruction::Sllw { rs1, rs2, rd },
            (0b000_0000, 0b101) => Instruction::Srlw { rs1, rs2, rd },
            (0b010_0000, 0b101) => Instruction::Sraw { rs1, rs2, rd },
            _                   => Instruction::Undefined,
        },
        _ => Instruction::Undefined,
    }
}

fn decode_itype(instr: u32, opcode: u32) -> Instruction {
    let imm0_10  = (instr >> 20) & 0b111_1111_1111;
    let imm11_31 = sign32(instr);
    let imm      = (imm0_10 | (imm11_31 << 11)) as i32 as i64;

    let rd_raw  = (instr >>  7) & 0b11111;
    let rs1_raw = (instr >> 15) & 0b11111;

    let rd  = Register::from_number(rd_raw).unwrap();
    let rs1 = Register::from_number(rs1_raw).unwrap();

    let funct3 = (instr >> 12) & 0b111;

    let shamt    = (instr >> 20) & 0b11_1111;
    let shtype   = (instr >> 26) & 0b11_1111;
    let shamt32  = (instr >> 20) & 0b1_1111;
    let shtype32 = (instr >> 25) & 0b11_11111;

    match opcode {
        0b001_0011 => match funct3 {
            0b000 => Instruction::Addi  { imm, rs1, rd },
            0b010 => Instruction::Slti  { imm, rs1, rd },
            0b011 => Instruction::Sltiu { imm, rs1, rd },
            0b100 => Instruction::Xori  { imm, rs1, rd },
            0b110 => Instruction::Ori   { imm, rs1, rd },
            0b111 => Instruction::Andi  { imm, rs1, rd },
            0b001 if shtype == 0b00_0000 => Instruction::Slli { shamt, rs1, rd },
            0b101 if shtype == 0b00_0000 => Instruction::Srli { shamt, rs1, rd },
            0b101 if shtype == 0b01_0000 => Instruction::Srai { shamt, rs1, rd },
            _                            => Instruction::Undefined,
        },
        0b000_0011 => match funct3 {
            0b000 => Instruction::Lb  { imm, rs1, rd },
            0b001 => Instruction::Lh  { imm, rs1, rd },
            0b010 => Instruction::Lw  { imm, rs1, rd },
            0b100 => Instruction::Lbu { imm, rs1, rd },
            0b101 => Instruction::Lhu { imm, rs1, rd },
            0b110 => Instruction::Lwu { imm, rs1, rd },
            0b011 => Instruction::Ld  { imm, rs1, rd },
            _     => Instruction::Undefined,
        },
        0b001_1011 => match funct3 {
            0b000 => Instruction::Addiw { imm, rs1, rd },
            0b001 if shtype32 == 0b000_0000 => Instruction::Slliw { shamt: shamt32, rs1, rd },
            0b101 if shtype32 == 0b000_0000 => Instruction::Srliw { shamt: shamt32, rs1, rd },
            0b101 if shtype32 == 0b010_0000 => Instruction::Sraiw { shamt: shamt32, rs1, rd },
            _                               => Instruction::Undefined,
        },
        0b111_0011 => {
            if funct3 == 0 && rs1_raw == 0 && rd_raw == 0 {
                match imm {
                    0 => Instruction::Ecall,
                    1 => Instruction::Ebreak,
                    _ => Instruction::Undefined,
                }
            } else {
                Instruction::Undefined
            }
        }
        0b000_1111 => {
            if funct3 == 0 {
                Instruction::Fence { imm, rs1, rd }
            } else {
                Instruction::Undefined
            }
        }
        0b110_0111 if funct3 == 0 => Instruction::Jalr { imm, rs1, rd },
        _                         => Instruction::Undefined,
    }
}

fn decode_stype(instr: u32, opcode: u32) -> Instruction {
    let imm0_4   = (instr >>  7) & 0b11111;
    let imm5_10  = (instr >> 25) & 0b11_1111;
    let imm11_31 = sign32(instr);
    let imm      = (imm0_4 | (imm5_10 << 5) | (imm11_31 << 11)) as i32 as i64;

    let rs1 = Register::from_number((instr >> 15) & 0b11111).unwrap();
    let rs2 = Register::from_number((instr >> 20) & 0b11111).unwrap();

    let funct3 = (instr >> 12) & 0b111;

    match opcode {
        0b010_0011 => match funct3 {
            0b000 => Instruction::Sb { imm, rs1, rs2 },
            0b001 => Instruction::Sh { imm, rs1, rs2 },
            0b010 => Instruction::Sw { imm, rs1, rs2 },
            0b011 => Instruction::Sd { imm, rs1, rs2 },
            _     => Instruction::Undefined,
        },
        _ => Instruction::Undefined,
    }
}

fn decode_btype(instr: u32, opcode: u32) -> Instruction {
    let imm1_4   = (instr >>  8) & 0b1111;
    let imm5_10  = (instr >> 25) & 0b11_1111;
    let imm11    = (instr >>  7) & 0b1;
    let imm12_31 = sign32(instr);
    let imm      = ((imm1_4 << 1) | (imm5_10 << 5) | (imm11 << 11) | (imm12_31 << 12))
                        as i32 as i64;

    let rs1 = Register::from_number((instr >> 15) & 0b11111).unwrap();
    let rs2 = Register::from_number((instr >> 20) & 0b11111).unwrap();

    let funct3 = (instr >> 12) & 0b111;

    match opcode {
        0b110_0011 => match funct3 {
            0b000 => Instruction::Beq  { imm, rs1, rs2 },
            0b001 => Instruction::Bne  { imm, rs1, rs2 },
            0b100 => Instruction::Blt  { imm, rs1, rs2 },
            0b101 => Instruction::Bge  { imm, rs1, rs2 },
            0b110 => Instruction::Bltu { imm, rs1, rs2 },
            0b111 => Instruction::Bgeu { imm, rs1, rs2 },
            _     => Instruction::Undefined,
        },
        _ => Instruction::Undefined,
    }
}

fn decode_utype(instr: u32, opcode: u32) -> Instruction {
    let imm = (instr & 0xFFFF_F000) as i32 as i64;
    let rd  = Register::from_number((instr >> 7) & 0b11111).unwrap();

    match opcode {
        0b011_0111 => Instruction::Lui   { imm, rd },
        0b001_0111 => Instruction::Auipc { imm, rd },
        _          => Instruction::Undefined,
    }
}

fn decode_jtype(instr: u32, opcode: u32) -> Instruction {
    let imm1_10  = (instr >> 21) & 0b11_1111_1111;
    let imm11    = (instr >> 20) & 0b1;
    let imm12_19 = (instr >> 12) & 0b1111_1111;
    let imm20_31 = sign32(instr);
    let imm      = ((imm1_10 << 1) | (imm11 << 11) | (imm12_19 << 12) | (imm20_31 << 20))
                        as i32 as i64;

    let rd = Register::from_number((instr >> 7) & 0b11111).unwrap();

    match opcode {
        0b110_1111 => Instruction::Jal { imm, rd },
        _          => Instruction::Undefined,
    }
}

pub fn decode_instruction(instr: u32) -> Instruction {
    let opcode = instr & 0b111_1111;
    if let Some(instruction_type) = &INSTRUCTION_TYPES[opcode as usize] {
        match instruction_type {
            InstructionType::R => decode_rtype(instr, opcode),
            InstructionType::I => decode_itype(instr, opcode),
            InstructionType::S => decode_stype(instr, opcode),
            InstructionType::B => decode_btype(instr, opcode),
            InstructionType::U => decode_utype(instr, opcode),
            InstructionType::J => decode_jtype(instr, opcode),
        }
    } else {
        Instruction::Undefined
    }
}

enum InstructionType {
    R,
    I,
    S,
    B,
    U,
    J,
}

const INSTRUCTION_TYPES: [Option<InstructionType>; 128] = [
    None,                     // _0000000
    None,                     // _0000001
    None,                     // _0000010
    Some(InstructionType::I), // _0000011
    None,                     // _0000100
    None,                     // _0000101
    None,                     // _0000110
    None,                     // _0000111
    None,                     // _0001000
    None,                     // _0001001
    None,                     // _0001010
    None,                     // _0001011
    None,                     // _0001100
    None,                     // _0001101
    None,                     // _0001110
    Some(InstructionType::I), // _0001111
    None,                     // _0010000
    None,                     // _0010001
    None,                     // _0010010
    Some(InstructionType::I), // _0010011
    None,                     // _0010100
    None,                     // _0010101
    None,                     // _0010110
    Some(InstructionType::U), // _0010111
    None,                     // _0011000
    None,                     // _0011001
    None,                     // _0011010
    Some(InstructionType::I), // _0011011
    None,                     // _0011100
    None,                     // _0011101
    None,                     // _0011110
    None,                     // _0011111
    None,                     // _0100000
    None,                     // _0100001
    None,                     // _0100010
    Some(InstructionType::S), // _0100011
    None,                     // _0100100
    None,                     // _0100101
    None,                     // _0100110
    None,                     // _0100111
    None,                     // _0101000
    None,                     // _0101001
    None,                     // _0101010
    None,                     // _0101011
    None,                     // _0101100
    None,                     // _0101101
    None,                     // _0101110
    None,                     // _0101111
    None,                     // _0110000
    None,                     // _0110001
    None,                     // _0110010
    Some(InstructionType::R), // _0110011
    None,                     // _0110100
    None,                     // _0110101
    None,                     // _0110110
    Some(InstructionType::U), // _0110111
    None,                     // _0111000
    None,                     // _0111001
    None,                     // _0111010
    Some(InstructionType::R), // _0111011
    None,                     // _0111100
    None,                     // _0111101
    None,                     // _0111110
    None,                     // _0111111
    None,                     // _1000000
    None,                     // _1000001
    None,                     // _1000010
    None,                     // _1000011
    None,                     // _1000100
    None,                     // _1000101
    None,                     // _1000110
    None,                     // _1000111
    None,                     // _1001000
    None,                     // _1001001
    None,                     // _1001010
    None,                     // _1001011
    None,                     // _1001100
    None,                     // _1001101
    None,                     // _1001110
    None,                     // _1001111
    None,                     // _1010000
    None,                     // _1010001
    None,                     // _1010010
    None,                     // _1010011
    None,                     // _1010100
    None,                     // _1010101
    None,                     // _1010110
    None,                     // _1010111
    None,                     // _1011000
    None,                     // _1011001
    None,                     // _1011010
    None,                     // _1011011
    None,                     // _1011100
    None,                     // _1011101
    None,                     // _1011110
    None,                     // _1011111
    None,                     // _1100000
    None,                     // _1100001
    None,                     // _1100010
    Some(InstructionType::B), // _1100011
    None,                     // _1100100
    None,                     // _1100101
    None,                     // _1100110
    Some(InstructionType::I), // _1100111
    None,                     // _1101000
    None,                     // _1101001
    None,                     // _1101010
    None,                     // _1101011
    None,                     // _1101100
    None,                     // _1101101
    None,                     // _1101110
    Some(InstructionType::J), // _1101111
    None,                     // _1110000
    None,                     // _1110001
    None,                     // _1110010
    Some(InstructionType::I), // _1110011
    None,                     // _1110100
    None,                     // _1110101
    None,                     // _1110110
    None,                     // _1110111
    None,                     // _1111000
    None,                     // _1111001
    None,                     // _1111010
    None,                     // _1111011
    None,                     // _1111100
    None,                     // _1111101
    None,                     // _1111110
    None,                     // _1111111
];
