mod decoder;
mod elf_loader;

use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryInto;

use decoder::{Instruction, Register};

#[derive(Debug)]
enum Jump {
    Cond {
        on_true:  u64,
        on_false: u64
    },
    Uncond {
        target:     u64,
        can_return: bool,
    },
    UncondIndirect {
        can_return: bool,
    },
}

fn parse_jump(pc: u64, inst: &Instruction) -> Option<Jump> {
    match inst {
        Instruction::Beq  { imm, .. } |
        Instruction::Bne  { imm, .. } |
        Instruction::Blt  { imm, .. } |
        Instruction::Bge  { imm, .. } |
        Instruction::Bltu { imm, .. } |
        Instruction::Bgeu { imm, .. } => {
            let on_true  = pc.wrapping_add(*imm as u64);
            let on_false = pc.wrapping_add(4);

            Some(Jump::Cond { on_true, on_false })
        }
        Instruction::Jal { imm, rd } => {
            let target     = pc.wrapping_add(*imm as u64);
            let can_return = *rd != Register::Zero;

            Some(Jump::Uncond { target, can_return })
        }
        Instruction::Jalr { rd, .. } => {
            let can_return = *rd != Register::Zero;

            Some(Jump::UncondIndirect { can_return })
        },
        _ => None,
    }
}

struct Executable {
    base: u64,
    mapped: Vec<u8>,
}

impl Executable {
    fn get_instruction(&self, address: u64) -> Instruction {
        let offset = address.checked_sub(self.base).unwrap() as usize;
        let instruction = u32::from_le_bytes(self.mapped[offset..offset + 4]
            .try_into().unwrap());

        decoder::decode_instruction(instruction)
    }

    fn disasm_function(&self, start: u64, size: u64) {
        let end = start + size;

        let within_function = |address: u64| -> bool {
            address >= start && address < end
        };

        #[derive(Debug, Default)]
        struct Label {
            start:      u64,
            end:        u64,
            entry:      bool,
            terminated: bool,
            pred:       Vec<u64>,
            succ:       Vec<u64>,
        }

        #[derive(Debug)]
        enum CondJumpType {
            OnTrue,
            OnFalse,
        }

        let mut cond_jumps: BTreeMap<(u64, u64), CondJumpType> = BTreeMap::new();

        let mut labels: BTreeMap<u64, Label> = BTreeMap::new();

        let mut current_label = start;

        macro_rules! get_current_label {
            () => { labels.get_mut(&current_label).unwrap() }
        }

        macro_rules! get_label {
            ($pc: expr) => { labels.entry($pc).or_insert(Label::default()) }
        }

        get_label!(start).entry = true;

        for pc in (start..end).step_by(4) {
            let jump = parse_jump(pc, &self.get_instruction(pc));

            if let Some(jump) = jump {
                match jump {
                    Jump::Cond { on_true, on_false } => {
                        assert!(within_function(on_true));
                        get_label!(on_true);

                        assert!(within_function(on_false));
                        get_label!(on_false);
                    }
                    Jump::Uncond { target, .. } => {
                        if within_function(target) {
                            get_label!(target);
                        }
                    }
                    _ => (),
                }
            }
        }

        for pc in (start..end).step_by(4) {
            let jump = parse_jump(pc, &self.get_instruction(pc));

            match labels.get(&pc) {
                Some(_) => {
                    get_current_label!().end = pc;
                    current_label = pc;
                },
                None => assert!(!get_current_label!().terminated),
            }

            if let Some(jump) = jump {
                match jump {
                    Jump::Cond { on_true, on_false } => {
                        let current = get_current_label!();

                        current.succ.extend_from_slice(&[on_true, on_false]);
                        current.terminated = true;

                        get_label!(on_true).pred.push(current_label);
                        get_label!(on_false).pred.push(current_label);

                        assert!(cond_jumps.insert((current_label, on_true),
                            CondJumpType::OnTrue).is_none());

                        assert!(cond_jumps.insert((current_label, on_false),
                            CondJumpType::OnFalse).is_none());
                    }
                    Jump::UncondIndirect { can_return } => {
                        if !can_return {
                            get_current_label!().terminated = true;
                        }
                    }
                    Jump::Uncond { target, can_return } => {
                        if within_function(target) {
                            let current = get_current_label!();

                            current.succ.push(target);
                            current.terminated = true;

                            get_label!(target).pred.push(current_label);
                        } else if !can_return {
                            get_current_label!().terminated = true;
                        }
                    }
                }
            }
        }

        println!("{:X?}", cond_jumps);

        get_current_label!().end = start + size;

        for (start, label) in labels.iter_mut() {
            assert!(label.terminated);

            label.start = *start;
        }

        let mut connections = BTreeSet::new();

        for (start, label) in labels.iter() {
            for pred in label.pred.iter() {
                connections.insert((pred, start));
            }

            for succ in label.succ.iter() {
                connections.insert((start, succ));
            }
        }

        println!("{:X?}", connections);


        println!("digraph G {{");

        for (a, b) in connections.iter() {
            print!("\"{:X}\" -> \"{:X}\"", a, b);

            if let Some(cj_type) = cond_jumps.get(&(**a, **b)) {
                let color = match cj_type {
                    CondJumpType::OnTrue  => "green",
                    CondJumpType::OnFalse => "red",
                };

                print!("[color={}]", color);
            }

            println!();
        }

        for (start, label) in labels.iter() {
            print!("\"{:X}\" [margin=0.2 shape=box fontname=\"Consolas\" label=\"", start);

            for pc in ((*start)..(label.end)).step_by(4) {
                let instruction = decoder::AddressedInstruction {
                    inst: self.get_instruction(pc),
                    pc,
                };

                print!("0x{:X}  {}\\l", pc, instruction);
            }

            println!("\"];");
        }

        println!("}}");
    }
}

fn main() {
    let mut file = std::fs::read("F://rv64/main").unwrap();
    let (base, entrypoint, mapped) = elf_loader::map_elf64(&mut file);

    let executable = Executable {
        base, mapped
    };

    executable.disasm_function(entrypoint, 0xB4);
}
