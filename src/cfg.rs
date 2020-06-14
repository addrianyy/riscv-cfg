use super::rv64::{Instruction, Register};
use std::collections::BTreeMap;

enum Jump {
    Cond {
        on_true:  u64,
        on_false: u64,
    },
    Uncond {
        target:     u64,
        can_return: bool,
    },
    UncondIndirect {
        can_return: bool,
    },
}

impl Jump {
    fn from_instruction(pc: u64, inst: &Instruction) -> Option<Jump> {
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
            }
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Block {
    pub start:      u64,
    pub end:        u64,
    pub entry:      bool,
    pub terminated: bool,
    pub pred:       Vec<u64>,
    pub succ:       Vec<u64>,
}

#[derive(Copy, Clone, Debug)]
pub enum EdgeType {
    CondTrue,
    CondFalse,
    Uncond,
}

#[derive(Clone, Debug)]
pub struct CFG {
    pub blocks: BTreeMap<u64, Block>,
    pub edges:  BTreeMap<(u64, u64), EdgeType>,
}

impl CFG {
    pub fn create_from_function(
        start: u64,
        size:  u64,
        get_instruction: impl Fn(u64) -> Instruction
    ) -> Self {
        let end = start + size;

        let within_function = |address: u64| -> bool {
            address >= start && address < end
        };

        let mut edges  = BTreeMap::new();
        let mut blocks = BTreeMap::new();

        let mut current_block = start;

        macro_rules! make_block {
            ($pc: expr) => {{
                assert!(within_function($pc));
                blocks.entry($pc).or_insert(Block { start: $pc, ..Block::default() })
            }};
        }

        macro_rules! get_block {
            () => { blocks.get_mut(&current_block).unwrap() };
            ($pc: expr) => { blocks.get_mut(&$pc).unwrap() };
        }

        macro_rules! make_edge {
            ($from: expr, $to: expr, $typ: expr) => {
                assert!(edges.insert(($from, $to), $typ).is_none());
            };
        }

        make_block!(start).entry = true;

        for pc in (start..end).step_by(4) {
            let jump = Jump::from_instruction(pc, &get_instruction(pc));

            if let Some(jump) = jump {
                match jump {
                    Jump::Cond { on_true, on_false } => {
                        make_block!(on_true);
                        make_block!(on_false);
                    }
                    Jump::Uncond { target, .. } => {
                        if within_function(target) {
                            make_block!(target);
                        }
                    }
                    _ => (),
                }
            }
        }

        for pc in (start..end).step_by(4) {
            let jump = Jump::from_instruction(pc, &get_instruction(pc));

            match blocks.get(&pc) {
                Some(_) => {
                    if current_block != pc {
                        let current = get_block!();

                        current.end = pc;
                        
                        if !current.terminated {
                            current.succ.push(pc);

                            get_block!(pc).pred.push(current_block);

                            make_edge!(current_block, pc, EdgeType::Uncond);
                        }

                        current_block = pc;
                    }
                },
                None => assert!(!get_block!().terminated),
            }

            if let Some(jump) = jump {
                match jump {
                    Jump::Cond { on_true, on_false } => {
                        let current = get_block!();

                        current.succ.extend_from_slice(&[on_true, on_false]);
                        current.terminated = true;

                        get_block!(on_true).pred.push(current_block);
                        get_block!(on_false).pred.push(current_block);

                        make_edge!(current_block, on_true, EdgeType::CondTrue);
                        make_edge!(current_block, on_false, EdgeType::CondFalse);
                    }
                    Jump::Uncond { target, can_return } => {
                        if within_function(target) {
                            let current = get_block!();

                            current.succ.push(target);
                            current.terminated = true;

                            get_block!(target).pred.push(current_block);

                            make_edge!(current_block, target, EdgeType::Uncond);
                        } else if !can_return {
                            get_block!().terminated = true;
                        }
                    }
                    Jump::UncondIndirect { can_return } => {
                        if !can_return {
                            get_block!().terminated = true;
                        }
                    }
                }
            }
        }

        get_block!().end = end;

        CFG {
            blocks,
            edges,
        }
    }
}
