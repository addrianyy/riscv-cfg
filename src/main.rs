mod rv64;
mod displayinst;
mod elf_loader;

use std::collections::BTreeMap;
use std::convert::TryInto;

use rv64::{Instruction, Register};
use displayinst::{DisplayInstruction, InstructionFormatter};

fn save_dot_svg_graph(graph_desc: &str, output_path: &str) {
    use std::process::{Command, Stdio};
    use std::io::Write;

    let mut process = Command::new("dot")
        .args(&["-Tsvg", "-o", output_path])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to run `dot`.");

    {
        let stdin = process.stdin.as_mut()
            .expect("Getting `dot` stdin failed.");

        stdin.write_all(graph_desc.as_bytes())
            .expect("Writing to `dot` stdin failed.");
    }

    let output = process.wait_with_output()
        .expect("Waiting for `dot` failed.");

    let no_output = output.stderr.is_empty() && output.stdout.is_empty();
    if !no_output {
        println!("{}", String::from_utf8_lossy(&output.stdout));
        println!("{}", String::from_utf8_lossy(&output.stderr));
    }

    assert!(output.status.success() && no_output,
        "`dot` failed to generate graph.");
}

#[derive(Debug)]
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

struct HTMLFormatter {
    reg_color:    String,
    mnem_color:   String,
    imm_color:    String,
    addr_color:   String,
    mnem_padding: usize,
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

#[derive(Debug, Default)]
struct Block {
    start:      u64,
    end:        u64,
    entry:      bool,
    terminated: bool,
    pred:       Vec<u64>,
    succ:       Vec<u64>,
}

enum EdgeType {
    CondTrue,
    CondFalse,
    Uncond,
}

struct CFG {
    blocks: BTreeMap<u64, Block>,
    edges:  BTreeMap<(u64, u64), EdgeType>,
}

struct Executable {
    base:   u64,
    mapped: Vec<u8>,
}

impl Executable {
    fn get_instruction(&self, address: u64) -> Instruction {
        let offset = address.checked_sub(self.base).unwrap() as usize;
        let instruction = u32::from_le_bytes(self.mapped[offset..offset + 4]
            .try_into().unwrap());

        rv64::decode_instruction(instruction)
    }

    fn get_function_cfg(&self, start: u64, size: u64) -> CFG {
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
            let jump = Jump::from_instruction(pc, &self.get_instruction(pc));

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
            let jump = Jump::from_instruction(pc, &self.get_instruction(pc));

            match blocks.get(&pc) {
                Some(_) => {
                    get_block!().end = pc;
                    current_block = pc;
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

    fn draw_function_cfg(&self, start: u64, size: u64, output_path: &str) {
        let cfg = self.get_function_cfg(start, size);


        let formatter = HTMLFormatter {
            mnem_padding: 6,
            mnem_color:   String::from("red"),
            reg_color:    String::from("blue"),
            addr_color:   String::from("orange"),
            imm_color:    String::from("green"),
        };

        let mut dotgraph = String::new();

        dotgraph.push_str("digraph CFG {\n");

        for ((from, to), edge_type) in cfg.edges.iter() {
            let color = match edge_type {
                EdgeType::CondTrue  => "green",
                EdgeType::CondFalse => "red",
                EdgeType::Uncond    => "blue",
            };

            dotgraph.push_str(&format!("{} -> {} [color={}];\n", from, to, color));
        }

        for (_, block) in cfg.blocks.iter() {
            dotgraph.push_str(&format!(
                r#"{} [style=filled fillcolor=gray90 margin=0.15 shape=box fontname="Consolas" label=<"#, block.start));

            for pc in (block.start..block.end).step_by(4) {
                let display_inst = DisplayInstruction::new(
                    self.get_instruction(pc), pc, &formatter);

                dotgraph.push_str(&format!(r#"0x{:X}&nbsp;&nbsp;{}<br align="left"/>"#,
                    pc, display_inst));
            }

            dotgraph.push_str(">];\n");
        }

        dotgraph.push_str("}\n");

        save_dot_svg_graph(&dotgraph, output_path);
    }
}

fn main() {
    let file = std::fs::read("F://rv64/main").unwrap();
    let (base, entrypoint, mapped) = elf_loader::map_elf64(&file);

    let executable = Executable { base, mapped };

    executable.draw_function_cfg(entrypoint, 0xB4, "cfg.svg");
}
