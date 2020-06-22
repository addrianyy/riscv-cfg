mod rv64;
mod displayinst;
mod dot;
mod htmlfmt;
mod cfg;
mod elf64;
mod funclist;

use std::convert::TryInto;

use rv64::{Instruction, Register};
use displayinst::DisplayInstruction;
use htmlfmt::HTMLFormatter;
use cfg::{CFG, EdgeType};
use funclist::FunctionList;
use displayinst::InstructionFormatter;

struct Executable {
    base:   u64,
    mapped: Vec<u8>,
    funcs:  FunctionList,
}

impl Executable {
    fn get_instruction(&self, address: u64) -> Instruction {
        let offset = address.checked_sub(self.base).unwrap() as usize;
        let instruction = u32::from_le_bytes(self.mapped[offset..offset + 4]
            .try_into().unwrap());

        rv64::decode_instruction(instruction)
    }

    fn draw_function_cfg(&self, func_name: &str, start: u64, size: u64, output_path: &str) {
        let cfg = CFG::create_from_function(start, size, |address| self.get_instruction(address));

        let formatter = HTMLFormatter {
            mnem_padding: 6,
            mnem_color:   String::from("#0d47a1"),
            reg_color:    String::from("#00796b"),
            addr_color:   String::from("#ff8a80"),
            imm_color:    String::from("#e91e63"),
            funcs:        &self.funcs,
        };

        let mut dotgraph = String::new();

        dotgraph.push_str(&format!("digraph CFG{} {{\n", func_name));

        for ((from, to), edge_type) in cfg.edges.iter() {
            let color = match edge_type {
                EdgeType::CondTrue  => "green",
                EdgeType::CondFalse => "red",
                EdgeType::Uncond    => "blue",
            };

            dotgraph.push_str(&format!("{} -> {} [color={}];\n", from, to, color));
        }

        for (_, block) in cfg.blocks.iter() {
            dotgraph.push_str(&format!("{} [style=filled fillcolor=gray90 ", block.start));
            dotgraph.push_str(r#"margin=0.15 shape=box fontname="Consolas" label=<"#);

            if block.entry {
                dotgraph.push_str(&format!("<b>{}</b><br />", func_name));
                dotgraph.push_str(r#"<br align="left"/>"#);
            }
            
            let mut previous = None;

            for pc in (block.start..block.end).step_by(4) {
                let inst = self.get_instruction(pc);

                let mut call_target = None;
                let mut call_rd     = Register::Zero;

                if let Instruction::Jalr { imm, rs1, rd } = inst {
                    call_rd = rd;

                    if let Some(Instruction::Auipc { imm: aimm, rd: ard }) = previous {
                        if rs1 == ard {
                            call_target = Some((pc - 4).wrapping_add(imm as u64)
                                .wrapping_add(aimm as u64));
                        }
                    }
                }

                dotgraph.push_str(&format!(r"0x{:X}&nbsp;&nbsp;", pc));

                match call_target {
                    Some(call_target) => {
                        if call_rd == Register::Ra {
                            dotgraph.push_str(&format!("{} {}", formatter.fmt_mnem("call"),
                                formatter.fmt_addr(call_target)));
                        } else {
                            dotgraph.push_str(&format!("{} {}, {}", formatter.fmt_mnem("callu"),
                                formatter.fmt_reg(call_rd), formatter.fmt_addr(call_target)));
                        }
                    }
                    None => {
                        let display_inst = DisplayInstruction::new(inst, pc, &formatter);
                        dotgraph.push_str(&format!(r"{}",display_inst));
                    }
                }

                dotgraph.push_str(r#"<br align="left"/>"#);

                previous = Some(inst);
            }

            dotgraph.push_str(">];\n");
        }

        dotgraph.push_str("}\n");

        dot::save_svg_graph(&dotgraph, output_path);
    }
}

fn main() {
    let file = std::fs::read("F://rv64/main")
        .expect("Failed to open ELF executable.");

    let elf = elf64::Elf::parse(&file);
    let funcs = funclist::load_from_debug_info(&file, &elf)
        .expect("Failed to load functions from debug info.");

    let mapped = elf.map(&file);

    let executable = Executable { 
        base: elf.base_address,
        mapped,
        funcs,
    };

    for func in &executable.funcs {
        let name = &func.name;
        
        executable.draw_function_cfg(name, func.start, func.size,
            &format!("cfg/{}.svg", &func.name));
    }
}
