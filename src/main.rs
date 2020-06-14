mod rv64;
mod displayinst;
mod elf_loader;
mod dot;
mod htmlfmt;
mod cfg;
mod elf64;
mod funclist;

use std::convert::TryInto;

use rv64::Instruction;
use displayinst::DisplayInstruction;
use htmlfmt::HTMLFormatter;
use cfg::{CFG, EdgeType};
use funclist::FunctionList;

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

    fn draw_function_cfg(&self, start: u64, size: u64, output_path: &str) {
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
            dotgraph.push_str(&format!("{} [style=filled fillcolor=gray90 ", block.start));
            dotgraph.push_str(r#"margin=0.15 shape=box fontname="Consolas" label=<"#);

            for pc in (block.start..block.end).step_by(4) {
                let display_inst = DisplayInstruction::new(
                    self.get_instruction(pc), pc, &formatter);

                dotgraph.push_str(&format!(r#"0x{:X}&nbsp;&nbsp;{}<br align="left"/>"#,
                    pc, display_inst));
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

    let entry_func = funcs.iter().find(|f| f.start == elf.entrypoint)
        .expect("Failed to find entrypoint in function list.")
        .clone();

    let (base, entrypoint, mapped) = elf_loader::map_elf64(&file);

    let executable = Executable { 
        base,
        mapped,
        funcs,
    };

    executable.draw_function_cfg(entry_func.start, entry_func.size, "cfg.svg");
}
