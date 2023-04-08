use std::*;
use gpu16_sim::gpu;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("gpu16_sim <assembly file>");
        return;
    }

    let asm_file = &args[1];

    let asm_string = match fs::read_to_string(asm_file) {
        Ok(text) => text,
        Err(_) => {
            println!("Failed to open assembly file {}", asm_file);
            return;
        }
    };

    let (instructions, labels) = gpu::parse(&asm_string).unwrap();

    for instruction in &instructions {
        println!("Instruction is {:?}", instruction)
    }

    let state = gpu::simulate(instructions.as_slice(), &labels);
    println!("CpuState is:\n{}", state);
}
