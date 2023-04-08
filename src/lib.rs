pub mod gpu;

#[cfg(test)]
mod tests {
    use std::*;
    use crate::gpu;

    #[test]
    fn matrix_multiply() {
        let asm_string = match fs::read_to_string("tests/matrix.S") {
            Ok(text) => text,
            Err(_) => {
                println!("Failed to open assembly file");
                return;
            }
        };

        let (instructions, labels) = gpu::parse(&asm_string).unwrap();

        let state = gpu::simulate(instructions.as_slice(), &labels);
        println!("CpuState is:\n{}", state);
    }
}
