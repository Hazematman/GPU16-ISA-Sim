pub mod gpu;

#[cfg(test)]
mod tests {
    use std::*;
    use crate::gpu;

    fn high_low(a: f32) -> (u16, u16) {
        let high = a as u16;
        let low = ((a * 65536.0) % 65536.0) as u16;
        (high, low)
    }

    #[test]
    /* This test checks if matrix multiplication works.
     * it will only look at results from the first CPU core
     */
    fn matrix_multiply() {
        let asm_string = match fs::read_to_string("tests/matrix.S") {
            Ok(text) => text,
            Err(_) => {
                println!("Failed to open assembly file");
                return;
            }
        };

        let (instructions, labels) = gpu::parse(&asm_string).unwrap();

        let mut memory = [0u16; 64*1024];

        let x = 1.0f32;
        let mat = [1.0f32, 0.0f32, 0.0f32, 0.0f32,
                   0.0f32, 1.0f32, 0.0f32, 0.0f32,
                   0.0f32, 0.0f32, 1.0f32, 0.0f32];

        let start_addr = 64;
        let (x_high, x_low) = high_low(x);
        memory[start_addr] = x_high;
        memory[start_addr + 1] = x_low;
        println!("MEMORY IS {}", memory[start_addr]);
        for i in 0..12 {
            let (high, low) = high_low(mat[i]);
            memory[(i)*2 + start_addr + 6] = high;
            memory[(i+start_addr+6)*2 + 1 + start_addr + 6] = low;
        }

        assert_eq!(x_high, 1, "Validate x high is correct");
        assert_eq!(x_low, 0, "Validate x low is correct");

        let x_prime = mat[0] * x + mat[1] * x + mat[2] * x + mat[3];

        let in_state = gpu::CpuState {
            cores: [
                gpu::CpuCore {
                    thread_id: 0,
                    registers: [0u16, start_addr as u16, (start_addr+6) as u16, 0u16, 0u16, 0u16, 0u16, 0u16],
                    ..Default::default()
                },
                gpu::CpuCore {thread_id: 1, exec_mask: true, ..Default::default() },
                gpu::CpuCore {thread_id: 2, exec_mask: true, ..Default::default() },
                gpu::CpuCore {thread_id: 3, exec_mask: true, ..Default::default() }
            ],
            program_counter: 0,
            stride: 0,
            memory: memory,
        };

        let state = gpu::simulate(in_state, instructions.as_slice(), &labels);
        println!("CpuState is:\n{}", state);

        let (x_prime_high, x_prime_low) = high_low(x_prime);
        assert_eq!(x_prime_high, state.memory[start_addr]);
        assert_eq!(x_prime_low, state.memory[start_addr+1]);

        // Todo test the rest of the components
    }
}
