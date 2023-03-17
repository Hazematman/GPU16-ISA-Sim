use std::*;
use std::iter::*;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug)]
pub enum Token {
    Text(String),
    Number(u16),
    NewLine,
    Comma,
    Period,
    Comment(String),
    LabelMarker(char),
}

fn get_number<T: Iterator<Item = char>>(c: char, iter: &mut Peekable<T>) -> u16 {
    let mut number = c.to_string().parse::<u16>().expect("The caller should have passed a digit.");
    while let Some(Ok(digit)) = iter.peek().map(|c| c.to_string().parse::<u16>()) {
        number = number * 10 + digit;
        iter.next();
    }
    number
}

fn get_text<T: Iterator<Item = char>>(iter: &mut Peekable<T>) -> String {
    let mut text = String::from("");
    while let Some(&c) = iter.peek() {
        if !(c.is_alphanumeric() || c == '_') {
            break;
        }

        text.push(c);
        iter.next();
    }

    text
}

fn get_comment<T: Iterator<Item = char>>(iter: &mut Peekable<T>) -> String {
    let mut comment = String::from("");
    while let Some(&c) = iter.peek() {
        if '\n' == c {
            break;
        }

        comment.push(c);
        iter.next();
    }

    comment
}

fn lex(input: &String) -> Result<Vec<Token>, String> {
    let mut result = Vec::new();
    let mut it = input.chars().peekable();

    while let Some(&c) = it.peek() {
        match c {
            '0'..='9' => {
                it.next();
                let n = get_number(c, &mut it);
                result.push(Token::Number(n));
            }
            '\n' => {
                it.next();
                result.push(Token::NewLine);
            }
            ';' => {
                let comment = get_comment(&mut it);
                result.push(Token::Comment(comment));
            }
            ',' => {
                it.next();
                result.push(Token::Comma);
            }
            '.' => {
                it.next();
                result.push(Token::Period);
            }
            c if c.is_alphabetic() => {
                let text = get_text(&mut it);
                result.push(Token::Text(text));
            }
            c if c.is_whitespace() => {
                it.next();
            }
            _ => {
                return Err(format!("Unexpected character {}", c));
            }
        }
    }

    Ok(result)
}

#[derive(Debug)]
pub enum Instruction {
    /* Three reg ops */
    Add(u8, u8, u8),
    Sub(u8, u8, u8),
    And(u8, u8, u8),
    Xor(u8, u8, u8),
    ShiftLeft(u8, u8, u8),
    ShiftRight(u8, u8, u8),

    /* Two reg ops */
    Not(u8, u8),

    /* Special ops */
    LoadI(u16),
    MoveAcum(bool, u8),
}

fn parse_reg<'a>(it: &mut impl Iterator<Item=&'a Token>) -> Result<u8, String> {
    let c = it.next().unwrap();
    match c {
        Token::Text(text) => {
            match text.as_str() {
                "r0" => Ok(0),
                "r1" => Ok(1),
                "r2" => Ok(2),
                "r3" => Ok(3),
                "r4" => Ok(4),
                "r5" => Ok(5),
                "r6" => Ok(6),
                "r7" => Ok(7),
                _ => Err(format!("Expected register got {:?}", text))
            }
        }
        _ => Err(format!("Expected register got {:?}", c))
    }
}

fn parse_two_reg<'a>(it: &mut impl Iterator<Item=&'a Token>, instr: &str) -> (u8, u8) {
    let r1 = match parse_reg(it) {
        Ok(reg) => reg,
        Err(message) => panic!("Error parsing {} instruction, expected register, got: {:?}", instr, message)
    };

    match it.next().unwrap() {
        Token::Comma => (),
        unknown => panic!("Error parsing {} instruction, expected comma, got: {:?}", instr, unknown)
    };

    let r2 = match parse_reg(it) {
        Ok(reg) => reg,
        Err(message) => panic!("Error parsing {} instruction, expected register, got: {:?}", instr, message)
    };

    (r1, r2)
}

fn parse_three_reg<'a>(it: &mut impl Iterator<Item=&'a Token>, instr: &str) -> (u8, u8, u8) {
    let (r1, r2) = parse_two_reg(it, instr);

    match it.next().unwrap() {
        Token::Comma => (),
        unknown => panic!("Error parsing {} instruction, expected comma, got: {:?}", instr, unknown)
    };

    let r3 = match parse_reg(it) {
        Ok(reg) => reg,
        Err(message) => panic!("Error parsing {} instruction, expected register, got: {:?}", instr, message)
    };

    /* Expect comment or newline at the end */
    match it.next().unwrap() {
        Token::Comment(_) => (),
        Token::NewLine => (),
        unknown => panic!("Error parsing {} instruction, expected end of line, got: {:?}", instr, unknown)
    }

    (r1, r2, r3)
}

fn parse_immediate<'a>(it: &mut impl Iterator<Item=&'a Token>) -> Result<u16, String> {
    let c = it.next().unwrap();
    match c {
        Token::Number(x) => Ok(*x),
        _ => Err(format!("Expected register got {:?}", c))
    }
}

fn parse_load_immediate<'a>(it: &mut impl Iterator<Item=&'a Token>) -> Instruction {
    let imm = match parse_immediate(it) {
        Ok(imm) => imm,
        Err(message) => panic!("Error parsing loadi instruction, expected immediate, got: {:?}", message)
    };

    Instruction::LoadI(imm)
}

fn parse_move_acum<'a>(it: &mut impl Iterator<Item=&'a Token>) -> Instruction {
    match it.next().unwrap() {
        Token::Period => (),
        unknown => panic!("Error parsing move_acum, expected type, got: {:?}", unknown)
    }

    let move_type = match it.next().unwrap() {
        Token::Text(text) => {
            match text.as_str() {
                "high" => true,
                "low" => false,
                unknown => panic!("Error parsing move_acum, expected type, got: {:?}", unknown)
            }
        }
        unknown => panic!("Error parsing move_acum, expected type, got: {:?}", unknown)
    };

    let reg = match parse_reg(it) {
        Ok(reg) => reg,
        Err(message) => panic!("Error parsing move_acum, expected register, got: {:?}", message)
    };

    Instruction::MoveAcum(move_type, reg)
}

fn parse_instructions(tokens: &[Token]) -> Result<Vec<Instruction>, String> {
    let mut instructions = Vec::new();
    let mut it = tokens.iter();

    while let Some(&ref t) = it.next() {
        match t {
            Token::Text(text) => {
                match text.as_str() {
                    "add" => {
                        let (r1, r2, r3) = parse_three_reg(&mut it, "add");
                        instructions.push(Instruction::Add(r1, r2, r3));
                    }
                    "sub" => {
                        let (r1, r2, r3) = parse_three_reg(&mut it, "sub");
                        instructions.push(Instruction::Sub(r1, r2, r3));
                    }
                    "and" => {
                        let (r1, r2, r3) = parse_three_reg(&mut it, "and");
                        instructions.push(Instruction::And(r1, r2, r3));
                    }
                    "xor" => {
                        let (r1, r2, r3) = parse_three_reg(&mut it, "xor");
                        instructions.push(Instruction::Xor(r1, r2, r3));
                    }
                    "shift_left" => {
                        let (r1, r2, r3) = parse_three_reg(&mut it, "shift_left");
                        instructions.push(Instruction::ShiftLeft(r1, r2, r3));
                    }
                    "shift_right" => {
                        let (r1, r2, r3) = parse_three_reg(&mut it, "shift_right");
                        instructions.push(Instruction::ShiftRight(r1, r2, r3));
                    }
                    "not" => {
                        let(r1, r2) = parse_two_reg(&mut it, "not");
                        instructions.push(Instruction::Not(r1, r2));
                    }
                    "loadi" => instructions.push(parse_load_immediate(&mut it)),
                    "move_acum" => instructions.push(parse_move_acum(&mut it)),
                    _ => {
                        return Err(format!("Unknown instruction {:?}", text))
                    }
                }
            }
            /* If token is newline or comment, skip */
            Token::NewLine => (),
            Token::Comment(_) => (),
            _ => {
                return Err(format!("Unhandled token {:?}", t))
            }
        }
    }

    Ok(instructions)
}

fn parse(input: &String) -> Result<Vec<Instruction>, String> {
    let tokens = lex(input).unwrap();

    /*
    for t in &tokens {
        println!("Token is {:?}", t);
    }
    */

    let instructions = parse_instructions(tokens.as_slice()).unwrap();

    Ok(instructions)
}

#[derive(Debug)]
struct CpuState {
    registers: [u16; 8],
    accumlator: u32,
    program_counter: u32,
}

fn simulate(instructions: &[Instruction], registers: &[u16; 8]) {
    let mut state = CpuState { registers: registers.clone(), accumlator: 0, program_counter: 0 };
    let mut running = true;
    while running {
        match instructions[state.program_counter as usize] {
            Instruction::Add(r1, r2, r3) => state.registers[r1 as usize] = state.registers[r2 as usize] + state.registers[r3 as usize],
            Instruction::Sub(r1, r2, r3) => state.registers[r1 as usize] = state.registers[r2 as usize] - state.registers[r3 as usize],
            Instruction::And(r1, r2, r3) => state.registers[r1 as usize] = state.registers[r2 as usize] & state.registers[r3 as usize],
            Instruction::Xor(r1, r2, r3) => state.registers[r1 as usize] = state.registers[r2 as usize] ^ state.registers[r3 as usize],
            Instruction::ShiftLeft(r1, r2, r3) => state.registers[r1 as usize] = state.registers[r2 as usize] << state.registers[r3 as usize],
            Instruction::ShiftRight(r1, r2, r3) => state.registers[r1 as usize] = state.registers[r2 as usize] >> state.registers[r3 as usize],
            Instruction::Not(r1, r2) => state.registers[r1 as usize] = !state.registers[r2 as usize],
            Instruction::LoadI(imm) => state.accumlator = imm as u32,
            Instruction::MoveAcum(move_type, reg) => {
                let mut value = state.accumlator;
                if move_type {
                    value >>= 16;
                }

                state.registers[reg as usize] = value as u16;
            }
        }

        state.program_counter += 1;
        if state.program_counter as usize >= instructions.len() {
            running = false;
        }
    }

    println!("CPU state is ${:?}", state);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("gpu16_sim <assembly file> <register file>");
        return;
    }

    let asm_file = &args[1];

    let registers: [u16; 8] = if args.len() > 2 {
        let reg_file = &args[2];
        let reg_f = File::open(reg_file).expect("File not found");
        let reader = BufReader::new(reg_f);
        reader.lines()
            .map(|line| line.unwrap().parse::<u16>().unwrap())
            .collect::<Vec<u16>>()
            .try_into()
            .unwrap()
    } else {
        [0; 8]
    };

    let asm_string = match fs::read_to_string(asm_file) {
        Ok(text) => text,
        Err(_) => {
            println!("Failed to open assembly file {}", asm_file);
            return;
        }
    };

    let instructions = parse(&asm_string).unwrap();

    for instruction in &instructions {
        println!("Instruction is {:?}", instruction)
    }

    simulate(instructions.as_slice(), &registers)
}
