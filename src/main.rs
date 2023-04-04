use std::*;
use std::collections::HashMap;
use std::iter::*;

#[derive(Debug)]
pub enum Token {
    Text(String),
    Number(u16),
    NewLine,
    Comma,
    Period,
    Colon,
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
            ':' => {
                it.next();
                result.push(Token::Colon);
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
pub enum RegSource {
    Reg,
    AcumHigh,
    AcumLow,
}

#[derive(Debug)]
pub enum MultSet {
    Set,
    Accum
}

#[derive(Debug)]
pub enum MultSign {
    UnsignedUnsigned,
    SignedUnsigned,
    UnsignedSigned,
    SignedSigned
}

#[derive(Debug)]
pub enum MultShift {
    No,
    Up,
    Down
}

#[derive(Debug)]
pub enum Instruction {
    /* Three reg ops */
    Add(u8, u8, u8, RegSource),
    Sub(u8, u8, u8, RegSource),
    And(u8, u8, u8, RegSource),
    Xor(u8, u8, u8, RegSource),
    ShiftLeft(u8, u8, u8, RegSource),
    ShiftRight(u8, u8, u8, RegSource),

    /* Two reg ops */
    Not(u8, u8),

    /* Mult op */
    Mult(u8, u8, MultSet, MultSign, MultShift),

    /* Stride ops */
    Stride(u16),

    /* Load store ops */
    Load(u8, u8, u16),
    Store(u8, u8, u16),

    /* Branching ops */
    Jump(u8),
    Branch(bool, bool, bool, String),

    /* Special ops */
    LoadI(u16),
    LoadAddr(String), // This is another form of LoadI, just a psuedo op
}

fn parse_reg_val(reg: &str) -> Result<u8, String> {
    match reg {
        "r0" => Ok(0),
        "r1" => Ok(1),
        "r2" => Ok(2),
        "r3" => Ok(3),
        "r4" => Ok(4),
        "r5" => Ok(5),
        "r6" => Ok(6),
        "r7" => Ok(7),
        _ => Err(format!("Expected register got {:?}", reg))
    }
}

fn parse_reg<'a>(it: &mut impl Iterator<Item=&'a Token>) -> Result<u8, String> {
    let c = it.next().unwrap();
    match c {
        Token::Text(text) => {
            parse_reg_val(text.as_str())
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

fn parse_three_reg_all<'a>(it: &mut impl Iterator<Item=&'a Token>, instr: &str) -> (u8, u8, u8, RegSource) {
    match it.next().unwrap() {
        Token::Period => {
            /* TODO This error checking feels really repetitive, is there a better way to do it? */
            match it.next().unwrap() {
                Token::Text(text) => {
                    match text.as_str() {
                        "accum" => (),
                        unknown => panic!("Error parsing {} instruction, expected source type, got: {:?}", instr, unknown)
                    }
                }
                unknown => panic!("Error parsing {} instruction, expected source type, got: {:?}", instr, unknown)
            }

            match it.next().unwrap() {
                Token::Period => (),
                unknown => panic!("Error parsing {} instruction, expected source type, got: {:?}", instr, unknown)
            }

            let source = match it.next().unwrap() {
                Token::Text(text) => {
                    match text.as_str() {
                        "low" => RegSource::AcumLow,
                        "high" => RegSource::AcumHigh,
                        unknown => panic!("Error parsing {} instruction, expected source type, got: {:?}", instr, unknown)
                    }
                }
                unknown => panic!("Error parsing {} instruction, expected source type, got: {:?}", instr, unknown)
            };

            let (rd, rs1) = parse_two_reg(it, instr);
            (rd, rs1, 0, source)
        }
        Token::Text(text) => {
            let r1 = parse_reg_val(text.as_str())
                .expect(format!("Error parsing {} instruction, expected register got: {}", instr, text).as_str());
            match it.next().unwrap() {
                Token::Comma => (),
                unknown => panic!("Error parsing {} instruction, expected comma, got: {:?}", instr, unknown)
            };
            let (r2, r3) = parse_two_reg(it, instr);

            (r1, r2, r3, RegSource::Reg)
        }
        unknown => panic!("Error parsing {} instruction, got: {:?}", instr, unknown)
    }
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

fn parse_load_addr<'a>(it: &mut impl Iterator<Item=&'a Token>) -> Instruction {
    let c = it.next().unwrap();
    let label = match c {
        Token::Text(s) => s.clone(),
        _ => panic!("Failed to parse label in load_addr")
    };

    Instruction::LoadAddr(label)
}

fn parse_two_reg_imm<'a>(it: &mut impl Iterator<Item=&'a Token>, instr: &str) -> (u8, u8, u16) {
    let (rd, rs) = parse_two_reg(it, instr);

    match it.next().unwrap() {
        Token::Comma => (),
        unknown => panic!("Error parsing {} instruction, expected comma, got: {:?}", instr, unknown)
    };

    let imm = parse_immediate(it).expect("Expected immediate value");

    (rd, rs, imm)
}

fn parse_mult<'a>(it: &mut impl Iterator<Item=&'a Token>) -> (u8, u8, MultSet, MultSign, MultShift) {
    let instr = "mult";

    match it.next().unwrap() {
        Token::Period => (),
        unknown => panic!("Error parsing {} instruction, expected source type, got: {:?}", instr, unknown)
    }

    let mult_set = match it.next().unwrap() {
        Token::Text(text) => {
            match text.as_str() {
                "set" => MultSet::Set,
                "accum" => MultSet::Accum,
                unknown => panic!("Error parsing {} instruction, expected mult set, got: {:?}", instr, unknown)
            }
        }
        unknown => panic!("Error parsing {} instruction, expected mult set, got: {:?}", instr, unknown)
    };

    match it.next().unwrap() {
        Token::Period => (),
        unknown => panic!("Error parsing {} instruction, expected source type, got: {:?}", instr, unknown)
    }

    let mult_sign = match it.next().unwrap() {
        Token::Text(text) => {
            match text.as_str() {
                "u16_u16" => MultSign::UnsignedUnsigned,
                "u16_s16" => MultSign::UnsignedSigned,
                "s16_u16" => MultSign::SignedUnsigned,
                "s16_s16" => MultSign::SignedSigned,
                unknown => panic!("Error parsing {} instruction, expected mult sign, got: {:?}", instr, unknown)
            }
        }
        unknown => panic!("Error parsing {} instruction, expected mult sign, got: {:?}", instr, unknown)
    };

    match it.next().unwrap() {
        Token::Period => (),
        unknown => panic!("Error parsing {} instruction, expected source type, got: {:?}", instr, unknown)
    }

    let mult_shift = match it.next().unwrap() {
        Token::Text(text) => {
            match text.as_str() {
                "up" => MultShift::Up,
                "down" => MultShift::Down,
                "no" => MultShift::No,
                unknown => panic!("Error parsing {} instruction, expected mult shift, got: {:?}", instr, unknown)
            }
        }
        unknown => panic!("Error parsing {} instruction, expected mult shift, got: {:?}", instr, unknown)
    };

    let (rs1, rs2) = parse_two_reg(it, instr);

    (rs1, rs2, mult_set, mult_sign, mult_shift)
}

#[derive(Debug)]
pub enum FlagType {
    Zero,
    Positive,
    Negative,
}

#[derive(Debug)]
pub enum BranchType {
    Flag(FlagType),
    Imm(String),
}

fn parse_branch_next_field<'a>(it: &mut impl Iterator<Item=&'a Token>) -> BranchType {
    match it.next().unwrap() {
        Token::Period => {
            let branch_type = match it.next().unwrap() {
                Token::Text(text) => text.as_str(),
                x => panic!("Error parsing branch instruction, expected branch type, got: {:?}", x)
            };

            match branch_type {
                "zero" => BranchType::Flag(FlagType::Zero),
                "negative" => BranchType::Flag(FlagType::Negative),
                "positive" => BranchType::Flag(FlagType::Positive),
                x => panic!("Error parsing branch instruction, expected branch type, got: {:?}", x)
            }
        }
        Token::Text(text) => {
            BranchType::Imm(text.clone())
        }
        x => panic!("Error parsing branch instruction, expected branch type, got: {:?}", x)
    }
}

fn parse_branch<'a>(it: &mut impl Iterator<Item=&'a Token>) -> Instruction {
    let mut zero = false;
    let mut positive = false;
    let mut negative = false;
    let mut imm = "".to_string();
    for _i in 0..3 {
        match parse_branch_next_field(it) {
            BranchType::Imm(num) => {
                imm = num;
                break;
            }
            BranchType::Flag(flag) => {
                match flag {
                    FlagType::Zero => zero = true,
                    FlagType::Positive => positive = true,
                    FlagType::Negative => negative = true,
                }
            }
        }
    }

    Instruction::Branch(negative, zero, positive, imm)
}

fn parse_instructions(tokens: &[Token]) -> Result<(Vec<Instruction>, HashMap<String, usize>), String> {
    let mut instructions = Vec::new();
    let mut it = tokens.iter().peekable();
    let mut labels: HashMap<String, usize> = HashMap::new();

    while let Some(&ref t) = it.next() {
        match t {
            Token::Text(text) => {
                match text.as_str() {
                    "add" => {
                        let (r1, r2, r3, source) = parse_three_reg_all(&mut it, "add");
                        instructions.push(Instruction::Add(r1, r2, r3, source));
                    }
                    "sub" => {
                        let (r1, r2, r3, source) = parse_three_reg_all(&mut it, "sub");
                        instructions.push(Instruction::Sub(r1, r2, r3, source));
                    }
                    "and" => {
                        let (r1, r2, r3, source) = parse_three_reg_all(&mut it, "and");
                        instructions.push(Instruction::And(r1, r2, r3, source));
                    }
                    "xor" => {
                        let (r1, r2, r3, source) = parse_three_reg_all(&mut it, "xor");
                        instructions.push(Instruction::Xor(r1, r2, r3, source));
                    }
                    "shift_left" => {
                        let (r1, r2, r3, source) = parse_three_reg_all(&mut it, "shift_left");
                        instructions.push(Instruction::ShiftLeft(r1, r2, r3, source));
                    }
                    "shift_right" => {
                        let (r1, r2, r3, source) = parse_three_reg_all(&mut it, "shift_right");
                        instructions.push(Instruction::ShiftRight(r1, r2, r3, source));
                    }
                    "not" => {
                        let(r1, r2) = parse_two_reg(&mut it, "not");
                        instructions.push(Instruction::Not(r1, r2));
                    }
                    "mult" => {
                        let(rs1, rs2, set, sign, shift) = parse_mult(&mut it);
                        instructions.push(Instruction::Mult(rs1, rs2, set, sign, shift));
                    }
                    "stride" => {
                        let imm = parse_immediate(&mut it).expect("Failed to parse immediate value");
                        instructions.push(Instruction::Stride(imm));
                    }
                    "load" => {
                        let(rd, rs, imm) = parse_two_reg_imm(&mut it, "load");
                        instructions.push(Instruction::Load(rd, rs, imm));
                    }
                    "store" => {
                        let(rd, rs, imm) = parse_two_reg_imm(&mut it, "store");
                        instructions.push(Instruction::Store(rd, rs, imm));
                    }
                    "jump" => {
                        let rs = parse_reg(&mut it).unwrap();
                        instructions.push(Instruction::Jump(rs));
                    }
                    "branch" => {
                        instructions.push(parse_branch(&mut it));
                    }
                    "loadi" => instructions.push(parse_load_immediate(&mut it)),
                    "load_addr" => instructions.push(parse_load_addr(&mut it)),
                    _ => {
                        match it.peek().unwrap() {
                            Token::Colon => {
                                it.next();
                                labels.insert(
                                    text.clone(),
                                    instructions.len()-1
                                );
                            }
                            _ => return Err(format!("Unknown instruction {:?}", text))
                        }
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

    Ok((instructions, labels))
}

fn parse(input: &String) -> Result<(Vec<Instruction>, HashMap<String, usize>), String> {
    let tokens = lex(input).unwrap();

    /*
    for t in &tokens {
        println!("Token is {:?}", t);
    }
    */

    let (instructions, labels) = parse_instructions(tokens.as_slice()).unwrap();

    Ok((instructions, labels))
}

#[derive(Debug)]
struct CpuCore {
    thread_id: u8,
    registers: [u16; 8],
    negative: bool,
    zero: bool,
    positive: bool,
    exec_mask: bool,
    accumulator: u32,
}

#[derive(Debug)]
struct CpuState {
    cores: [CpuCore; 4],
    program_counter: u16,
    stride: u16,
    memory: [u16; 64*1024],
}

impl fmt::Display for CpuState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for core in &self.cores {
            write!(f, "{:?}\n", core).unwrap();
        }
        write!(f, "stride {}\nprogram counter {}", self.stride, self.program_counter)
    }
}

impl Default for CpuCore {
    fn default() -> CpuCore {
        CpuCore {
            thread_id: 0u8,
            registers: [0u16; 8],
            negative: false,
            zero: false,
            positive: false,
            exec_mask: false,
            accumulator: 0u32
        }
    }
}

fn get_source2_value(state: &CpuCore, rs2: u8, source: &RegSource) -> u16 {
    match source {
        RegSource::Reg => state.registers[rs2 as usize],
        RegSource::AcumLow => state.accumulator as u16,
        RegSource::AcumHigh => (state.accumulator >> 16) as u16,
    }
}

fn set_flags(state: &mut CpuCore, result: u16) {
    state.zero = result == 0;
    state.positive = (result as i16) > 0;
    state.negative = (result as i16) < 0;
}

fn simulate(instructions: &[Instruction], labels: &HashMap<String, usize>) {
    let mut state = CpuState {
        cores: [
            CpuCore {thread_id: 0, ..Default::default() },
            CpuCore {thread_id: 1, ..Default::default() },
            CpuCore {thread_id: 2, ..Default::default() },
            CpuCore {thread_id: 3, ..Default::default() }
        ],
        program_counter: 0,
        stride: 0,
        memory: [0; 64*1024]
    };

    let mut running = true;
    while running {
        match &instructions[state.program_counter as usize] {
            Instruction::Add(rd, rs1, rs2, source) => {
                for mut core in &mut state.cores {
                    let s2 = get_source2_value(&core, *rs2, source);
                    let result = core.registers[*rs1 as usize] + s2;
                    if !core.exec_mask {
                        set_flags(&mut core, result);
                        core.registers[*rd as usize] = result;
                    }
                }
            }
            Instruction::Sub(rd, rs1, rs2, source) => {
                for mut core in &mut state.cores {
                    let s2 = get_source2_value(&core, *rs2, source);
                    let result = core.registers[*rs1 as usize].wrapping_sub(s2);
                    if !core.exec_mask {
                        set_flags(&mut core, result);
                        core.registers[*rd as usize] = result;
                    }
                }
            }
            Instruction::And(rd, rs1, rs2, source) => {
                for mut core in &mut state.cores {
                    let s2 = get_source2_value(&core, *rs2, source);
                    let result = core.registers[*rs1 as usize] & s2;
                    if !core.exec_mask {
                        set_flags(&mut core, result);
                        core.registers[*rd as usize] = result;
                    }
                }
            }
            Instruction::Xor(rd, rs1, rs2, source) => {
                for mut core in &mut state.cores {
                    let s2 = get_source2_value(&core, *rs2, source);
                    let result = core.registers[*rs1 as usize] ^ s2;
                    if !core.exec_mask {
                        set_flags(&mut core, result);
                        core.registers[*rd as usize] = result;
                    }
                }
            }
            Instruction::ShiftLeft(rd, rs1, rs2, source) => {
                for mut core in &mut state.cores {
                    let s2 = get_source2_value(&core, *rs2, source);
                    let result = core.registers[*rs1 as usize] << s2;
                    if !core.exec_mask {
                        set_flags(&mut core, result);
                        core.registers[*rd as usize] = result;
                    }
                }
            }
            Instruction::ShiftRight(rd, rs1, rs2, source) => {
                for mut core in &mut state.cores {
                    let s2 = get_source2_value(&core, *rs2, source);
                    let result = core.registers[*rs1 as usize] >> s2;
                    if !core.exec_mask {
                        set_flags(&mut core, result);
                        core.registers[*rd as usize] = result;
                    }
                }
            }
            Instruction::Not(rd, rs1) => {
                for mut core in &mut state.cores {
                    let result = !core.registers[*rs1 as usize];
                    if !core.exec_mask {
                        set_flags(&mut core, result);
                        core.registers[*rd as usize] = result;
                    }
                }
            }
            Instruction::Mult(rs1, rs2, mult_set, mult_sign, mult_shift) => {
                for mut core in &mut state.cores {
                    let a = core.registers[*rs1 as usize];
                    let b = core.registers[*rs2 as usize];

                    let result = match mult_sign {
                        MultSign::UnsignedUnsigned => ((a as u32) * (b as u32)) as u32,
                        MultSign::UnsignedSigned => ((a as i32) * (b as i16) as i32) as u32,
                        MultSign::SignedUnsigned => ((a as i16) as i32 * (b as i32)) as u32,
                        MultSign::SignedSigned => ((a as i16) as i32 * (b as i16) as i32) as u32
                    };

                    let shift = match mult_shift {
                        MultShift::Up => result << 16,
                        MultShift::Down => result >> 16,
                        MultShift::No => result
                    };

                    let final_result = match mult_set {
                        MultSet::Set => shift,
                        MultSet::Accum => shift + core.accumulator
                    };

                    if !core.exec_mask {
                        core.accumulator = final_result;
                    }
                }
            }
            Instruction::Jump(rs) => {
                state.program_counter = state.cores[0].registers[*rs as usize] as u16;
            }
            Instruction::Branch(neg, zero, pos, imm) => {
                let addr = match labels.get(imm) {
                    Some(l) => *l as u16,
                    None => panic!("No label {} to load", imm)
                };
                let diff = (addr as i32) - (state.program_counter as i32);
                if diff < -128 || diff > 127 {
                    panic!("Tried to branch to label too far away")
                }

                for i in 0..4 {
                    state.cores[i].exec_mask = !((*neg && state.cores[i].negative)
                                               || (*zero && state.cores[i].zero)
                                               || (*pos && state.cores[i].positive));
                }

                let all_masked = state.cores.iter().fold(true, |acc, x| acc && x.exec_mask);

                if all_masked {
                    state.program_counter = addr;
                }
            }
            Instruction::Stride(imm) => {
                state.stride = *imm;
            }
            Instruction::Load(rd, rs, imm) => {
                for mut core in &mut state.cores {
                    if !core.exec_mask {
                        core.registers[*rs as usize] = state.memory[(*rd as u16 + *imm) as usize];
                    }
                }
            }
            Instruction::Store(rd, rs, imm) => {
                for core in &state.cores {
                    if !core.exec_mask {
                        state.memory[(*rd as u16 + *imm) as usize] = core.registers[*rs as usize];
                    }
                }
            }
            Instruction::LoadI(imm) => {
                for mut core in &mut state.cores {
                    if !core.exec_mask {
                        core.accumulator = *imm as u32;
                    }
                }
            }
            Instruction::LoadAddr(label) => {
                for mut core in &mut state.cores {
                    let addr = match labels.get(label) {
                        Some(l) => *l as u32,
                        None => panic!("No label {} to load", label)
                    };

                    if !core.exec_mask {
                        core.accumulator = addr;
                    }
                }
            }
        }

        state.program_counter += 1;
        if state.program_counter as usize >= instructions.len() {
            running = false;
        }
    }

    println!("CPU state is\n{}", state);
}

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

    let (instructions, labels) = parse(&asm_string).unwrap();

    for instruction in &instructions {
        println!("Instruction is {:?}", instruction)
    }

    simulate(instructions.as_slice(), &labels)
}
