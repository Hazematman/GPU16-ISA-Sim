use std::*;
use std::iter::*;

#[derive(Debug)]
pub enum Token {
    Text(String),
    Num(u16),
    NewLine,
    Comma,
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
        if !c.is_alphanumeric() {
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
                result.push(Token::Num(n));
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
    Add(u8, u8, u8),
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

fn parse_add<'a>(it: &mut impl Iterator<Item=&'a Token>) -> Instruction {
    let r1 = match parse_reg(it) {
        Ok(reg) => reg,
        Err(message) => panic!("Error parsing add instruction, expected register, got: {:?}", message)
    };

    match it.next().unwrap() {
        Token::Comma => (),
        unknown => panic!("Error parsing add instruction, expected comma, got: {:?}", unknown)
    };

    let r2 = match parse_reg(it) {
        Ok(reg) => reg,
        Err(message) => panic!("Error parsing add instruction, expected register, got: {:?}", message)
    };

    match it.next().unwrap() {
        Token::Comma => (),
        unknown => panic!("Error parsing add instruction, expected comma, got: {:?}", unknown)
    };

    let r3 = match parse_reg(it) {
        Ok(reg) => reg,
        Err(message) => panic!("Error parsing add instruction, expected register, got: {:?}", message)
    };

    /* Expect comment or newline at the end */
    match it.next().unwrap() {
        Token::Comment(_) => (),
        Token::NewLine => (),
        unknown => panic!("Error parsing add instruction, expected end of line, got: {:?}", unknown)
    }

    Instruction::Add(r1, r2, r3)
}

fn parse_instructions(tokens: &[Token]) -> Result<Vec<Instruction>, String> {
    let mut instructions = Vec::new();
    let mut it = tokens.iter();

    while let Some(&ref t) = it.next() {
        match t {
            Token::Text(text) => {
                match text.as_str() {
                    "add" => {
                        let add_instr = parse_add(&mut it);
                        instructions.push(add_instr);
                    }
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

    for t in &tokens {
        println!("Token is {:?}", t);
    }

    let instructions = parse_instructions(tokens.as_slice()).unwrap();

    Ok(instructions)
}

fn main() {
    let test_string = "add r1, r2, r3;this is a comment\n";
    let instructions = parse(&String::from(test_string)).unwrap();

    for instruction in instructions {
        println!("Instruction is {:?}", instruction)
    }
}
