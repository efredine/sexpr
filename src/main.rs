use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
enum Token {
    Int(isize),
    Float(f64),
    Quoted(String),
    String(String),
    LeftPar,
    RightPar,
}

fn main() {
    let expr = String::from(
        "((data \"quoted data\" 123 4.5)
 (data (!@# (4.5) \"(more\" \"data)\")))",
    );

    // let simple = String::from("( abc 123 0.67 )  ( 100.67 )");
    // let simple = String::from("0.67.67");
    // println!("{}", simple);
    let tokens = lex(&expr);
    println!("{:?}", tokens.unwrap());
}

fn lex(source: &String) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut iter = source.chars().peekable();
    while let Some(_) = iter.next_if(|x| x.is_whitespace()) {}
    while let Some(c) = iter.next() {
        let token = match c {
            '(' => Ok(Token::LeftPar),
            ')' => Ok(Token::RightPar),
            '"' => get_quoted(&mut iter),
            '0'..='9' | '.' => get_number(c, &mut iter),
            _ => get_str(c, &mut iter),
        };
        match token {
            Ok(t) => tokens.push(t),
            Err(error) => return Err(error.to_string()),
        }
    }
    Ok(tokens)
}

fn is_atom_terminator(c: char) -> bool {
    match c {
        ' ' | '\n' | '\r' | ')' | '(' | '"' => true,
        _ => false,
    }
}

fn atom_end(token: Token, next: Option<&char>) -> Result<Token, String> {
    if let Some(c) = next {
        if is_atom_terminator(*c) {
            return Ok(token);
        }
    }
    Err(format!("Atom not properly terminated."))
}

fn get_str(first: char, iter: &mut Peekable<Chars>) -> Result<Token, String> {
    let mut result = String::from(first);
    while let Some(c) = iter.next_if(|&x| !is_atom_terminator(x)) {
        result.push(c);
    }
    atom_end(Token::String(result), iter.peek())
}

fn get_number(first: char, iter: &mut Peekable<Chars>) -> Result<Token, String> {
    let mut dot_count = if first == '.' { 1 } else { 0 };
    let mut result = String::from(first);
    while let Some(c) = iter.next_if(|&x| x.is_numeric() || x == '.') {
        if c == '.' {
            dot_count += 1
        }
        if dot_count > 1 {
            return Err(format!("Invalid number format."));
        }
        result.push(c);
    }
    if dot_count == 0 {
        match result.parse::<isize>() {
            Ok(n) => atom_end(Token::Int(n), iter.peek()),
            Err(error) => Err(error.to_string()),
        }
    } else {
        match result.parse::<f64>() {
            Ok(n) => atom_end(Token::Float(n), iter.peek()),
            Err(error) => Err(error.to_string()),
        }
    }
}

fn get_quoted(iter: &mut Peekable<Chars>) -> Result<Token, String> {
    let mut result = String::new();
    while let Some(c) = iter.next_if(|&x| x != '"') {
        result.push(c);
    }
    if let Some(end) = iter.next() {
        if end == '"' {
            return atom_end(Token::Quoted(result), iter.peek());
        }
    }
    Err(format!("Invalid quoted string."))
}
