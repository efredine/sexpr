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

#[derive(Debug)]
enum Expr<'a> {
    List(Vec<Expr<'a>>),
    Atom(&'a Token),
}

fn main() {
    let expr = String::from(
        "((data \"quoted data\" 123 4.5)
 (data (!@# (4.5) \"(more\" \"data)\")))",
    );

    println!("{}", expr);
    let tokens = lex(&expr).unwrap();
    println!("{:?}", tokens);
    let expression = get_expression(&tokens).unwrap();
    println!("{:?}", expression);
    println!("{}", format_expression(&expression));
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
            '0'..='9' => get_number(c, &mut iter),
            '.' => get_float(String::from(c), &mut iter),
            _ => get_str(c, &mut iter),
        };
        match token {
            Ok(t) => tokens.push(t),
            Err(error) => return Err(error.to_string()),
        }
        while let Some(_) = iter.next_if(|x| x.is_whitespace()) {}
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
    let mut result = String::from(first);
    while let Some(c) = iter.next_if(|&x| x.is_numeric() || x == '.') {
        result.push(c);
        if c == '.' {
            return get_float(result, iter);
        }
    }
    match result.parse::<isize>() {
        Ok(n) => atom_end(Token::Int(n), iter.peek()),
        Err(error) => Err(error.to_string()),
    }
}

fn get_float(first: String, iter: &mut Peekable<Chars>) -> Result<Token, String> {
    let mut result = String::from(first);
    while let Some(c) = iter.next_if(|&x| x.is_numeric()) {
        result.push(c);
    }
    match result.parse::<f64>() {
        Ok(n) => atom_end(Token::Float(n), iter.peek()),
        Err(error) => Err(error.to_string()),
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

fn get_expression(tokens: &Vec<Token>) -> Result<Expr, String> {
    let mut iter = tokens.iter();
    if let Some(token) = iter.next() {
        return match token {
            Token::LeftPar => parse_expression(&mut iter),
            _ => Err(format!("Invalid root expression.")),
        };
    }
    Err(format!("Missing root expression."))
}

fn parse_expression<'a>(iter: &mut std::slice::Iter<'a, Token>) -> Result<Expr<'a>, String> {
    let mut expr_list: Vec<Expr<'a>> = Vec::new();
    while let Some(token) = iter.next() {
        let node = match token {
            Token::LeftPar => parse_expression(iter),
            Token::RightPar => return Ok(Expr::List(expr_list)),
            _ => Ok(Expr::Atom(&token)),
        };
        match node {
            Ok(expr) => expr_list.push(expr),
            Err(error) => return Err(error),
        }
    }

    Err(format!("Missing closing paren"))
}

fn format_expression(expression: &Expr) -> String {
    match expression {
        Expr::Atom(t) => match t {
            Token::String(s) => s.clone(),
            Token::Int(i) => i.to_string(),
            Token::Float(f) => f.to_string(),
            Token::Quoted(q) => format!("\"{}\"", q),
            _ => String::from(""),
        },
        Expr::List(e) => {
            let string_list: Vec<String> = e.iter().map(format_expression).collect();
            format!("({})", string_list.join(" "))
        }
    }
}
