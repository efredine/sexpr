use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, Clone)]
enum Token<'a> {
    Int(isize),
    Float(f64),
    Quoted(&'a str),
    String(&'a str),
    LeftPar,
    RightPar,
}

struct TokenIterator<'a> {
    source: &'a str,
    iter: Peekable<CharIndices<'a>>,
}

impl<'a> TokenIterator<'a> {
    fn new(source: &str) -> TokenIterator {
        let iter = source.char_indices().peekable();
        TokenIterator { source, iter }
    }

    fn get_str(&mut self, start: usize) -> Result<Token<'a>, String> {
        let mut end = start;
        while let Some((i, _)) = self.iter.next_if(|(_, x)| !is_atom_terminator(*x)) {
            end = i;
        }
        atom_end(
            Token::String(&self.source[start..end + 1]),
            self.iter.peek(),
        )
    }

    fn get_quoted(&mut self, start: usize) -> Result<Token<'a>, String> {
        let mut end = start;
        while let Some((i, _)) = self.iter.next_if(|(_, x)| *x != '"') {
            end = i;
        }
        let result = if end > start + 1 {
            &self.source[start + 1..end + 1]
        } else {
            ""
        };
        if let Some((_, end)) = self.iter.next() {
            if end == '"' {
                return atom_end(Token::Quoted(result), self.iter.peek());
            }
        }
        Err(format!("Invalid quoted string."))
    }

    fn get_float(&mut self, start: usize) -> Result<Token<'a>, String> {
        let mut end = start;
        while let Some((i, _)) = self.iter.next_if(|(_, x)| x.is_numeric()) {
            end = i;
        }
        match self.source[start..end + 1].parse::<f64>() {
            Ok(n) => atom_end(Token::Float(n), self.iter.peek()),
            Err(error) => Err(error.to_string()),
        }
    }

    fn get_number(&mut self, start: usize) -> Result<Token<'a>, String> {
        let mut end = start;
        while let Some((i, c)) = self.iter.next_if(|(_, x)| x.is_numeric() || *x == '.') {
            end = i;
            if c == '.' {
                return self.get_float(start);
            }
        }
        match self.source[start..end + 1].parse::<isize>() {
            Ok(n) => atom_end(Token::Int(n), self.iter.peek()),
            Err(error) => Err(error.to_string()),
        }
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Result<Token<'a>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((i, c)) = self.iter.next() {
            let token = match c {
                '(' => Some(Ok(Token::LeftPar)),
                ')' => Some(Ok(Token::RightPar)),
                '"' => Some(self.get_quoted(i)),
                '.' => Some(self.get_float(i)),
                c if c.is_numeric() || c == '-' => Some(self.get_number(i)),
                c if c.is_whitespace() => None,
                _ => Some(self.get_str(i)),
            };
            match token {
                Some(r) => match r {
                    Ok(t) => return Some(Ok(t)),
                    Err(error) => return Some(Err(error.to_string())),
                },
                None => continue,
            }
        }
        return None;
    }
}

#[derive(Debug)]
enum Expr<'a> {
    List(Vec<Expr<'a>>),
    Atom(&'a Token<'a>),
}

fn lex(source: &str) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut token_iter = TokenIterator::new(source);

    while let Some(token) = token_iter.next() {
        match token {
            Ok(t) => tokens.push(t),
            Err(error) => return Err(error.to_string()),
        };
    }
    Ok(tokens)
}

fn is_atom_terminator(c: char) -> bool {
    match c {
        ' ' | '\n' | '\r' | ')' | '(' | '"' => true,
        _ => false,
    }
}

fn atom_end<'a>(token: Token<'a>, next: Option<&(usize, char)>) -> Result<Token<'a>, String> {
    if let Some((_, c)) = next {
        if is_atom_terminator(*c) {
            return Ok(token);
        }
    }
    Err(format!("Atom not properly terminated."))
}

fn get_expression<'a>(tokens: &'a Vec<Token>) -> Result<Expr<'a>, String> {
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
            Token::String(s) => String::from(*s),
            Token::Int(i) => i.to_string(),
            Token::Float(f) => f.to_string(),
            Token::Quoted(q) => format!("\"{}\"", q),
            _ => "".to_string(),
        },
        Expr::List(e) => {
            let string_list: Vec<String> = e.iter().map(format_expression).collect();
            format!("({})", string_list.join(" "))
        }
    }
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
