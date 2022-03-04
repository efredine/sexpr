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

#[derive(Debug)]
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

#[derive(Debug)]
enum Expr<'a> {
    List(Vec<Expr<'a>>),
    Atom(Token<'a>),
}

#[derive(Debug)]
struct ExpressionParser<'a> {
    iter: TokenIterator<'a>,
}

impl<'a> ExpressionParser<'a> {
    fn new(source: &'a str) -> ExpressionParser<'a> {
        ExpressionParser {
            iter: TokenIterator::new(source),
        }
    }

    fn parse_expression(&mut self) -> Result<Expr<'a>, String> {
        let mut expr_list: Vec<Expr> = Vec::new();
        while let Some(result) = self.iter.next() {
            match result {
                Ok(token) => match token {
                    Token::LeftPar => match self.parse_expression() {
                        Ok(e) => expr_list.push(e),
                        Err(e) => return Err(e),
                    },
                    Token::RightPar => return Ok(Expr::List(expr_list)),
                    _ => expr_list.push(Expr::Atom(token)),
                },
                Err(e) => return Err(e),
            };
        }

        Err(format!("Missing closing paren"))
    }

    fn get_expression(&mut self) -> Result<Expr<'a>, String> {
        if let Some(result) = self.iter.next() {
            return match result {
                Ok(token) => match token {
                    Token::LeftPar => self.parse_expression(),
                    _ => Err(format!("Invalid root expression.")),
                },
                Err(e) => Err(e),
            };
        }
        Err(format!("Missing root expression."))
    }
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
    let tokens: Result<Vec<_>, _> = TokenIterator::new(&expr).collect();
    println!("{:?}", tokens.unwrap());
    let expression = ExpressionParser::new(&expr).get_expression().unwrap();
    println!("{}", format_expression(&expression));
}
