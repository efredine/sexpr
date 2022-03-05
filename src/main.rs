use std::iter::Peekable;
use std::num::{ParseFloatError, ParseIntError};
use std::str::CharIndices;

#[derive(Debug)]
enum Error {
    AtomTerminationError,
    FloatError(ParseFloatError),
    IntError(ParseIntError),
    TooManyRightParens,
    MissingClosingParen,
    QuoteError,
}

#[derive(Debug)]
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

    fn atom_end(&mut self, token: Token<'a>) -> Result<Token<'a>, Error> {
        if let Some((_, c)) = self.iter.peek() {
            if is_atom_terminator(*c) {
                return Ok(token);
            }
        }
        Err(Error::AtomTerminationError)
    }

    fn consume_while(
        &mut self,
        condition: for<'r> fn(&'r (usize, char)) -> bool,
    ) -> Option<(usize, char)> {
        let mut prev: Option<(usize, char)> = None;
        while let Some(next) = self.iter.next_if(condition) {
            prev = Option::from(next);
        }
        return prev;
    }

    fn get_range(&self, start: (usize, char), end: (usize, char)) -> &'a str {
        &self.source[start.0..end.0 + end.1.len_utf8()]
    }

    fn get_str(&mut self, start: (usize, char)) -> Result<Token<'a>, Error> {
        let end = self
            .consume_while(|(_, x)| !is_atom_terminator(*x))
            .unwrap_or(start);
        self.atom_end(Token::String(self.get_range(start, end)))
    }

    fn get_quoted(&mut self, start: (usize, char)) -> Result<Token<'a>, Error> {
        let end = self.consume_while(|(_, x)| *x != '"').unwrap_or(start);
        if let Some((_, next)) = self.iter.next() {
            if next == '"' {
                return self.atom_end(Token::Quoted(self.get_range(start, end)));
            }
        }
        Err(Error::QuoteError)
    }

    fn get_float(&mut self, start: (usize, char)) -> Result<Token<'a>, Error> {
        let end = self.consume_while(|(_, x)| x.is_numeric()).unwrap_or(start);
        match self.get_range(start, end).parse::<f64>() {
            Ok(n) => self.atom_end(Token::Float(n)),
            Err(error) => Err(Error::FloatError(error)),
        }
    }

    fn get_number(&mut self, start: (usize, char)) -> Result<Token<'a>, Error> {
        let mut end = start;
        while let Some(next) = self.iter.next_if(|(_, x)| x.is_numeric() || *x == '.') {
            end = next;
            if next.1 == '.' {
                return self.get_float(start);
            }
        }
        match self.get_range(start, end).parse::<isize>() {
            Ok(n) => self.atom_end(Token::Int(n)),
            Err(error) => Err(Error::IntError(error)),
        }
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(next) = self.iter.next() {
            match next.1 {
                '(' => return Some(Ok(Token::LeftPar)),
                ')' => return Some(Ok(Token::RightPar)),
                '"' => return Some(self.get_quoted(next)),
                '.' => return Some(self.get_float(next)),
                c if c.is_numeric() || c == '-' => return Some(self.get_number(next)),
                c if c.is_whitespace() => continue,
                _ => return Some(self.get_str(next)),
            };
        }
        return None;
    }
}

fn is_atom_terminator(c: char) -> bool {
    match c {
        ')' | '(' | '"' => true,
        c if c.is_whitespace() => true,
        _ => false,
    }
}

#[derive(Debug)]
enum Expr<'a> {
    List(Vec<Expr<'a>>),
    Atom(Token<'a>),
}

#[derive(Debug)]
struct ExpressionParser<'a> {
    stack: Vec<Vec<Expr<'a>>>,
    iter: TokenIterator<'a>,
}

impl<'a> ExpressionParser<'a> {
    fn new(source: &'a str) -> ExpressionParser<'a> {
        ExpressionParser {
            iter: TokenIterator::new(source),
            stack: Vec::new(),
        }
    }

    fn pop_or_else(&mut self, error: Error) -> Option<Result<Expr<'a>, Error>> {
        match self.stack.pop() {
            Some(expr_list) => Some(Ok(Expr::List(expr_list))),
            None => Some(Err(error)),
        }
    }

    fn parse_expression(&mut self) -> Option<Result<Expr<'a>, Error>> {
        while let Some(result) = self.iter.next() {
            match result {
                Err(e) => return Some(Err(e)),
                Ok(token) => match token {
                    Token::LeftPar => {
                        self.stack.push(Vec::new());
                        match self.parse_expression() {
                            Some(result) => match result {
                                Err(error) => return Some(Err(error)),
                                Ok(expr) => match self.stack.pop() {
                                    Some(mut top) => {
                                        top.push(expr);
                                        self.stack.push(top);
                                    }
                                    None => return Some(Ok(expr)),
                                },
                            },
                            None => return self.pop_or_else(Error::MissingClosingParen),
                        }
                    }
                    Token::RightPar => return self.pop_or_else(Error::TooManyRightParens),
                    _ => match self.stack.pop() {
                        Some(mut top) => {
                            top.push(Expr::Atom(token));
                            self.stack.push(top)
                        }
                        None => return Some(Ok(Expr::Atom(token))),
                    },
                },
            }
        }

        None
    }
}

impl<'a> Iterator for ExpressionParser<'a> {
    type Item = Result<Expr<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_expression()
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
    let mut parser = ExpressionParser::new(&expr);
    while let Some(parsed_expr) = parser.next() {
        println!("{}", format_expression(&parsed_expr.unwrap()));
    }
}
