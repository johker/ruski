/// A parser for lambda expressions



use std::fmt;
use crate::term::{Term, app, expand};
use crate::term::Term::{S,K,I};

/// An error returned by `parse()` when a parsing issue is encountered.
#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    /// lexical error; contains the invalid character and its index
    InvalidCharacter((usize, char)),
    /// syntax error; the expression is invalid
    InvalidExpression,
    /// syntax error; the expression is empty
    EmptyExpression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[doc(hidden)]
pub enum Token {
    ///  Starling Symbol
    S,
    /// Kestrel Symbol
    K,
    /// Idiot Symbol
    I,
    /// Left parenthesis
    Lparen,
    /// Right parenthesis
    Rparen,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self {
            Token::S => write!(f, "S"),
            Token::K => write!(f, "K"),
            Token::I => write!(f, "I"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
        }

    }
}

/// Attempts to parse the input `&str` as combinator `Term`. 
///
/// # Examples
pub fn parse(input: &str) {
    if let Ok(mut tokens) = tokenize(input) {
        if let Ok(_ast) = tokens_to_ast(&mut tokens, &mut 0) {
            // Do sth with ast
        }
    }
}

#[doc(hidden)]
pub fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
    let chars = input.chars().enumerate();
    let mut tokens = Vec::with_capacity(input.len());

    for (i, c) in chars {
        match c {
            'S' => tokens.push(Token::S),
            'K'=> tokens.push(Token::K),
            'I' => tokens.push(Token::I), 
            ')' => tokens.push(Token::Rparen),
            '(' => tokens.push(Token::Lparen), 
            _ => {
                if c.is_whitespace() {
                    // ignore
                } else {
                    return Err(ParseError::InvalidCharacter((i, c)));
                }
            }
        }
    }
    Ok(tokens)
}

#[doc(hidden)]
pub fn tokens_to_ast(tokens: &mut Vec<Token>, pos: &mut usize) -> Result<Term, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError::EmptyExpression);
    }

    let mut term = Term::Null;

    while let Some(token) = tokens.get(*pos) {
       // println!("Pos = {}, Token = {:?}", pos, token);
       match token {
            Token::S => term = expand(term, S),
            Token::K => term = expand(term, K),
            Token::I => term = expand(term, I),
            Token::Lparen => {
                *pos += 1;
                if let Ok(subterm) = tokens_to_ast(tokens, pos) {
                    term = app(term, subterm);
                }
            }
            Token::Rparen => return Ok(term),
       }
       // println!("Term = {:?}", term);
       *pos += 1;
    }
    return Ok(term);
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::term::Term::S;

    #[test]
    fn tokenize_fails_when_input_contains_invalid_characters() {
        assert_eq!(tokenize("( S X S ( S I ) S S )"), Err(ParseError::InvalidCharacter((4,'X'))));
    }

    #[test]
    fn tokenize_succeeds_when_valid_input_provided() {
        let test_input = "( S K S ( S I ) S S )"; 
        let tokens = tokenize(test_input);

        assert!(tokens.is_ok()); 
        assert_eq!(
            tokens.unwrap(),
            vec![
               Token::Lparen,
               Token::S,
               Token::K,
               Token::S,
               Token::Lparen,
               Token::S,
               Token::I,
               Token::Rparen,
               Token::S,
               Token::S,
               Token::Rparen
            ]
        );
    }

    #[test]
    fn tokens_to_ast_generates_binary_tree() {
        let mut tokens = vec![
            Token::S,
            Token::S,
            Token::S,
            Token::Lparen,
            Token::S,
            Token::S,
            Token::Rparen,
            Token::S,
            Token::S,
        ]; 
        let ast = tokens_to_ast(&mut tokens, &mut 0).unwrap();

        assert_eq!(ast,app(app(app(app(app(S,S),S),app(S,S)),S),S));
    }

    #[test]
    fn list_reductions_of_term() {
        let term_str = "S ( S S S ( S S ( K K S ) S ) ) S S ( S ( K S K ) K S )";
        let test_term = app(app(app(app(S,app(app(app(S,S),S),app(app(app(S,S),app(app(K,K),S)),S))),S),S),app(app(app(S,app(app(K,S),K)),K),S));
        assert_eq!(tokens_to_ast(&mut tokenize(&term_str).unwrap(), &mut 0).unwrap(), test_term);

    }

}
