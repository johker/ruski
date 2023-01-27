//! A parser for lambda expressions

use crate::term::Term;

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

#[derive(Debug, PartialEq, Eq)]
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


/// Attempts to parse the input `&str` as combinator `Term`. 
///
/// # Examples
pub fn parse(input: &str) {
    if let Ok(tokens) = tokenize(input) {
        if let Ok(ast) = get_ast(&tokens) {
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
pub fn get_ast(tokens: &mut Vec<Token>) -> Result<Token, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError::EmptyExpression);
    }

    let mut term = Term::Null;

    while let Some(token) = tokens.pop() {
       match token {
            Token::S => term.add(Combinator::S),
            Token::K => term.add(Combinator::K),
            Token::I => term.add(Combinator::I),
            Token::Lparen => {
                if let Ok(subterm) = get_ast(tokens) {
                    term = Term::App(subterm, term);
                }
            }
            Token::Rparen => return Ok(term);
       }
    }
}


#[cfg(test)]
mod tests {
    use super::*; 

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
    fn succ_ast() {
        let tokens = vec![
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
        ]; 
        let ast = get_ast(&tokens);
    }
}
