
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
    
}

#[doc(hidden)]
pub fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
    let chars = input.chars.enumerate();
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
                    return Err(InvalidCharacter((i, c)));
                }
            }
        }
    }
    Ok(tokens)
}



#[cfg(test)]
mod tests {
    use super::*; 

    #[test]
    fn tokenize_fails_when_input_contains_invalid_characters() {
        assert_eq!(tokenize("( S X S ( S I ) S S )"), Err(InvalidCharacter((5,'X'))));
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

}
