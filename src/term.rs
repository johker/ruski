//! [Combinator terms](https://en.wikipedia.org/wiki/SKI_combinator_calculus)

pub use self::Term::*;
use self::TermError::*;
use crate::parser::{Token};

/// A combinator term is either a variable representing a combinatory term, a base
/// (S, K or I) or an application of one term to another. 
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    /// Empty Term
    Null,
    /// Starling - S - Combinator
    S,
    /// Kestrel - K - Combinator
    K,
    /// Idiot - I - Combinator
    I,
    /// Application
    App(Box<(Term, Term)>), 
}



/// An error that can be returned when an inapplicable function is applied to a `Term`.
#[derive(Debug, PartialEq, Eq)]
pub enum TermError {
    /// the term is not an abstraction
    NotAbs,
    /// the term is not an application
    NotApp,
}

impl Term {

    /// Returns a pair containing an application's underlying terms, consuming it in the process.
    ///
    /// # Example
    /// ```
    /// use ruski::*;
    ///
    /// assert_eq!(app(S, K).unapp(), Ok((S, K)));
    /// ```
    /// # Errors
    ///
    /// Returns a `TermError` if `self` is not an `App`lication.
    pub fn unapp(self) -> Result<(Term, Term), TermError> {
        if let App(boxed) = self {
            let (lhs, rhs) = *boxed;
            Ok((lhs, rhs))
        } else {
            Err(NotApp)
        }
    }

    /// Returns a pair containing mutable references to an application's underlying terms.
    ///
    /// # Example
    /// ```
    /// use ruski::*;
    ///
    /// assert_eq!(app(S, K).unapp_mut(), Ok((&mut S, &mut K)));
    /// ```
    /// # Errors
    ///
    /// Returns a `TermError` if `self` is not an `App`lication.
    pub fn unapp_mut(&mut self) -> Result<(&mut Term, &mut Term), TermError> {
        if let App(boxed) = self {
            let (ref mut lhs, ref mut rhs) = **boxed;
            Ok((lhs, rhs))
        } else {
            Err(NotApp)
        }
    }

    /// Returns a pair containing references to an application's underlying terms.
    ///
    /// # Example
    /// ```
    /// use ruski::*;
    ///
    /// assert_eq!(app(S, K).unapp_ref(), Ok((&S, &K)));
    /// ```
    /// # Errors
    ///
    /// Returns a `TermError` if `self` is not an `App`lication.
    pub fn unapp_ref(&self) -> Result<(&Term, &Term), TermError> {
        if let App(boxed) = self {
            let (ref lhs, ref rhs) = **boxed;
            Ok((lhs, rhs))
        } else {
            Err(NotApp)
        }
    }

    #[doc(hidden)]
    pub fn flat(&self) -> Vec<Token> {
        vec![]
    }
}

/// Produces an `App`lication of two given `Term`s without any reduction, 
/// consuming them in the process.
///
/// # Example
/// Adds a combinator to a term either as an abstraction or
/// as application depending on the current state of the term.
/// The argument is consumed in the process. 
///
/// # Example
/// ```
/// use ruski::*;
///
/// assert_eq!(expand(Null, S), S);
/// assert_eq!(expand(S, S), app(S, S));
/// ```
pub fn expand(lterm: Term, rterm: Term) -> Term {
    match lterm {
        Term::Null => rterm,
        _ => app(lterm, rterm),
    }
}

/// ```
/// use ruski::*;
///
/// assert_eq!(app(S, K), App((Box::new((S, K)))));
/// ```
pub fn app(lhs: Term, rhs: Term) -> Term {
    App(Box::new((lhs, rhs)))
}
