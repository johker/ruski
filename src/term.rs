//! [Combinator terms](https://en.wikipedia.org/wiki/SKI_combinator_calculus)

pub use self::Term::*;
use self::TermError::*;

/// A combinator term is either a variable representing a combinatory term, a base
/// (S, K or I) or an application of one term to another. 
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    /// Empty Term
    Null,
    /// Combinator 
    Com(Combinator),
    /// Application
    App(Box<(Term, Term)>), 
}

/// Base symbols for the compuational system
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Combinator {
    /// Starling - S - Combinator
    S,
    /// Kestrel - K - Combinator
    K,
    /// Idiot - I - Combinator
    I,
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
    /// assert_eq!(app(Com(S), Com(K)).unapp(), Ok((Com(S), Com(K))));
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

    /// Returns a pair containing references to an application's underlying terms.
    ///
    /// # Example
    /// ```
    /// use ruski::*;
    ///
    /// assert_eq!(app(Com(S), Com(K)).unapp_ref(), Ok((&Com(S), &Com(K))));
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
/// assert_eq!(expand(Term::Null, Combinator::S), Com(S));
/// assert_eq!(expand(Com(S), Combinator::S), app(Com(S), Com(S)));
/// ```
pub fn expand(term: Term, combinator: Combinator) -> Term {
    match term {
        Term::Null => Com(combinator), 
        _ => app(term, Com(combinator)),
    }
}

/// ```
/// use ruski::*;
///
/// assert_eq!(app(Com(S), Com(K)), App((Box::new((Com(S), Com(K))))));
/// ```
pub fn app(lhs: Term, rhs: Term) -> Term {
    App(Box::new((lhs, rhs)))
}
