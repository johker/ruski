//! [Combinator terms](https://en.wikipedia.org/wiki/SKI_combinator_calculus)

pub use self::Term::*;

/// A combinator term is either a variable representing a combinatory term, a base
/// (S, K or I) or an application of one term to another. 
#[derive(Debug, PartialEq, Eq)]
pub enum Term {
    /// Empty Term
    Null,
    /// Combinator 
    Com(Combinator),
    /// Application
    App(Box<(Term, Term)>), 
}

/// Base symbols for the compuational system
#[derive(Debug, PartialEq, Eq)]
pub enum Combinator {
    /// Starling - S - Combinator
    S,
    /// Kestrel - K - Combinator
    K,
    /// Idiot - I - Combinator
    I,
}


impl Term {

    /// Adds a combinator to a term either as an abstraction or
    /// as application depending on the current state of the term.
    /// In case of an application the combinator is added to the left 
    /// to ensure left assiociativity.
    /// The argument is consumed in the process. 
    ///
    pub fn expand(term: Term, combinator: Combinator) -> Term {
        match term {
            Term::Null => Com(combinator), 
            _ => app(Com(combinator), term),
        }
    }

}

/// Produces an `App`lication of two given `Term`s without any reduction, 
/// consuming them in the process.
///
/// # Example
/// ```
/// use ruski::*;
///
/// assert_eq!(app(Com(S), Com(K)), App(Box::new(Com(S), Com(K))));
/// ```
pub fn app(lhs: Term, rhs: Term) -> Term {
    App(Box::new((lhs, rhs)))
}
