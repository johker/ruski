//! [Combinator terms](https://en.wikipedia.org/wiki/SKI_combinator_calculus)

/// A combinator term is either a variable representing a combinatory term, a base
/// (S, K or I) or an application of one term to another. 
#[derive(Debug, PartialEq, Eq)]
pub enum Term {
    /// Empty Term
    Null,
    /// Abstraction
    Abs(Box<Combinator>),
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
    /// Adds a combinator to a term either as an 
    /// abstraction or as application depending on 
    /// the current state of the term.
    ///
    pub fn add(mut self, combinator: Combinator) {
        if self == Term::Null {
            self = Term::Abs(Box::new(combinator));
        } else {
            self = Term::App(Box::new((self, Term::Abs(Box::new(combinator)))));
        }
    }
}
