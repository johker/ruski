//! [Combinator terms](https://en.wikipedia.org/wiki/SKI_combinator_calculus)

/// A combinator term is either a variable representing a combinatory term, a base
/// (S, K or I) or an application of one term to another. 
pub enum Term {
    /// Abstraction
    Abs(Base),
        /// Application
    App(Box<(Term, Term)>), 
}

/// Base symbols for the compuational system
pub enum Base {
    /// Starling - S - Combinator
    S,
    /// Kestrel - K - Combinator
    K,
    /// Idiot - I - Combinator
    I,
}
