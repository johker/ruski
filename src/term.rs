
/// A combinator term is either a variable representing a combinatory term, a base
/// (S, K or I) or an application of one term to another. 
pub enum Term {
    // Abstraction
    Abs(Base),
    // Application
    App(Box<(Term, Term)>), 
}

pub enum Base {
    /// Starling - S - Combinator
    S,
    /// Kestrel - K - Combinator
    K,
    /// Idiot - I - Combinator
    I,
}
