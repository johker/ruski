
pub mod term;
pub mod parser;
pub mod reduction;
pub mod graph;
pub mod evolution;

pub use self::term::Term::*;
pub use self::reduction::RuleType;
pub use self::term::{Term, app, expand};
