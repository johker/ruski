
pub use self::Order::*;
pub use self::RuleType::*;
use crate::term::Term::*;
use crate::term::Combinator::*;
use crate::term::Term;
use crate::term::app;

/// The [evaluation
/// order](https://writings.stephenwolfram.com/2020/12/combinators-a-centennial-view/#the-question-of-evaluation-order)
/// of reductions. 
pub enum Order {
    // Leftmost outermost
    LO, 
    // Leftmost innermost
    LI,
    // Rightmost outermost
    RO,
    // Rightmost innermost
    RI,
    // Outermost leftmost
    OL,
    // Outermost rightmost 
    OR,
    // Innermost leftmost
    IL, 
    // Innermost rightmost
    IR,
}

/// Describes the Rule that applies to a node 
/// in an AST
#[derive(Debug, PartialEq, Eq)]
pub enum RuleType {
   // S Rule
    SReducible,
    // K Rule
    KReducible,
    // Not reducible
    Irreducible, 
}

impl Term {

    /// Performs a reduction on a `Term` with the specified evaluation `Order` and
    /// an optional limit on the number of reductions (`0` means no limit) and 
    /// returns the reduced `Term`.
    ///
    /// TODO: Add Example
    pub fn reduce(&mut self, order: Order, limit: usize) -> usize{
        let mut count = 0;
        match order {
            LO => self.reduce_lo(limit, &mut count),
            LI => self.reduce_lo(limit, &mut count),
            RO => self.reduce_lo(limit, &mut count),
            RI => self.reduce_lo(limit, &mut count),
            OL => self.reduce_lo(limit, &mut count),
            OR => self.reduce_lo(limit, &mut count),
            IL => self.reduce_lo(limit, &mut count),
            IR => self.reduce_lo(limit, &mut count),
        }
        count
    }

   fn reduce_lo(&mut self, limit: usize, count: &mut usize) {
       if limit != 0 && *count == limit {
           return;
       }
   }


    /// Checks if the term can be reduced at the top level.
    /// If it can be reduced the rule type that applies is
    /// returned.
    ///
    /// # Example
    /// ```
    /// use ruski::*;
    ///
    /// ```
    pub fn is_reducible(&self) -> RuleType {
        if let Ok(lhs) = self.lhs_ref() {
            if let Ok(llhs) = lhs.lhs_ref() {
                match llhs {
                    Com(x) => {
                        match x {
                            K => return RuleType::KReducible,
                            _ => (),
                        }
                    },
                    App(boxed) => {
                        let (ref lhs, ref rhs) = **boxed;
                        match lhs {
                            Com(x) => {
                                match x {
                                    S => return RuleType::SReducible,
                                    _ => (),
                                }
                            },
                            _ => (),
                        }
                    },
                    _ => (),
                }
            }
        }
        return RuleType::Irreducible;
    }

}

#[cfg(test)]
mod tests {
    use super::*; 

    #[test]
    fn term_is_marked_as_reducible() {
        assert_eq!(app(app(app(Com(S), Com(I)), Com(I)), Com(K)).is_reducible(), RuleType::SReducible);
    }
}
