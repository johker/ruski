
pub use self::Order::*;
pub use self::RuleType::*;
use crate::term::Term::*;
use crate::term::Combinator::*;
use crate::term::{Term, TermError, app};
use std::mem;

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

       //match self.is_reducible(limit, *count) {
       //     RuleType::SReducible =>  
       //}
   }

    /// Applies the S combinator to the term. This function 
    /// can only be safely executed if the term is SReducible.
    fn apply_srule(&mut self) -> Result<(), TermError> { 
        let to_apply = mem::replace(self, Null);
        let (lhs, rhs) = to_apply.unapp()?;
        let (llhs, rlhs) = lhs.unapp()?;
        let (_, rllhs) = llhs.unapp()?;
        let new_term = app(app(rllhs,rhs.clone()), app(rlhs,rhs));
        let _is_null = mem::replace(self, new_term);
        Ok(())
    }
  
    /// Applies the K combinator to the term. This function 
    /// can only be safely executed if the term is KReducible.
    fn apply_krule(&mut self) -> Result<(), TermError> { 
        // TODO
        let to_apply = mem::replace(self, Null);
        let (lhs, rhs) = to_apply.unapp()?;
        let (_, rlhs) = lhs.unapp()?;
        let _is_null = mem::replace(self, rlhs);
        Ok(())
    }

    /// Checks if the term can be reduced at the top level.
    /// If it can be reduced the rule type that applies is
    /// returned.
    ///
    /// # Example
    /// ```
    /// use ruski::*;
    ///
    /// assert_eq!(app(app(app(Com(S), Com(I)), Com(I)), Com(K)).is_reducible(), RuleType::SReducible);
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
                        let (ref lhs, ref _rhs) = **boxed;
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
        assert_eq!(app(app(Com(K), Com(I)), Com(I)).is_reducible(), RuleType::KReducible);
    }


    #[test]
    fn term_is_reduced_by_srule() {
        let mut test_term = app(app(app(Com(S), Com(I)), Com(I)), Com(K));
        assert_eq!(test_term.apply_srule(), Ok(()));
        assert_eq!(test_term, app(app(Com(I), Com(K)), app(Com(I), Com(K))));
    }

    #[test]
    fn term_is_reduced_by_krule() {
        let mut test_term = app(app(Com(K), Com(S)), Com(I));
        assert_eq!(test_term.apply_krule(), Ok(()));
        assert_eq!(test_term, Com(S));
    }
}
