
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
    // I Rule, 
    IReducible,
    // Not reducible
    NotReducible, 
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

       match *self {
            App(_) => {
                match self.is_reducible() {
                    RuleType::SReducible => {
                        self.apply_srule(count);
                    },
                    RuleType::KReducible => {
                        self.apply_krule(count);
                    },
                    RuleType::IReducible => {
                        self.apply_irule(count);
                    },
                    RuleType::NotReducible => {
                        if let Ok((ref mut lhs_ref, ref mut rhs_ref)) = self.unapp_mut() {
                            lhs_ref.reduce_lo(limit, count);
                            rhs_ref.reduce_lo(limit, count);
                        }
                    },
                }
            },
            _ => (),
       }
   }

    /// Applies the S combinator to the term. This function 
    /// can only be safely executed if the term is SReducible.
    fn apply_srule(&mut self, count: &mut usize) { 
        let to_apply = mem::replace(self, Null);
        if let Ok((lhs, rhs)) = to_apply.unapp() {
            if let Ok((llhs, rlhs)) = lhs.unapp() {
                if let Ok((_, rllhs)) = llhs.unapp() {
                    let new_term = app(app(rllhs,rhs.clone()), app(rlhs,rhs));
                    let _is_null = mem::replace(self, new_term);
                    *count += 1;
                }
            }
        }
    }
  
    /// Applies the K combinator to the term. This function 
    /// can only be safely executed if the term is KReducible.
    fn apply_krule(&mut self, count: &mut usize) { 
        let to_apply = mem::replace(self, Null);
        if let Ok((lhs, _)) = to_apply.unapp() {
            if let Ok((_, rlhs)) = lhs.unapp() {
                let _is_null = mem::replace(self, rlhs);
                *count += 1;
            }
        }
    }

    /// Applies the I combinator to the term. This function 
    /// can only be safely executed if the term is IReducible.
    fn apply_irule(&mut self, count: &mut usize) { 
        let to_apply = mem::replace(self, Null);
        if let Ok((_, rhs)) = to_apply.unapp(){
            let _is_null = mem::replace(self, rhs);
            *count += 1;
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
    /// assert_eq!(app(app(app(Com(S), Com(I)), Com(I)), Com(K)).is_reducible(), RuleType::SReducible);
    ///
    /// ```
    pub fn is_reducible(&self) -> RuleType {
        if let Ok((lhs, _)) = self.unapp_ref() {
            match lhs {
                Com(x) => {
                    match x {
                        I => return RuleType::IReducible,
                        _ => (),
                    }
                },
                _ => (),
            }
            if let Ok((llhs, _)) = lhs.unapp_ref() {
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
        return RuleType::NotReducible;
    }
}

#[cfg(test)]
mod tests {
    use super::*; 

    #[test]
    fn term_is_marked_as_reducible() {
        assert_eq!(app(app(app(Com(S), Com(I)), Com(I)), Com(K)).is_reducible(), RuleType::SReducible);
        assert_eq!(app(app(Com(K), Com(I)), Com(I)).is_reducible(), RuleType::KReducible);
        assert_eq!(app(Com(I), Com(K)).is_reducible(), RuleType::IReducible);
    }

    #[test]
    fn term_is_reduced_by_srule() {
        let mut test_term = app(app(app(Com(S), Com(I)), Com(I)), Com(K));
        test_term.apply_srule(&mut 0);
        assert_eq!(test_term, app(app(Com(I), Com(K)), app(Com(I), Com(K))));
    }

    #[test]
    fn term_is_reduced_by_krule() {
        let mut test_term = app(app(Com(K), Com(S)), Com(I));
        test_term.apply_krule(&mut 0);
        assert_eq!(test_term, Com(S));
    }

    #[test]
    fn term_is_reduced_by_irule() {
        let mut test_term = app(Com(I), Com(K));
        test_term.apply_irule(&mut 0);
        assert_eq!(test_term, Com(K));
    }
}
