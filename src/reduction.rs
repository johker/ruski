
pub use self::Order::*;
pub use self::RuleType::*;
use crate::term::Term::*;
use crate::parser::{Token, tokens_to_ast};
use crate::term::{Term, app};
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

pub struct Matches {
    s: usize,
    k: usize,
    i: usize,
}

impl Matches {

    pub fn new() -> Self {
        Matches {
            s: 0,
            k: 0,
            i: 0,
        }
    }

    pub fn sum(&self) -> usize {
       self.s + self.k + self.i
    }
}


impl Term {

    /// Reduces the AST of the passed token sequence for every possible match
    /// and pushes the flat token sequence to the reductions vector.
    pub fn derive_reductions(tokens: &mut Vec<Token>, reductions: &mut Vec<Vec<Token>>) {
        if let Ok(ast) = tokens_to_ast(tokens, &mut 0) {
            let mut n = 0; // Number applied reductions
            let mut m = 0; // Number of possible reductions in current iteration
            loop {
                let mut next_reduced_term = ast.clone();
                if !next_reduced_term.reduce_nth_match(&mut m, &mut n) {
                    break; // No new match
                }
                reductions.push(next_reduced_term.flat());
                m = 0;
            }
        }
    }

    /// Recursively traverses the AST and increases the current match index m
    /// if a reduction rule matches. Applies the nth possible reduction to the term.
    /// Once a reduction is applied it returns true, false otherwise.
    fn reduce_nth_match(&mut self, m: &mut usize, n: &mut usize) -> bool{
        if let App(_) = *self {
            match self.is_reducible() {
                RuleType::SReducible => {
                    *m += 1;
                    if m > n {
                        self.apply_srule(&mut 0);
                        *n += 1;
                        return true;
                    }
                },
                RuleType::KReducible => {
                    *m += 1;
                    if m > n {
                        self.apply_krule(&mut 0);
                        *n += 1;
                        return true;
                    }
                },
                RuleType::IReducible => {
                    *m += 1;
                    if m > n {
                        self.apply_irule(&mut 0);
                        *n += 1;
                        return true;
                   }
                },
                RuleType::NotReducible => (),
            }
            if let Ok((ref mut lhs_ref, ref mut rhs_ref)) = self.unapp_mut() {
                if lhs_ref.reduce_nth_match(m, n) {
                    return true;
                }
                if rhs_ref.reduce_nth_match(m, n) {
                    return true;
                }
            }
        }
        return false;
    }




    /// Performs a reduction on a `Term` with the specified evaluation `Order` and
    /// an optional limit on the number of reductions (`0` means no limit) and 
    /// returns the reduced `Term`.
    ///
    /// TODO: Add reductions for evaluation orders 
    pub fn reduce(&mut self, order: Order, limit: usize) -> usize{
        let mut count = 0;
        match order {
            LO => self.reduce_lo(limit, &mut count),
            LI => self.reduce_li(limit, &mut count),
            RO => self.reduce_lo(limit, &mut count),
            RI => self.reduce_lo(limit, &mut count),
            OL => self.reduce_lo(limit, &mut count),
            OR => self.reduce_lo(limit, &mut count),
            IL => self.reduce_lo(limit, &mut count),
            IR => self.reduce_lo(limit, &mut count),
        }
        count
    }

    fn reduce_li(&mut self, limit: usize, count: &mut usize) { 
        if limit != 0 && *count == limit {
            return;
        }

        if let Ok((ref mut lhs_ref, ref mut rhs_ref)) = self.unapp_mut() {
            lhs_ref.reduce_li(limit, count);
            rhs_ref.reduce_li(limit, count);
        }

        if let App(_) = *self {

        }
    }

    /// Count matches up to the defined limit (or indefinitevly for limit 0)
    /// of SKI rule by traversing the tree.
    pub fn count_matches(&self, limit: usize, matches: &mut Matches) {
        if limit != 0 && matches.sum() == limit {
            return;
        }
        if let App(_) = *self {
            match self.is_reducible() {
                RuleType::SReducible => {
                    matches.s += 1;
                },
                RuleType::KReducible => {
                    matches.k += 1;
                },
                RuleType::IReducible => {
                    matches.i += 1;
                },
                RuleType::NotReducible => {
                    // Do nothing 
                },
            }
            if let Ok((lhs_ref, rhs_ref)) = self.unapp_ref() {
                lhs_ref.count_matches(limit, matches);
                rhs_ref.count_matches(limit, matches);
            }
       }

    }

    fn reduce_lo(&mut self, limit: usize, count: &mut usize) {
        if limit != 0 && *count == limit {
            return;
        }

        if let App(_) = *self {
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
       }
   }


    /// Applies the S combinator rule to the term at the specified position. 
    /// This function can only be safely executed if the term is SReducible
    /// at the specified position. Increases the count parameter if the operation
    /// was successful.
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
    /// assert_eq!(app(app(app(S, I), I), K).is_reducible(), RuleType::SReducible);
    ///
    /// ```
    pub fn is_reducible(&self) -> RuleType {
        if let Ok((lhs, _)) = self.unapp_ref() {
            match lhs {
                I => return RuleType::IReducible,
                _ => (),
            }
            if let Ok((llhs, _)) = lhs.unapp_ref() {
                match llhs {
                    K => return RuleType::KReducible,
                    App(boxed) => {
                        let (ref lhs, ref _rhs) = **boxed;
                        match lhs {
                            S => return RuleType::SReducible,
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
    use crate::parser::tokenize;

    #[test]
    fn derive_reductions_of_term() {
        let term_str = "S ( S S S ( S S ( K K S ) S ) ) S S ( S ( K S K ) K S )";
        let mut tokens = tokenize(&term_str).unwrap();
        let mut reductions = vec![];
        Term::derive_reductions(&mut tokens, &mut reductions);

        //  S Rule
        //
        //          o                  o
        //         / \               /   \
        //        o   z             o     o
        //       / \        =>     / \   / \
        //      o   y             x   z y   z
        //     / \
        //    s   x
        //
        //  K Rule
        //
        //         o
        //        / \
        //       o   y      =>     x
        //     / \
        //    k  x
        //
        //

        let rt = app(app(app(S,app(app(K,S),K)),K),S);
        let lt = app(app(app(S,app(app(app(S,S),S),app(app(app(S,S),app(app(K,K),S)),S))),S),S);
        let _test_term = app(lt.clone(),rt.clone());

        let x1 = app(app(app(S,S),S),app(app(app(S,S),app(app(K,K),S)),S));
        let y1 = S;
        let z1 = S;
        let red_term1 = app(app(app(x1,z1.clone()),app(y1,z1)),rt.clone());

        let x2 = S;
        let y2 = S;
        let z2 = app(app(app(S,S),app(app(K,K),S)),S);
        let red_term2 = app(app(app(app(S,app(app(x2,z2.clone()),app(y2,z2))),S),S),rt.clone());

        let x3 = S;
        let y3 = app(app(K,K),S);
        let z3 = S;
        let red_term3 = app(app(app(app(S,app(app(app(S,S),S),app(app(x3,z3.clone()),app(y3,z3)))),S),S),rt.clone());

        let x4 = K;
        let _y4 = S;
        let red_term4 = app(app(app(app(S,app(app(app(S,S),S),app(app(app(S,S),x4),S))),S),S),rt.clone());

        let x5 = app(app(K,S),K);
        let y5 = K;
        let z5 = S;
        let red_term5 = app(lt.clone(),app(app(x5,z5.clone()),app(y5,z5)));

        let x6 = S;
        let _y6 = K;
        let red_term6 = app(lt.clone(),app(app(app(S,x6),K),S));

        assert_eq!(reductions.len(), 6);
        assert!(reductions.contains(&red_term1.flat()));
        assert!(reductions.contains(&red_term2.flat()));
        assert!(reductions.contains(&red_term3.flat()));
        assert!(reductions.contains(&red_term4.flat()));
        assert!(reductions.contains(&red_term5.flat()));
        assert!(reductions.contains(&red_term6.flat()));
    }

    #[test]
    fn term_is_marked_as_reducible() {
        assert_eq!(app(app(app(S, I), I), K).is_reducible(), RuleType::SReducible);
        assert_eq!(app(app(K, I), I).is_reducible(), RuleType::KReducible);
        assert_eq!(app(I, K).is_reducible(), RuleType::IReducible);
    }

    #[test]
    fn term_is_reduced_by_srule() {
        let mut test_term = app(app(app(S, I), I), K);
        test_term.apply_srule(&mut 0);
        assert_eq!(test_term, app(app(I, K), app(I, K)));
    }

    #[test]
    fn subterm_is_reduced_by_srule() {
        let mut test_term = app(I,app(app(app(S, I), I), K));
        if let Ok((_remainder, term_to_reduce)) = test_term.unapp_mut() {
            term_to_reduce.apply_srule(&mut 0);
        }
        assert_eq!(test_term, app(I, app(app(I, K), app(I, K))));
    }

    #[test]
    fn term_is_reduced_by_krule() {
        let mut test_term = app(app(K, S), I);
        test_term.apply_krule(&mut 0);
        assert_eq!(test_term, S);
    }

    #[test]
    fn term_is_reduced_by_irule() {
        let mut test_term = app(I, K);
        test_term.apply_irule(&mut 0);
        assert_eq!(test_term,K);
    }

    #[test]
    fn matches_are_counted_correctly() {
        let lterm = app(app(app(S,app(app(app(S,S),S),app(app(app(S,S),app(app(K,K),S)),S))),S),S);
        let rterm = app(app(app(S,app(app(K,S),K)),K),S);
        let test_term = app(lterm, rterm);
        let mut matches = Matches::new();
        let limit = 100;
        test_term.count_matches(limit, &mut matches);
        assert_eq!(matches.sum(), 6);
    }
}
