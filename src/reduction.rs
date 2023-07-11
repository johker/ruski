
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

#[derive(Clone, Debug, PartialEq, Eq)]
#[doc(hidden)]
pub enum ReductionToken {
    ///  Starling Symbol
    S,
    /// Kestrel Symbol
    K,
    /// Idiot Symbol
    I,
    /// Match S
    MS,
    /// Match K
    MK,
    /// Match I
    MI,
    /// Left parenthesis
    Lparen,
    /// Right parenthesis
    Rparen,
}

impl fmt::Display for ReductionToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self {
            Token::S => write!(f, "S"),
            Token::K => write!(f, "K"),
            Token::I => write!(f, "I"),
            Token::MS => write!(f, "MS"),
            Token::MK => write!(f, "MK"),
            Token::MI => write!(f, "MI"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
        }

    }
}

impl Term {

    /// Count matches of SKI rule by traversing the tree
    pub fn list_reductions(tokens: &mut Vec<Token>, list: &mut Vec<Vec<Token>>) {
        if let Ok(mut ast) = tokens_to_ast(tokens, &mut 0) {
            let mut marked_matches = vec![];
            let mut reduced_subexpressions = vec![];
            Term::_list_reductions(&mut ast, &mut marked_matches, &mut reduced_subexpressions);
            // TODO: Insert reduced_subexpressions in marked_matches 
            // to create the final list of reductions
        }
    }

    /// @param traversed_ast
    fn _list_reductions(term: &mut Term, marked_matches: &mut Vec<ReductionToken>, reduced_subexpressions: &mut Vec<Vec<Token>>) {
        match *term {
            Term::S => tokens.push(ReductionToken::S),
            Term::K => tokens.push(ReductionToken::K),
            Term::I => tokens.push(ReductionToken::I),
            Term::App(_) => {
                match term.is_reducible() {
                    RuleType::SReducible => {
                        println!("SReducible");
                        marked_matches.push(ReductionToken::MS);
                        term.apply_srule(&mut 0);
                        list.push(term.flat());
                    },
                    RuleType::KReducible => {
                        println!("KReducible");
                        marked_matches.push(ReductionToken::MK);
                        term.apply_krule(&mut 0);
                        list.push(term.flat());
                    },
                    RuleType::IReducible => {
                        println!("IReducible");
                        marked_matches.push(ReductionToken::MI);
                        term.apply_irule(&mut 0);
                        list.push(term.flat());
                    },
                    RuleType::NotReducible => {
                        //TODO combine recursions flat and _list_reductions
                        let (ref lhs, ref rhs) = **boxed;
                        tokens.extend(lhs.flat());
                        let rhs_fl = rhs.flat();
                        let rhs_fl_len = rhs_fl.len();
                        if rhs_fl_len > 1 {
                            tokens.push(Token::Lparen);
                        }
                        tokens.extend(rhs_fl);
                        if rhs_fl_len > 1 {
                            tokens.push(Token::Rparen);
                        }
                    },
                }
            },
            Term::Null => (),
        }
        if let App(_) = *term {
            if let Ok((lhs, rhs)) = term.unapp_mut() {
                Term::_list_reductions(lhs, marked_matches, reduced_subexpressions);
                Term::_list_reductions(rhs, marked_matches, reduced_subexpressions);
            }
        }
    }


    /// Performs a reduction on a `Term` with the specified evaluation `Order` and
    /// an optional limit on the number of reductions (`0` means no limit) and 
    /// returns the reduced `Term`.
    ///
    /// TODO: Add Example
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

    /// Count matches of SKI rule by traversing the tree
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
    fn list_reductions_of_term() {
        let term_str = "S ( S S S ( S S ( K K S ) S ) ) S S ( S ( K S K ) K S )";
        let mut tokens = tokenize(&term_str).unwrap();
        let mut reductions = vec![];
        Term::list_reductions(&mut tokens, &mut reductions);

        let mut i = 0;
        for red in reductions.clone() {
            i+=1;
            println!("{}: {:?}",i, red);
        }

        // TODO: Draw results

        let start = app(app(app(app(S,app(app(app(S,S),S), app(app(app(S,S),app(app(K,K),S)),S))) ,S),S), app(app(app(S,app(app(K,S),K)),K),S));
        println!("S = {:?}", start.flat());
        let term_reds1 = app(app(app(app(app(app(S,S),S),app(app(app(S,S),app(app(K,K),S)),S)),S),app(S,S)),app(app(app(S,app(app(K,S),K)),K),S));
        println!("E = {:?}", term_reds1.flat());
        assert!(reductions.contains(&term_reds1.flat()));
        assert_eq!(reductions.len(), 6);
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
