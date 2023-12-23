
/// Graph that represents both subexpressions and the reductions.
/// Subexpressions allow efficient representation of large terms.
/// Reduction paths facilitate the calculation of probabilities in
/// an artificial chemistry.
///

use std::fmt;
use std::fmt::Display;
use std::collections::HashMap;
use std::str::ParseBoolError;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::RuleType;
use crate::parser::{Token, ParseError};
use crate::reduction::Reduction;
use crate::term::Term;

static NODE_COUNTER: AtomicUsize = AtomicUsize::new(1);

#[derive(Debug, Eq)]
pub struct Node {
    node_id: usize,
    // Term including all sub expressions
    term: Vec<Token>,
    // Number of expressions that are not
    // subexpression of another expression
    nexpr: u32,
    // Indicates whether the node has been integrated
    // (if max level has been exceeded)
    integrated: bool,
}


impl Eq for Sibling {}

impl Node {

    pub fn new(term: Vec<Token>, is_root: bool) -> Self {
        Self {
            node_id: NODE_COUNTER.fetch_add(1, Ordering::Relaxed),
            term: term,
            nexpr : if is_root { 1 } else { 0 },
            integrated: true,
        }
    }

    /// Returns true if the terms are equal, false otherwise.
    pub fn term_eq(&self, term: &Vec<Token>) -> bool {
        self.term == *term
    }

    /// Returns the id of the node
    pub fn id(&self) -> usize {
        self.node_id
    }

    /// Increase number of expressions
    pub fn icr_nexpr(&mut self) {
        self.nexpr += 1;
    }


    /// Return number of expressions
    pub fn nexpr(&self) -> u32 {
        self.nexpr
    }

}
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id
    }
}

impl fmt::Display for Node {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut node_string: String = "".to_owned();
        node_string.push_str("ID: ");
        node_string.push_str(&self.node_id.to_string());
        node_string.push_str(", TERM: ");
        for t in &self.term {
            node_string.push_str(&t.to_string());
        }
        node_string.push_str(", #EXPR: ");
        node_string.push_str(&self.nexpr.to_string());
        write!(
            f,
            "N[{}]",
            node_string
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Sibling {
    HEAD,
    ARG,
}

#[derive(Clone, Debug)]
pub struct Pair<T>
where T: Display {
    head: Option<T>,
    arg: Option<T>,
}

impl<T: Display> Pair<T> {

    pub fn new(head: Option<T>, arg: Option<T>) -> Self {
        Self {
            head: head,
            arg: arg,
        }
    }

    pub fn get_head(&self) -> &Option<T> {
        &self.head
    }

    pub fn set_head(&mut self, head: T) {
        self.head = Some(head);
    }

    pub fn get_arg(&self) -> &Option<T> {
        &self.arg
    }

    pub fn set_arg(&mut self, arg: T) {
        self.arg = Some(arg);
    }
}


impl<T: Display> fmt::Display for Pair<T> {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pair_string: String = "".to_owned();
        if let Some(head) = self.get_head() {
            pair_string.push_str(&head.to_string());
        } else {
            pair_string.push_str("None");
        }
        pair_string.push_str(",");
        if let Some(arg) = self.get_arg() {
            pair_string.push_str(&arg.to_string());
        } else {
            pair_string.push_str("None");
        }
        write!(
            f,
            "[{}]",
            pair_string
        )
    }
}

#[derive(Clone, Debug)]
pub struct Edge {
    destination_node_id: usize,
    weight: f32,
}

impl Edge {

    pub fn new(node_id: usize, weight: f32) -> Self {
        Self {
            destination_node_id: node_id,
            weight: weight,
        }
    }
    pub fn dnid(&self) -> usize {
        self.destination_node_id
    }

    pub fn get_weight(&self) -> f32 {
        self.weight
    }

    pub fn set_weight(&mut self, weight : f32) {
        self.weight = weight;
    }
}

impl PartialEq for Edge {
    fn eq(&self, other: &Self) -> bool {
        self.destination_node_id == other.destination_node_id
    }
}

impl Eq for Edge {}


impl fmt::Display for Edge {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut edge_string: String = "".to_owned();
            edge_string.push_str("DNID: ");
            edge_string.push_str(&self.destination_node_id.to_string());
            edge_string.push_str(", WEIGHT: ");
            edge_string.push_str(&self.weight.to_string());
        write!(
            f,
            "[{}]",
            edge_string
        )
    }
}

/// Graph that shows how expressions break down
/// into shared subexpressions and its possible reductions.
///
#[derive(Debug)]
pub struct Graph {
    // Subexpressions: Pair of Edges representing Head & Arg
    pub subexpressions: HashMap<usize, Pair<Edge>>,
    // Reductions: List of Edges representing possible reductions
    // TODO: encode subexpression by weight: ARG=1.0, HEAD=2.0, KRED=3.0, ...
    pub sr: HashMap<usize, Vec<Edge>>,
    pub kr: HashMap<usize, Vec<Edge>>,
    pub ir: HashMap<usize, Vec<Edge>>,
    // Expression Nodes by Id
    pub nodes: HashMap<usize, Node>,
    // Terminal Node I
    pub ti: Node,
    // Terminal Node K
    pub tk: Node,
    // Terminal Node S
    pub ts: Node,
}

impl fmt::Display for Graph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut node_string: String = "".to_owned();
        node_string.push_str("\n");
        node_string.push_str(&self.ts.to_string());
        node_string.push_str("\n");
        node_string.push_str(&self.tk.to_string());
        node_string.push_str("\n");
        node_string.push_str(&self.ti.to_string());
        for (_, node) in self.nodes.iter() {
            node_string.push_str("\n");
            node_string.push_str(&node.to_string());
            node_string.push_str(", ");
        }
        node_string = node_string.trim_end_matches(", ").to_string();
        let mut edge_string: String = "".to_owned();
        for (node, pair) in self.subexpressions.iter() {
            edge_string.push_str("\n");
            edge_string.push_str("P[");
            edge_string.push_str(&node.to_string());
            edge_string.push_str(" <= ");
            edge_string.push_str(&pair.to_string());
            edge_string.push_str("]");
        }
        edge_string = edge_string.trim_end_matches(", ").to_string();
        write!(
            f,
            "\nNODES({}): {}\nEDGES: {}",
            &self.node_size(),
            node_string,
            edge_string,
        )
    }
}

impl Graph {

    pub fn new() -> Self {
        Self {
            subexpressions: HashMap::new(),
            sr: HashMap::new(),
            kr: HashMap::new(),
            ir: HashMap::new(),
            nodes: HashMap::new(),
            ti: Node::new(vec![Token::I], false),
            tk: Node::new(vec![Token::K], false),
            ts: Node::new(vec![Token::S], false),
        }
    }

    /// Returns the id of the node containing the term or None
    /// if no term can be found
    pub fn get_term_id(&self, term: &Vec<Token>) -> Option<usize> {
        self.nodes.iter().find_map(|(key, val)| if val.term_eq(term) { Some(*key) } else { None })

    }

    /// Returns the id of the node containing the term or None
    /// if no term can be found. Includes the elementary terms S,K and I.
    pub fn contains(&self, term: &Vec<Token>) -> Option<usize> {
        if term.len() == 1 {
           if self.ts.term_eq(term) {
                return Some(self.ts.id())
           }
           if self.tk.term_eq(term) {
                return Some(self.tk.id())
           }
           if self.ti.term_eq(term) {
                return Some(self.ti.id())
           }
        }
       return self.get_term_id(term);
    }

    /// Checks if id is part of the graph
    fn contains_id(&self, id: &usize) -> bool {
        self.nodes.contains_key(id) || self.ts.id() == *id || self.tk.id() == *id || self.ti.id() == *id
    }

    /// Returns the number of nodes
    pub fn node_size(&self) -> usize {
        self.nodes.len()
    }

    /// Adds an new node with the given term
    /// and returns its assigned IDs.
    pub fn add_node(&mut self, term: Vec<Token>, is_root: bool) -> usize {
        let node = Node::new(term, is_root);
        let node_id = node.id();
        self.nodes.insert(node_id, node);
        node_id.clone()
    }

    /// Adds an edge to the reductions according to the rule type of the reduction.
    pub fn add_reduction_edge(&mut self, reduction: &Reduction, origin_id: &usize, destination_id: &usize) {
        let new_reduction_edge = Edge::new(*destination_id, 0.0);
        match reduction.rule() {
            RuleType::SReducible => {
                if let Some(s_rdcs) = self.sr.get_mut(origin_id) {
                    s_rdcs.push(new_reduction_edge);
                }
            },
            RuleType::KReducible => {
                if let Some(k_rdcs) = self.sr.get_mut(origin_id) {
                    k_rdcs.push(new_reduction_edge);

                }
            },
            RuleType::IReducible => {
                if let Some(i_rdcs) = self.sr.get_mut(origin_id) {
                    i_rdcs.push(new_reduction_edge);

                }
            },
            RuleType::NotReducible => (),
        }

    }

    /// Adds an edge to a subexpression from the nodes with origin_id to destination_id and assigns the
    /// weight parameter to it.
    pub fn add_subexpr_edge(&mut self, origin_id: &usize, destination_id: &usize, sibling: Sibling, weight: f32) {
        if self.contains_id(&origin_id) && self.contains_id(&destination_id) {
            let edge = Edge::new(*destination_id, weight);
            if let Some(outgoing_pair) = self.subexpressions.get_mut(&origin_id) {
                // Use destination_id to create an outgoing edge
                if sibling == Sibling::HEAD {
                    outgoing_pair.set_head(edge);
                } else {
                    outgoing_pair.set_arg(edge);
                }
            } else {
                let mut new_pair = Pair::new(None,None);
                if sibling == Sibling::HEAD {
                    new_pair.set_head(edge);
                } else {
                    new_pair.set_arg(edge);
                }
                self.subexpressions.insert(*origin_id, new_pair);
            }
        }
    }

    /// Adds a term to the graph consuming it in the process.
    pub fn add_term(&mut self, mut term: Vec<Token>) -> Result<usize, ParseError> {
        if term.is_empty() {
            return Err(ParseError::EmptyExpression);
        }
        if let Some(node_id) = self.contains(&mut term) {
            if let Some(node) = self.nodes.get_mut(&node_id) {
                node.icr_nexpr();
            }
            return Ok(node_id)
        } else {
            if let Some(node_id) = self.integrate(&mut term, 0) {
                if let Some(node) = self.nodes.get_mut(&node_id) {
                    node.icr_nexpr();
                }
                return Ok(node_id);
            }
        }
        return Err(ParseError::InvalidExpression);
    }


    /// Intgrates a new node into the graph
    /// Returns the ID of the new node or a provisional id of
    /// 0 if the integration limit is reached.
    ///
    /// # Example
    ///
    /// use ruski::*
    /// use ruski::parser::Token;
    ///
    /// let graph = Graph::new();
    /// let term = vec![S,S,S,Lparen,S,S,Rparen,S,S];
    /// assert_eq!(graph.integrate(term), Some(5), 0);
    ///
    /// # Errors
    ///
    /// Return None if the expression is invalid
    pub fn integrate(&mut self, term: &mut Vec<Token>, level: usize) -> Option<usize>  {
        // Check if term exists
        if let Some(node_id) = self.contains(term) {
            println!("Term already exists");
            // TODO: Update reduction weights?
            return Some(node_id);
        }

        // Create new node with connection to first primitive element.
        let node_id = self.add_node((*term.clone()).to_vec(), false);
        // TODO: Make level a configuration variable
        if level > 10 {
            return Some(node_id);
        }

        let mut reductions = vec![];
        // Breakdown potential reductions of the term
        Term::derive_reductions(term, &mut reductions);
        println!("Found {} potential reductions", reductions.len());
        for mut r in reductions {
            // For each potential reduction: call integrate
            if let Some(reduced_term_id) = self.integrate(r.term(), level+1) {
                self.add_reduction_edge(&r, &node_id, &reduced_term_id);
            }
            // TODO: Indicate if reductions have been derived already (node variable)
        }
        if let Some(token) = term.pop() {
            match token {
                Token::S => self.add_subexpr_edge(&node_id, &self.ts.id(), Sibling::ARG, 0.0),
                Token::K => self.add_subexpr_edge(&node_id, &self.tk.id(), Sibling::ARG, 0.0),
                Token::I => self.add_subexpr_edge(&node_id, &self.ti.id(), Sibling::ARG, 0.0),
                Token::Rparen => {
                    if let Some (lparen_pos) = Graph::lpidx(term) {
                        let mut arg_term = term.split_off(lparen_pos);
                        arg_term = arg_term.split_off(1); // Remove left parenthesis
                        if let Some(arg_id) = self.integrate(&mut arg_term, level) {
                            if let Some(head_id) = self.integrate(term, level) {
                                self.add_subexpr_edge(&node_id, &head_id, Sibling::HEAD, 0.0);
                                self.add_subexpr_edge(&node_id, &arg_id, Sibling::ARG, 0.0);
                                return Some(node_id);
                            }
                        }
                    }
                },
                Token::Lparen => (),
            }
            if let Some(node_id_subterm) = self.integrate(term, level) {
                self.add_subexpr_edge(&node_id, &node_id_subterm, Sibling::HEAD, 0.0);
            }
            return Some(node_id);
        }
        return None;
    }

    /// Returns the first left parenthesis index that is not preceeded by
    /// a right parenthesis going from right to left of the passed token
    /// sequence or None if no left parenthesis can be found.
    pub fn lpidx(term: &Vec<Token>) -> Option<usize> {
        let mut level = 1;
        let mut lpos = term.len() -1;
        while let Some(token) = term.get(lpos) {
            match token {
                Token::Rparen => level += 1,
                Token::Lparen => level -= 1,
                _ => (),
            }
            if level == 0 {
                return Some(lpos);
            }
            if lpos == 0 {
                return None;
            }
            lpos -= 1;
        }
        return None;
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tokenize;

    #[test]
    fn lpidx_finds_head_parenthesis_position() {
        let test_input = "S S S ( S S";
        let tokens = tokenize(test_input).unwrap();
        assert_eq!(Graph::lpidx(&tokens), Some(3));

        let test_input = "S S S ( S ( S K ) S";
        let tokens = tokenize(test_input).unwrap();
        assert_eq!(Graph::lpidx(&tokens), Some(3));

        let test_input = "S S S S ( S K ) S";
        let tokens = tokenize(test_input).unwrap();
        assert_eq!(Graph::lpidx(&tokens), None);
    }

    #[test]
    fn add_term_increases_count_when_called_twice () {
        let mut graph = Graph::new();
        let tokens = tokenize(&"S S S ( S S ) S S".to_string()).unwrap();
        let root = graph.add_term(tokens).unwrap();
        assert_eq!(graph.nodes.get(&root).unwrap().nexpr(), 1);
        let tokens = tokenize(&"S S S ( S S ) S S".to_string()).unwrap();
        let root = graph.add_term(tokens).unwrap();
        assert_eq!(graph.nodes.get(&root).unwrap().nexpr(), 2);
    }

    #[test]
    fn integrate_generates_all_nodes() {
        let mut graph = Graph::new();
        let test_input = "S S S ( S S ) S S";
        let mut tokens = tokenize(test_input).unwrap();
        let root = graph.integrate(&mut tokens, 0).unwrap();

        let n0 = graph.contains(&tokenize("S S S ( S S ) S S").unwrap()).unwrap();
        let n1 = graph.contains(&tokenize("S S S ( S S ) S").unwrap()).unwrap();
        let n2 = graph.contains(&tokenize("S S S ( S S )").unwrap()).unwrap();
        let n3 = graph.contains(&tokenize("S S S").unwrap()).unwrap();
        let n4 = graph.contains(&tokenize("S S").unwrap()).unwrap();


        assert_eq!(root, n0);
        assert_eq!(graph.subexpressions.get(&n0).unwrap().get_head().as_ref().unwrap().dnid(), n1);
        assert_eq!(graph.subexpressions.get(&n0).unwrap().get_arg().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.subexpressions.get(&n1).unwrap().get_head().as_ref().unwrap().dnid(), n2);
        assert_eq!(graph.subexpressions.get(&n1).unwrap().get_arg().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.subexpressions.get(&n2).unwrap().get_head().as_ref().unwrap().dnid(), n3);
        assert_eq!(graph.subexpressions.get(&n2).unwrap().get_arg().as_ref().unwrap().dnid(), n4);

        assert_eq!(graph.subexpressions.get(&n3).unwrap().get_head().as_ref().unwrap().dnid(), n4);
        assert_eq!(graph.subexpressions.get(&n3).unwrap().get_arg().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.subexpressions.get(&n4).unwrap().get_head().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.subexpressions.get(&n4).unwrap().get_arg().as_ref().unwrap().dnid(), graph.ts.id());

        let mut graph = Graph::new();
        let test_input = "S ( S ( S S ) ( S ( S ( S S ) S ) S ) ) ( S ( S S ) S ( S ( S S ) ( S ( S ( S S ) S ) S ) ) ( S ( S ( S S ) ( S ( S ( S S ) S ) S ) ) ) )";
        let mut tokens = tokenize(test_input).unwrap();
        let root = graph.integrate(&mut tokens, 0).unwrap();

        let n0 = graph.contains(&tokenize("S ( S ( S S ) ( S ( S ( S S ) S ) S ) ) ( S ( S S ) S ( S ( S S ) ( S ( S ( S S ) S ) S ) ) ( S ( S ( S S ) ( S ( S ( S S ) S ) S ) ) ) )").unwrap()).unwrap();
        let n1 = graph.contains(&tokenize("S ( S S ) S ( S ( S S ) ( S ( S ( S S ) S ) S ) ) ( S ( S ( S S ) ( S ( S ( S S ) S ) S ) ) )").unwrap()).unwrap();
        let n2 = graph.contains(&tokenize("S ( S S ) S ( S ( S S ) ( S ( S ( S S ) S ) S ) )").unwrap()).unwrap();
        let n3 = graph.contains(&tokenize("S ( S ( S S ) ( S ( S ( S S ) S ) S ) )").unwrap()).unwrap();
        let n4 = graph.contains(&tokenize("S ( S S ) ( S ( S ( S S ) S ) S )").unwrap()).unwrap();
        let n5 = graph.contains(&tokenize("S ( S ( S S ) S ) S").unwrap()).unwrap();
        let n6 = graph.contains(&tokenize("S ( S ( S S ) S )").unwrap()).unwrap();
        let n7 = graph.contains(&tokenize("S ( S S ) S").unwrap()).unwrap();
        let n8 = graph.contains(&tokenize("S ( S S )").unwrap()).unwrap();
        let n9 = graph.contains(&tokenize("S S").unwrap()).unwrap();


        assert_eq!(root, n0);
        assert_eq!(graph.subexpressions.get(&n0).unwrap().get_head().as_ref().unwrap().dnid(), n3);
        assert_eq!(graph.subexpressions.get(&n0).unwrap().get_arg().as_ref().unwrap().dnid(), n1);

        assert_eq!(graph.subexpressions.get(&n1).unwrap().get_head().as_ref().unwrap().dnid(), n2);
        assert_eq!(graph.subexpressions.get(&n1).unwrap().get_arg().as_ref().unwrap().dnid(), n3);

        assert_eq!(graph.subexpressions.get(&n2).unwrap().get_head().as_ref().unwrap().dnid(), n7);
        assert_eq!(graph.subexpressions.get(&n2).unwrap().get_arg().as_ref().unwrap().dnid(), n4);

        assert_eq!(graph.subexpressions.get(&n3).unwrap().get_head().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.subexpressions.get(&n3).unwrap().get_arg().as_ref().unwrap().dnid(), n4);

        assert_eq!(graph.subexpressions.get(&n4).unwrap().get_head().as_ref().unwrap().dnid(), n8);
        assert_eq!(graph.subexpressions.get(&n4).unwrap().get_arg().as_ref().unwrap().dnid(), n5);

        assert_eq!(graph.subexpressions.get(&n5).unwrap().get_head().as_ref().unwrap().dnid(), n6);
        assert_eq!(graph.subexpressions.get(&n5).unwrap().get_arg().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.subexpressions.get(&n6).unwrap().get_head().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.subexpressions.get(&n6).unwrap().get_arg().as_ref().unwrap().dnid(), n7);

        assert_eq!(graph.subexpressions.get(&n7).unwrap().get_head().as_ref().unwrap().dnid(), n8);
        assert_eq!(graph.subexpressions.get(&n7).unwrap().get_arg().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.subexpressions.get(&n8).unwrap().get_head().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.subexpressions.get(&n8).unwrap().get_arg().as_ref().unwrap().dnid(), n9);

        assert_eq!(graph.subexpressions.get(&n9).unwrap().get_head().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.subexpressions.get(&n9).unwrap().get_arg().as_ref().unwrap().dnid(), graph.ts.id());
    }
}
