
/// Directed Acyclic Graph that represents
/// the reduction of expressions.
///

use std::fmt;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::parser::{Token, ParseError};

static NODE_COUNTER: AtomicUsize = AtomicUsize::new(1);

#[derive(Debug, Eq)]
pub struct Node {
    node_id: usize,
    // Term including all sub expressions
    term: Vec<Token>,
    // Number of root expressions
    root_exprs: u32,
}


impl Eq for Sibling {}

impl Node {

    pub fn new(term: Vec<Token>, is_root: bool) -> Self {
        Self {
            node_id: NODE_COUNTER.fetch_add(1, Ordering::Relaxed),
            term: term,
            root_exprs : if is_root { 1 } else { 0 },
        }
    }

    /// Returns true if the terms are equal, false otherwise.
    pub fn term_eq(&self, term: &Vec<Token>) -> bool {
        self.term == *term
    }

    /// Returns the id of the node
    pub fn get_id(&self) -> usize {
        self.node_id
    }

    /// Increase root expressions
    pub fn inc_expr(&mut self) {
        self.root_exprs += 1;
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
        node_string.push_str(", TERM : ");
        for t in &self.term {
            node_string.push_str(&t.to_string());
        }
        node_string.push_str(", #EXPR : ");
        node_string.push_str(&self.root_exprs.to_string());
        write!(
            f,
            "N[{}]",
            node_string
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Sibling {
    LEFT,
    RIGHT,
}

#[derive(Clone, Debug)]
pub struct Pair<T> {
    left: Option<T>,
    right: Option<T>,
}

impl<T> Pair<T> {

    pub fn new(left: Option<T>, right: Option<T>) -> Self {
        Self {
            left: left,
            right: right,
        }
    }

    pub fn get_left(&self) -> &Option<T> {
        &self.left
    }

    pub fn set_left(&mut self, left: T) {
        self.left = Some(left);
    }

    pub fn get_right(&self) -> &Option<T> {
        &self.right
    }

    pub fn set_right(&mut self, right: T) {
        self.right = Some(right);
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
    pub fn get_destination_node_id(&self) -> usize {
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


/// Directed Acyclic Graph which shows how expressions
/// break down into shared subexpressions.
///
#[derive(Debug)]
pub struct Graph {
    // Outgoing edge list (one per node)
    pub edges: HashMap<usize, Pair<Edge>>,
    // Expression Nodes by Id
    pub nodes: HashMap<usize, Node>,
    // Terminal Node I
    pub ti: Node,
    // Terminal Node K
    pub tk: Node,
    // Terminal Node S
    pub ts: Node,
}

impl Graph {

    pub fn new() -> Self {
        Self {
            edges: HashMap::new(),
            nodes: HashMap::new(),
            ti: Node::new(vec![Token::I], false),
            tk: Node::new(vec![Token::K], false),
            ts: Node::new(vec![Token::S], false),
        }
    }

    /// Returns the node id that contains the passed token
    fn contains(&self, term: &Vec<Token>) -> Option<usize> {
        for n in self.nodes.values(){
            if n.term_eq(term) {
                return Some(n.get_id())
            }
        }
        None
    }

    /// Adds an new node with the given term 
    /// and returns its assigned IDs.
    pub fn add_node(&mut self, term: Vec<Token>, is_root: bool) -> usize {
        let node = Node::new(term, is_root); 
        let node_id = node.get_id();
        self.nodes.insert(node_id, node);
        node_id.clone()
    }

    /// Adds a connection to the directed graph from the
    /// node with origin_id to destination_id and assigns the
    /// weight parameter to it.
    fn add_edge(&mut self, origin_id: &usize, destination_id: &usize, sibling: Sibling, weight: f32) {
        if self.nodes.contains_key(&origin_id) && self.nodes.contains_key(&destination_id) {
            let edge = Edge::new(*destination_id, weight);
            if let Some(outgoing_pair) = self.edges.get_mut(&origin_id) {
                // Use destination_id to create an outgoing edge
                if sibling == Sibling::LEFT {
                    outgoing_pair.set_left(edge);
                } else {
                    outgoing_pair.set_right(edge);
                }
            } else {
                let mut new_pair = Pair::new(None,None);
                if sibling == Sibling::LEFT {
                    new_pair.set_left(edge);
                } else {
                    new_pair.set_right(edge);
                }
                self.edges.insert(*origin_id, new_pair);
            }
        }
    }

    /// Adds a term to the graph 
    fn add_term(&mut self, term: &mut Vec<Token>) -> Result<usize, ParseError> {
        if term.is_empty() {
            return Err(ParseError::EmptyExpression);
        }

        if let Some(node_id) = self.contains(term) {
            if let Some(mut node) = self.nodes.get_mut(&node_id) {
                node.inc_expr();
            }
        } else {
            let pos = term.len() -1;
            if let Some(node_id) = self.integrate(term) {
                return Ok(node_id);
            }
        }
        return Err(ParseError::InvalidExpression);
    }

    /// Intgrates a new node into the graph
    ///
    /// # Example
    ///
    /// use ruski::*
    /// use ruski::parser::Token;
    /// 
    /// let graph = Graph::new();
    /// let term = vec![S,S,S,Lparen,S,S,Rparen,S,S];
    /// assert_eq!(graph.integrate(term), Some(5));
    ///
    /// # Errors 
    ///
    /// Return None if the expression is invalid
    fn integrate(&mut self, term: &mut Vec<Token>) -> Option<usize>  {
        // Create new node with connection to first primitive
        // element.
        println!("Integrating {:?}", term);
        let node_id = self.add_node((*term.clone()).to_vec(), true);
        if let Some(token) = term.pop() {
            match token {
                Token::S => self.add_edge(&node_id, &self.ts.get_id(), Sibling::RIGHT, 0.0),
                Token::K => self.add_edge(&node_id, &self.tk.get_id(), Sibling::RIGHT, 0.0),
                Token::I => self.add_edge(&node_id, &self.ti.get_id(), Sibling::RIGHT, 0.0),
                Token::Rparen => {
                    if let Some (lparen_pos) = self.get_sub_idx(term, &(term.len() -1)) {
                        let mut left_term = term.split_off(lparen_pos);
                        if let Some(left_id) = self.integrate(&mut left_term) {
                            if let Some(right_id) = self.integrate(term) {
                                self.add_edge(&node_id, &left_id, Sibling::LEFT, 0.0);
                                self.add_edge(&node_id, &right_id, Sibling::RIGHT, 0.0);
                                return Some(node_id);
                            }
                        }
                    }
                },
                Token::Lparen => (),
            }
            if let Some(node_id_subterm) = self.integrate(term) {
                self.add_edge(&node_id, &node_id_subterm, Sibling::LEFT, 0.0);
            }
            return Some(node_id);
        }
        return None;
    }

    fn get_sub_idx(&self, term: &Vec<Token>, pos: &usize) -> Option<usize> {
        let mut level = 0;
        let mut lpos = *pos;
        while let Some(token) = term.get(lpos) {
            match token {
                Token::Rparen => level += 1,
                Token::Lparen => level -= 1,
                _ => (), 
            }
            if level == 0 {
                return Some(lpos);
            }
            lpos -= 1;
        }
        return None;
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Token::{Lparen, Rparen};
    use crate::term::Term::S;
    use crate::parser::tokenize;

    #[test]
    fn integrate_generates_all_nodes() {
        let mut graph = Graph::new();
        let test_input = "S S S ( S S ) S S"; 
        let mut tokens = tokenize(test_input).unwrap();
        let n1_id = graph.integrate(&mut tokens).unwrap();
        let n1_pair = graph.edges.get(&n1_id).unwrap();
        assert_eq!(n1_pair.get_right().as_ref().unwrap().get_destination_node_id(), graph.ts.get_id());
        let n2_id = n1_pair.get_left().as_ref().unwrap().get_destination_node_id();
        let n2_term = tokenize("S S S ( S S ) S").unwrap();
        assert!(graph.nodes.get(&n2_id).unwrap().term_eq(&n2_term)); 
        
    }

}
