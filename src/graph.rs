
/// Directed Acyclic Graph that represents
/// the reduction of expressions.
///

use std::fmt;
use std::fmt::Display;
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
    pub fn id(&self) -> usize {
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
        node_string.push_str(", TERM: ");
        for t in &self.term {
            node_string.push_str(&t.to_string());
        }
        node_string.push_str(", #EXPR: ");
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
pub struct Pair<T> 
where T: Display {
    left: Option<T>,
    right: Option<T>,
}

impl<T: Display> Pair<T> {

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


impl<T: Display> fmt::Display for Pair<T> {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pair_string: String = "".to_owned();
        if let Some(left) = self.get_left() {
            pair_string.push_str(&left.to_string());
        } else {
            pair_string.push_str("None");
        }
        pair_string.push_str(",");
        if let Some(right) = self.get_right() {
            pair_string.push_str(&right.to_string());
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
        for (node, pair) in self.edges.iter() {
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
            edges: HashMap::new(),
            nodes: HashMap::new(),
            ti: Node::new(vec![Token::I], false),
            tk: Node::new(vec![Token::K], false),
            ts: Node::new(vec![Token::S], false),
        }
    }

    /// Returns the id of the node containing the term or None 
    /// if no term can be found
    pub fn get_term_id(&self, term: &Vec<Token>) -> Option<usize> {
        self.nodes.iter().find_map(|(key, val)| if val.term == *term { Some(*key) } else { None })

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

    /// Adds a connection to the directed graph from the
    /// node with origin_id to destination_id and assigns the
    /// weight parameter to it.
    pub fn add_edge(&mut self, origin_id: &usize, destination_id: &usize, sibling: Sibling, weight: f32) {
        if self.contains_id(&origin_id) && self.contains_id(&destination_id) {
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
    pub fn add_term(&mut self, term: &mut Vec<Token>) -> Result<usize, ParseError> {
        if term.is_empty() {
            return Err(ParseError::EmptyExpression);
        }

        if let Some(node_id) = self.contains(term) {
            if let Some(node) = self.nodes.get_mut(&node_id) {
                node.inc_expr();
            }
            return Ok(node_id)
        } else {
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
    pub fn integrate(&mut self, term: &mut Vec<Token>) -> Option<usize>  {
        // Check if term exists
        if let Some(node_id) = self.contains(term) {
            return Some(node_id);
        }
        // Create new node with connection to first primitive
        // element.
        let node_id = self.add_node((*term.clone()).to_vec(), false);
        if let Some(token) = term.pop() {
            match token {
                Token::S => self.add_edge(&node_id, &self.ts.id(), Sibling::RIGHT, 0.0),
                Token::K => self.add_edge(&node_id, &self.tk.id(), Sibling::RIGHT, 0.0),
                Token::I => self.add_edge(&node_id, &self.ti.id(), Sibling::RIGHT, 0.0),
                Token::Rparen => {
                    if let Some (lparen_pos) = Graph::lpidx(term) {
                        let mut left_term = term.split_off(lparen_pos);
                        left_term = left_term.split_off(1); // Remove left parenthesis
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

    /// Returns the first left parenthesis index that is not preceeded by
    /// a right parenthesis going from right to left of the passed token
    /// sequence or None j.
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
    fn lpidx_finds_left_parenthesis_position() {
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
    fn integrate_generates_all_nodes() {
        let mut graph = Graph::new();
        let test_input = "S S S ( S S ) S S"; 
        let mut tokens = tokenize(test_input).unwrap();
        let root = graph.integrate(&mut tokens).unwrap();

        let n0 = graph.contains(&tokenize("S S S ( S S ) S S").unwrap()).unwrap();
        let n1 = graph.contains(&tokenize("S S S ( S S ) S").unwrap()).unwrap();
        let n2 = graph.contains(&tokenize("S S S ( S S )").unwrap()).unwrap();
        let n3 = graph.contains(&tokenize("S S S").unwrap()).unwrap();
        let n4 = graph.contains(&tokenize("S S").unwrap()).unwrap();


        assert_eq!(root, n0);
        assert_eq!(graph.edges.get(&n0).unwrap().get_left().as_ref().unwrap().dnid(), n1);
        assert_eq!(graph.edges.get(&n0).unwrap().get_right().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.edges.get(&n1).unwrap().get_left().as_ref().unwrap().dnid(), n2);
        assert_eq!(graph.edges.get(&n1).unwrap().get_right().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.edges.get(&n2).unwrap().get_left().as_ref().unwrap().dnid(), n4);
        assert_eq!(graph.edges.get(&n2).unwrap().get_right().as_ref().unwrap().dnid(), n3);

        assert_eq!(graph.edges.get(&n3).unwrap().get_left().as_ref().unwrap().dnid(), n4);
        assert_eq!(graph.edges.get(&n3).unwrap().get_right().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.edges.get(&n4).unwrap().get_left().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.edges.get(&n4).unwrap().get_right().as_ref().unwrap().dnid(), graph.ts.id());

        let mut graph = Graph::new();
        let n0 = graph.contains(&tokenize("S ( S ( S S ) S ( S ( S S ) S ) S ) ( S ( S S ) S ( S ( S S ) S ( S ( S S ) S ) S ) ( S ( S ( S S ) S ( S ( S S ) S ) S ) ) )").unwrap()).unwrap();
        let n1 = graph.contains(&tokenize("S ( S S ) S ( S ( S S ) S ( S ( S S ) S ) S ) ( S ( S ( S S ) S ( S ( S S ) S ) S ) )").unwrap()).unwrap();
        let n2 = graph.contains(&tokenize("S ( S S ) S ( S ( S S ) S ( S ( S S ) S ) S )").unwrap()).unwrap();
        let n3 = graph.contains(&tokenize("S ( S ( S S ) S ( S ( S S ) S ) S )").unwrap()).unwrap();
        let n4 = graph.contains(&tokenize("S ( S S ) S ( S ( S S ) S ) S").unwrap()).unwrap();
        let n5 = graph.contains(&tokenize("S ( S ( S S ) S ) S").unwrap()).unwrap();
        let n6 = graph.contains(&tokenize("S ( S ( S S ) S )").unwrap()).unwrap();
        let n7 = graph.contains(&tokenize("S ( S S ) S").unwrap()).unwrap();
        let n8 = graph.contains(&tokenize("S ( S S )").unwrap()).unwrap();
        let n9 = graph.contains(&tokenize("S S").unwrap()).unwrap();

        assert_eq!(graph.edges.get(&n0).unwrap().get_left().as_ref().unwrap().dnid(), n3);
        assert_eq!(graph.edges.get(&n0).unwrap().get_right().as_ref().unwrap().dnid(), n1);

        assert_eq!(graph.edges.get(&n1).unwrap().get_left().as_ref().unwrap().dnid(), n2);
        assert_eq!(graph.edges.get(&n1).unwrap().get_right().as_ref().unwrap().dnid(), n3);

        assert_eq!(graph.edges.get(&n2).unwrap().get_left().as_ref().unwrap().dnid(), n7);
        assert_eq!(graph.edges.get(&n2).unwrap().get_right().as_ref().unwrap().dnid(), n4);

        assert_eq!(graph.edges.get(&n3).unwrap().get_left().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.edges.get(&n3).unwrap().get_right().as_ref().unwrap().dnid(), n4);

        assert_eq!(graph.edges.get(&n4).unwrap().get_left().as_ref().unwrap().dnid(), n8);
        assert_eq!(graph.edges.get(&n4).unwrap().get_right().as_ref().unwrap().dnid(), n5);

        assert_eq!(graph.edges.get(&n5).unwrap().get_left().as_ref().unwrap().dnid(), n6);
        assert_eq!(graph.edges.get(&n5).unwrap().get_right().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.edges.get(&n6).unwrap().get_left().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.edges.get(&n6).unwrap().get_right().as_ref().unwrap().dnid(), n7);

        assert_eq!(graph.edges.get(&n7).unwrap().get_left().as_ref().unwrap().dnid(), n8);
        assert_eq!(graph.edges.get(&n7).unwrap().get_right().as_ref().unwrap().dnid(), graph.ts.id());

        assert_eq!(graph.edges.get(&n8).unwrap().get_left().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.edges.get(&n8).unwrap().get_right().as_ref().unwrap().dnid(), n9);

        assert_eq!(graph.edges.get(&n9).unwrap().get_left().as_ref().unwrap().dnid(), graph.ts.id());
        assert_eq!(graph.edges.get(&n9).unwrap().get_right().as_ref().unwrap().dnid(), graph.ts.id());
    }
}
