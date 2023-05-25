
/// Directed Acyclic Graph that represents 
/// the reduction of expressions.
///


use std::sync::atomic::{AtomicUsize, Ordering};

static NODE_COUNTER: AtomicUsize = AtomicUsize::new(1);

#[derive(Clone, Debug, Hash, Eq)]
pub struct Node {
    node_id: usize,
    // Term including all sub expressions
    term: Vec<Token>,
    // Number of root expressions
    root_exprs: u32,
}

pub enum Sibling {
    LEFT,
    RIGHT,
}

impl Node {

    pub fn new(term: Term, is_root: bool) -> Self {
        Self {
            node_id: NODE_COUNTER.fetch_add(1, Ordering::Relaxed),
            term: term,
            root_exprs = if is_root { 1 } else { 0 },
        }
    }

    /// Returns true if the terms are equal, false otherwise.
    pub fn term_eq(&self, term: &Term) -> bool {
        self.term == term
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

#[derive(Copy, Clone, Debug)]
pub struct Edges {
    // ID of lefthand side child node
    lnode_id: usize,
    // ID of righthand side child node
    rnode_id: usize,
    // Propensity of lefthand side expression
    lweight: f32,
    // Propensity of righthand side expression
    rweight: f32,
}


#[derive(Clone, Debug, Default)]
pub struct Graph {
    // Outgoing edge list (one per node)
    pub edges: HashMap<usize, Edges>,
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
            ti: Node::new(vec![Token::I], 0),
            tk: Node::new(vec![Token::K], 0),
            ts: Node::new(vec![Token::S], 0),
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
    pub fn add_node(&mut self, term: i32, is_root: bool) -> usize {
        let node = Node::new(term.clone(), is_root); 
        let node_id = node.get_id();
        self.nodes.insert(node_id, node);
        node_id.clone()
    }

    /// Adds a connection to the directed graph from the
    /// node with origin_id to destination_id and assigns the
    /// weight parameter to it.
    pub fn add_edge(&mut self, origin_id: &usize, destination_id: &usize, weight: f32, sibling: &Sibling) {
        if self.nodes.contains_key(&origin_id) && self.nodes.contains_key(&destination_id) {
            if let Some(incoming_edges) = self.edges.get_mut(&destination_id) {
                // Use origin_id to create an incoming edge
                let edge = Edge::new(origin_id, weight);
                if !incoming_edges.contains(&edge){
                        incoming_edges.push(edge);
                }
            } else {
                    let mut new_incoming_edges = vec![]; 
                    new_incoming_edges.push(Edge::new(origin_id, weight));
                    self.edges.insert(destination_id, new_incoming_edges);
            }
        }
    }

    /// Adds a term to the graph 
    fn add_term(&mut self, term: &Vec<Token>) {
        if tokens.is_empty() {
            return Err(ParseError::EmptyExpression);
        }

        if let Some(node_id) = self.contains(term) {
            if let Some(node) = self.nodes.get(node_id) {
                node.inc_expr();
            }
        } else {
            let pos = term.size() -1;
            self.integrate(term, &pos);
        }
    }

    /// Intgrates a new node into the graph
    fn integrate(&mut self, term: &Vec<Token>, pos: &usize, bool side) {
        // Create new node with connection to first primitive
        // element.
        let node_id = self.add_node(term, true);
        while let Some(token) = tokens.get(*pos) {
            println!("Pos = {}, Token = {:?}", pos, token);
            match token {
                Token::S => term = self.add_edge(term, node_id, self.ts.get_id(), Sibling::RIGHT),
                Token::K => term = self.add_edge(term, node_id, self.tk.get_id(), Sibling::RIGHT),
                Token::I => term = self.add_edge(term, node_id, self.ti.get_id(), Sibling::RIGHT),
                Token::Rparen => {
                    *pos -= 1;
                    if let Ok(subterm) = get_ast(tokens, pos) {
                        term = app(term, subterm);
                    }
                }
                Token::Rparen => return Ok(term),
            }
            println!("Term = {:?}", term);
            *pos += 1;
        }

    }

}
