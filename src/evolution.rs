use core::num;

use crate::graph::Graph;

#[derive(Default, Debug, PartialEq)]
pub struct Simulation {
    // Reaction rate for cleavage
    k_cl: f32,
    // Reaction rate for condensation
    k_co: f32,
    // Reaction rate for S Reduction
    k_s: f32,
    // Reaction rate for K Reduction
    k_k: f32,
    // Reaction rate for I Reduction
    k_i: f32,
    // Simulated container volume
    volume: f32,
}

impl Simulation {

    /// Unnormalized probability that the cleavage reaction will occur
    /// in infinitesimal time.
    fn cleavage_propensity(&self, graph: &Graph) -> f32 {
        let x_s = graph.ts.nexpr();
        let x_k = graph.tk.nexpr();
        let x_i = graph.ti.nexpr();
        let mut sum_x = 0;
        for node in graph.nodes.values() {
            sum_x += node.nexpr();
        }
        self.k_cl * (sum_x - x_s - x_k - x_i) as f32
    }

    /// Unnormalized probability that the condensation reaction will occur
    /// in infinitesimal time.
    fn condensation_propensity(&self, graph: &Graph) -> f32 {
        let mut sum_x = 0.0;
        for node in graph.nodes.values() {
            sum_x += node.nexpr() as f32;
        }
        self.k_co * sum_x * (sum_x -1.0) / self.volume
    }

    /// Unnormalized probability that the reduction reaction will occur
    /// in infinitesimal time.
    fn reduction_propensity(&self, graph: &Graph) -> f32 {
        let mut red_prp = 0.0;
        for krk in graph.kr.keys() {
            // For each k reduction key: count reductions and multiply with
            // number of molecules of this expressions
            let num_expr = graph.nodes.get(krk).unwrap().nexpr() as f32;
            let num_red = graph.kr.get(krk).unwrap().len() as f32;
            red_prp += self.k_k * num_expr * num_red;
        }
        for irk in graph.ir.keys() {
            // For each i reduction key: count reductions and multiply with
            // number of molecules of this expressions
            let num_expr = graph.nodes.get(irk).unwrap().nexpr() as f32;
            let num_red = graph.ir.get(irk).unwrap().len() as f32;
            red_prp += self.k_i * num_expr * num_red;
        }
        for srk in graph.sr.keys() {
            // For each s reduction key: count reductions and multiply with
            // number of molecules of this expressions
            let num_expr = graph.nodes.get(srk).unwrap().nexpr() as f32;
            let num_red = graph.ir.get(srk).unwrap().len() as f32;
            red_prp += self.k_i * num_expr * num_red;
        }
        //for red in graph.reductions {
        //    // TODO: identify matches by outgoing edge encoding
        //}
        //for state in pool.expressions.values() {
        //    red_prp += k_i * state.matches.i;
        //    red_prp += k_k * state.matches.k;
        //    red_prp += k_s * state.matches.s / self.volume;
        //}
        0.0
    }

    pub fn run() {
        let mut react_cnt = 0;
        loop {

            // Sample reaction type
            react_cnt += 1;
            //if react_cnt > ccc.max_react_cnt {
            //    break;
            //}
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::parser::tokenize;

    #[test]
    fn reduction_propensity_calculated() {

        let test_sim = Simulation {
            volume: 20.0,
            k_i: 0.2,
            k_k: 0.3,
            k_s: 0.5,
            ..Default::default()
        };
        let mut test_graph = Graph::new();
        let test_input = "S ( S S S ( S S ( K K S ) S ) ) S S ( S ( K S K ) K S )";
        let mut tokens = tokenize(test_input).unwrap();
        let _root = test_graph.integrate(&mut tokens, 0).unwrap();

        // The test term has 4 admissible S reductions and 2 admissible K reductions
        // Formula:
        let expected_prop = 4.0*test_sim.k_s + 2.0*test_sim.k_k;


        assert_eq!(test_sim.reduction_propensity(&mut test_graph), expected_prop);
    }


}
