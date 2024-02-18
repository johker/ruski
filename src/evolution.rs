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
        self.k_co * sum_x * (sum_x - 1.0) / self.volume
    }

    /// Unnormalized probability that the reduction reaction will occur
    /// in infinitesimal time.
    fn reduction_propensity(&self, graph: &Graph) -> f32 {
        // For each s reduction key: count reductions and multiply with
        // number of molecules of this expressions
        let mut red_prp = 0.0;
        for k_reduction_key in graph.kr.keys() {
            // For each k reduction key: count reductions and multiply with
            // number of molecules of this expressions
            let num_k_expr = graph.nodes.get(k_reduction_key).unwrap().nexpr() as f32;
            let num_k_red = graph.kr.get(k_reduction_key).unwrap().len() as f32;
            red_prp += self.k_k * num_k_expr * num_k_red;
        }
        for i_reduction_key in graph.ir.keys() {
            // For each i reduction key: count reductions and multiply with
            // number of molecules of this expressions
            let num_i_expr = graph.nodes.get(i_reduction_key).unwrap().nexpr() as f32;
            let num_i_red = graph.ir.get(i_reduction_key).unwrap().len() as f32;
            red_prp += self.k_i * num_i_expr * num_i_red;
        }

        for substrate_node_id in graph.sr.keys() {
            let mut num_s_substrate_expr = 0.0;
            let mut num_s_reactant_expr = 0.0;
            num_s_substrate_expr += graph.nodes.get(substrate_node_id).unwrap().nexpr() as f32;
            if let Some(s_reaction_pairs) = graph.sr.get(substrate_node_id) {
                for s_reaction_pair in s_reaction_pairs {
                    if let Some(reactant_edge) = s_reaction_pair.reactant() {
                        let destination_node_id = reactant_edge.destination_node_id();
                        num_s_reactant_expr +=
                            graph.nodes.get(&destination_node_id).unwrap().nexpr() as f32;
                    }
                }
            }
            red_prp += self.k_s * num_s_substrate_expr * num_s_reactant_expr / self.volume;
        }
        red_prp
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
    use crate::{graph, parser::tokenize};

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
        let tokens = tokenize(test_input).unwrap();
        let _root = test_graph.add_term(tokens).unwrap();

        // The test term has 4 admissible S reductions and 2 admissible K reductions
        // Formula:
        let expected_prop = 4.0 * test_sim.k_s + 2.0 * test_sim.k_k;

        assert_eq!(
            test_sim.reduction_propensity(&mut test_graph),
            expected_prop
        );
    }
}
