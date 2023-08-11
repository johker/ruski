

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
        let x_s = graph.ts.nexpr;
        let x_k = graph.tk.nexpr;
        let x_i = graph.ti.nexpr;
        let mut sum_x = 0;
        for node in graph.nodes.values() {
            sum_x += node.nexpr;
        }
        self.k_cl * (sum_x - x_s - x_k - x_i)
    }

    /// Unnormalized probability that the condensation reaction will occur
    /// in infinitesimal time.
    fn condensation_propensity(&self, graph: &Graph) -> f32 {
        for node in graph.nodes.values() {
            sum_x += node.nexpr;
        }
        self.k_co * sum_x * (sum_x -1) / volume
    }

    /// Unnormalized probability that the reduction reaction will occur
    /// in infinitesimal time.
    fn reduction_propensity(&self, graph: &Graph) -> f32 {
        let mut red_prp = 0;
        for red in graph.reductions {
            // TODO: identify mathes by outgoing edge encoding
        }
        //for state in pool.expressions.values() {
        //    red_prp += k_i * state.matches.i;
        //    red_prp += k_k * state.matches.k;
        //    red_prp += k_s * state.matches.s / self.volume;
        //}
    }

    pub fn run() {
        let mut react_cnt = 0;
        loop {

            // Sample reaction type
            react_cnt += 1;
            if react_cnt > ccc.max_react_cnt {
                break;
            }
        }
    }
}
