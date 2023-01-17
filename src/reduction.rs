

pub enum Order {
    // Leftmost outermost
    LMOM, 
    // Leftmost innermost
    LMIM,
}

/// Performs a reduction on a `Term` with the specified evaluation `Order` and
/// an optional limit on the number of reductions (`0` means no limit) and 
/// returns the reduced `Term`.
pub fn reduce(mut term: Term, order: Order, limit: usize) {
}
