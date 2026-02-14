//! Set lookup arguments
use super::*;
use crate::util::ns::Namespace;
use log::debug;

use std::convert::TryInto;

/// Do set lookup arguments
pub fn apply(c: &mut Computation) {
    let mut asserted_map_contains_keys = TermSet::default();

    // Collect map operations from all outputs, not just the first one
    // This handles circuits with multiple constraints/outputs
    for output in &c.outputs {
        extras::collect_asserted_ops(
            output,
            &|o: &Op| o == &Op::ExtOp(ExtOp::MapContainsKey),
            &mut asserted_map_contains_keys,
        );
    }

    if asserted_map_contains_keys.is_empty() {
        return;
    }
    let mut maps_to_keys: TermMap<Vec<Term>> = TermMap::default();
    for containment in &asserted_map_contains_keys {
        let [map, key]: &[Term; 2] = containment.cs().try_into().unwrap();
        maps_to_keys
            .entry(map.clone())
            .or_default()
            .push(key.clone());
    }
    let ns = Namespace::new();
    let mut to_assert = Vec::new();
    for (i, (map, keys)) in maps_to_keys.into_iter().enumerate() {
        assert!(
            map.is_const(),
            "set membership only supported for constant sets"
        );
        debug!(
            "set membership argument; set size {}, key count {}",
            map.as_map_opt().unwrap().map.len(),
            keys.len()
        );
        let haystack: Vec<Term> = map
            .as_map_opt()
            .unwrap()
            .map
            .keys()
            .cloned()
            .map(const_)
            .collect();
        to_assert.push(super::checker::rom::lookup(
            c,
            ns.subspace(format!("setmem{}", i)),
            haystack,
            keys,
            None,
        ));
    }
    let num_lookup_constraints = to_assert.len();
    to_assert.extend(c.outputs.iter().cloned());

    let subs: TermMap<Term> = asserted_map_contains_keys
        .into_iter()
        .map(|c| (c, bool_lit(true)))
        .collect();

    let multi_output_separate = cfg!(feature = "datan") && c.outputs.len() > 1;
    if multi_output_separate {
        // datan: keep separate outputs for multi-round support
        c.outputs = c.outputs
            .iter()
            .map(|out| extras::substitute(out, subs.clone()))
            .collect();
        for constraint in &to_assert[..num_lookup_constraints] {
            c.outputs
                .push(extras::substitute(constraint, subs.clone()));
        }
    } else {
        // Combine all into single output (bellman-compatible, 1 round)
        c.outputs = vec![extras::substitute(&term(AND, to_assert), subs)];
    }
}
