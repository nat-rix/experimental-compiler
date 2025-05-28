use super::{
    BasicBlockTree, RegSet,
    instr::{Label, Reg},
};

#[derive(Debug, Clone, Default)]
pub struct Vertex {
    neighbors: RegSet,
    pub color: Option<usize>,
    weight: usize,
}

#[derive(Debug, Clone, Default)]
pub struct Graph {
    pub vertices: Vec<Vertex>,
    ordering: Vec<Reg>,
}

impl Graph {
    fn extend_to_vertex(&mut self, a: Reg) {
        if a.0 >= self.vertices.len() {
            self.vertices.resize(a.0 + 1, Default::default());
        }
    }

    fn insert_single_sided_edge(&mut self, a: Reg, b: Reg) {
        self.extend_to_vertex(a);
        self.vertices[a.0].neighbors.insert(b);
    }

    pub fn insert_edge(&mut self, a: Reg, b: Reg) {
        self.extend_to_vertex(core::cmp::max(a, b));
        self.insert_single_sided_edge(a, b);
        self.insert_single_sided_edge(b, a);
    }

    pub fn colorize_conflicting(&mut self, reg: Reg, color: usize) -> bool {
        self.extend_to_vertex(reg);
        let entry = &mut self.vertices[reg.0];
        if entry.color.is_some_and(|c| c != color) {
            return true;
        }
        entry.color = Some(color);
        false
    }
}

fn create_liveness_information(tree: &mut BasicBlockTree) {
    let mut work_set: Vec<_> = tree
        .blocks
        .iter()
        .enumerate()
        .filter(|(_, block)| block.tail.is_some_and(|tail| tail.is_final()))
        .map(|(i, _)| Label(i))
        .collect();
    for block in &mut tree.blocks {
        block.annotations = vec![Default::default(); block.instr_count()];
    }
    while let Some(label) = work_set.pop() {
        let block = tree.get_mut(label);
        let mut live = block.live_out.clone();

        let instr_count = block.instr_count();
        for i in (0..instr_count).rev() {
            let (dsts, srcs) = block.get_reg_split(i);
            for dst in dsts {
                live.remove(*dst);
            }
            for src in srcs {
                live.insert(*src);
            }
            block.annotations[i].live_vars = live.clone();
        }

        let xref_count = block.xrefs.len();
        for xref_index in 0..xref_count {
            let xref = tree.get_mut(label).xrefs[xref_index];
            let xref_block = tree.get_mut(xref);
            if xref_block.live_out.union(&live) {
                work_set.push(xref);
            }
        }
    }
}

fn create_graph(tree: &mut BasicBlockTree) {
    for block in &tree.blocks {
        for ann in &block.annotations {
            let live = &ann.live_vars.items;
            for (i, j) in (0..live.len()).flat_map(|i| (i + 1..live.len()).map(move |j| (i, j))) {
                tree.inference.insert_edge(live[i], live[j]);
            }
        }
    }
}

fn create_simplicial_precoloring(tree: &mut BasicBlockTree, n: usize) -> usize {
    tree.inference.ordering = (0..n).map(Reg).collect();
    let mut k = 0;
    for reg in 0..n {
        if tree.inference.vertices[reg].color.is_some() {
            tree.inference.ordering.swap(reg, k);
            k += 1;
        }
    }
    k
}

fn create_simplicial_elimination_ordering(tree: &mut BasicBlockTree) {
    let n = tree.inference.vertices.len();
    let k = create_simplicial_precoloring(tree, n);
    for i in k..n {
        let (j, &r) = tree.inference.ordering[k..n]
            .iter()
            .enumerate()
            .max_by_key(|(_, r)| tree.inference.vertices[r.0].weight)
            .unwrap();
        tree.inference.ordering.swap(i, k + j);
        for ui in 0..tree.inference.vertices[r.0].neighbors.items.len() {
            let u = tree.inference.vertices[r.0].neighbors.items[ui];
            if !tree.inference.ordering[i + 1..].contains(&u) {
                continue;
            }
            tree.inference.vertices[u.0].weight += 1;
        }
    }
}

fn create_coloring(tree: &mut BasicBlockTree) {
    for vr in &tree.inference.ordering {
        let mut c = 0;
        let v = &tree.inference.vertices[vr.0];
        if v.color.is_some() {
            continue;
        }
        while v
            .neighbors
            .items
            .iter()
            .any(|n| tree.inference.vertices[n.0].color == Some(c))
        {
            c += 1;
        }
        tree.inference.vertices[vr.0].color = Some(c);
    }
}

pub fn analysis(tree: &mut BasicBlockTree) {
    create_liveness_information(tree);
    create_graph(tree);
    create_simplicial_elimination_ordering(tree);
    create_coloring(tree);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_simplicial_precoloring() {
        let mut tree = BasicBlockTree::new_fun();
        let d = || Default::default();
        let c = || Vertex {
            color: Some(0),
            ..Default::default()
        };
        tree.inference.vertices = vec![c(), c(), d(), c(), d(), c(), d(), d(), d(), c(), c()];
        let n = tree.inference.vertices.len();
        let needle = &[0, 1, 3, 5, 9, 10].map(Reg);
        assert_eq!(create_simplicial_precoloring(&mut tree, n), needle.len());
        assert!(tree.inference.ordering.as_slice().starts_with(needle));
        for i in 0..n {
            assert!(tree.inference.ordering.contains(&Reg(i)));
        }
    }

    #[test]
    fn test_simplicial_ordering() {
        let mut tree = BasicBlockTree::new_fun();
        let d = || Default::default();
        let c = || Vertex {
            color: Some(0),
            ..Default::default()
        };
        tree.inference.vertices = vec![d(), d(), d(), d(), d(), c()];
        tree.inference.insert_edge(Reg(0), Reg(1));
        tree.inference.insert_edge(Reg(1), Reg(2));
        tree.inference.insert_edge(Reg(2), Reg(3));
        create_simplicial_elimination_ordering(&mut tree);
        assert_eq!(
            tree.inference.ordering.as_slice(),
            &[5, 0, 1, 2, 4, 3].map(Reg)
        );
    }

    #[test]
    fn test_coloring() {
        let mut tree = BasicBlockTree::new_fun();
        let d = || Default::default();
        let c = || Vertex {
            color: Some(0),
            ..Default::default()
        };
        tree.inference.vertices = vec![d(), d(), d(), d(), d(), c()];
        tree.inference.insert_edge(Reg(0), Reg(1));
        tree.inference.insert_edge(Reg(1), Reg(2));
        tree.inference.insert_edge(Reg(2), Reg(3));
        create_simplicial_elimination_ordering(&mut tree);
        create_coloring(&mut tree);
        assert_eq!(
            tree.inference
                .vertices
                .iter()
                .map(|v| v.color.unwrap())
                .collect::<Vec<_>>()
                .as_slice(),
            &[0, 1, 0, 1, 0, 0]
        );
    }
}
