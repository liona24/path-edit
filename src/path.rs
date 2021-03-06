use std::cell::Cell;
use std::rc::Rc;

use arrayvec::ArrayVec;

use crate::geom_storage::{GeomStorage, GeomStorageMut};
use crate::index::{Index, IndexViewLine, IndexViewPoint};
use crate::n_vec::{NVec, PushVector};

#[derive(Debug, PartialEq, Clone)]
pub enum Selection {
    /// Select a point represented by the given `IndexView`
    Point { i: IndexViewPoint },
    /// Select an line represented by the given `IndexView`
    Line { i: IndexViewLine },
    /// An empty selection
    Empty,
}

/// An editable path object which is represented by vertices and edges.
///
/// Editing is possible by selecting either a point or an edge and then adding/removing/modifing points, arcs, or curves.
///
/// Internally the path stores:\
/// a) The actual vertices required vertices, especially flattened curves etc.\
/// b) The selectable points\
/// c) The selectable lines, which are also the edges that represent the actual path\
/// d) Backward references to allow dynamic mutation operations while still keeping all indices valid\
///
/// The reference scheme is outlined below:
/// ```text
/// lines:              [ l_0, l_1, ... l_a ... l_k ]
///                                  ┌───┴──────────┐       <-- each line contains the index of two vertices
///                                  |              |           i.e. l_a = (2, 5)
/// vertices:           [ v_0, v_1, v_2, v_3, v_4, v_5, ...     v_n ]
///                                  └───┐                  <-- each point contains the index of one vertex
///                                      |                      i.e. p_b = 2
/// points:             [ p_0, p_1, ... p_b ... p_m ]
///
/// .. and the backward references:
/// referencing_points: { 2: b, ... }
/// referencing_lines:  { 2: [a], 5: [a], ... }
/// ```
///
/// The only references given out are the backward references.
pub struct Path<S> {
    storage: S,

    /// Backward reference to (possibly) retrieve the point's index (value) which is referencing the vertex at given its index (key)
    referencing_points: Vec<Option<Index>>,
    /// Backward reference to (possibly) retrieve the lines' index (value) which are referencing the vertex at the given index (key)
    /// Note that there may only be 2 lines because this is a (looped) path
    referencing_lines: Vec<ArrayVec<[Index; 2]>>,
}

impl<S: GeomStorage<f32, u16> + GeomStorageMut<f32, u16>> Path<S> {
    /// Construct a new empty path backed by the given storage
    ///
    /// Note: The content of the storage is ignored. If the path should recognize points and lines already
    /// contained in the storage use `Path::from_vertices`
    pub fn new(storage: S) -> Self {
        let referencing_points = Vec::new();
        let referencing_lines = Vec::new();

        assert_eq!(storage.get_points().num_components(), 1);
        assert_eq!(storage.get_lines().num_components(), 2);

        Self {
            storage,
            referencing_points,
            referencing_lines,
        }
    }

    /// Construct a new editable path backed by the given storage parsing all
    /// points and lines contained in the storage.
    ///
    /// `points` should be an array of 1-d indices into `vertices`, representing the visible points of the path.\
    /// `lines` should be an array of 2-d indices into `vertices`, representing the visible lines of the path.
    pub fn from_vertices(storage: S) -> Self {
        let mut referencing_lines: Vec<ArrayVec<[Index; 2]>>;
        let mut referencing_points: Vec<Option<Index>>;
        {
            let points = storage.get_points();
            let lines = storage.get_lines();
            let vertices = storage.get_vertices();

            assert_eq!(points.num_components(), 1);
            assert_eq!(lines.num_components(), 2);

            referencing_points = Vec::with_capacity(vertices.len());
            referencing_lines = Vec::with_capacity(vertices.len());

            for _ in 0..vertices.len() {
                referencing_points.push(None);
                referencing_lines.push(ArrayVec::new());
            }

            for (i, p) in points.iter().enumerate() {
                let r = p[0] as usize;
                let i_rc = Rc::new(Cell::new(i as i32));
                referencing_points[r] = Some(i_rc);
            }

            for (i, line) in lines.iter().enumerate() {
                for &r in line.iter() {
                    referencing_lines[r as usize].push(Rc::new(Cell::new(i as i32)));
                }
            }
        }

        Self {
            storage,
            referencing_points,
            referencing_lines,
        }
    }

    /// Add a new point to the path at the given position.
    ///
    /// This point will be visible and selectable.
    ///
    /// Panics if the selection (i.e. the underlying index) is invalid.
    ///
    /// Depending on the current selection `selected` a new edge will be constructed:\
    /// If a point is selected, a new edge between those points will be constructed\
    /// If an edge is selected, the edge will be split at the inserted point, and a new edge will be inserted. (See below)\
    /// If nothing is selected, only the point will be inserted.
    ///
    /// Returns `(IndexViewPoint, Option<IndexViewLine>)`, i.e. the index of the new point. Depending on whether an edge was inserted, the index of the edge, too.
    ///
    /// Example of adding a point `p` with an edge (`a1 - a2`) selected:
    /// ```text
    ///                 p
    /// Before:
    ///          a1 -------- a2
    ///
    ///                 p
    /// After:       /     \
    ///          a1          a2
    /// ```
    pub fn add_point(
        &mut self,
        position: &[f32],
        selected: &Selection,
    ) -> (IndexViewPoint, Option<IndexViewLine>) {
        // index of the new vertex
        let v_new = self.storage.get_vertices_mut().push(position) - 1;
        // index of the new point
        let p_new = self.storage.get_points_mut().push(&[v_new as u16]) - 1;

        // the index of the new point is tracked in `referencing_points`
        let point_index = Rc::new(Cell::new(p_new as i32));
        self.referencing_points.push(Some(Rc::clone(&point_index)));

        let mut line_index = None;
        self.referencing_lines.push(ArrayVec::new());

        match selected {
            // No selection: Nothing to do
            Selection::Empty => {}
            // A point is selected: Construct a new line
            Selection::Point { i } => {
                // Retrieve the selected point
                let index_of_referencing_point = i
                    .try_get()
                    .expect("Selected point's index should be valid!");
                let v_other = &self.storage.get_points()[index_of_referencing_point];
                let v_other = v_other[0];

                // Add the new line
                let l_new = self.storage.get_lines_mut().push(&[v_new as u16, v_other]) - 1;

                // track the new line index
                let rc_l_new = Rc::new(Cell::new(l_new as i32));
                self.referencing_lines[v_new].push(Rc::clone(&rc_l_new));

                line_index = Some(IndexViewLine::new(rc_l_new));

                // track the new line index for the other vertex
                self.referencing_lines[v_other as usize].push(Rc::new(Cell::new(l_new as i32)));
            }
            // An edge is selected, update the old edge and construct a new one
            Selection::Line { i } => {
                // Retrieve the selected line
                let index_of_referencing_line =
                    i.try_get().expect("Selected line's index should be valid!");
                let lines = self.storage.get_lines_mut();

                let p_old;
                {
                    // Update the old line
                    let selected_line = &mut lines[index_of_referencing_line];

                    // p_old1 = selected_line[0];
                    p_old = selected_line[0];
                    selected_line[0] = v_new as u16;
                }

                // Add the new line
                let new_line = (lines.push(&[v_new as u16, p_old]) - 1) as i32;

                // Update the old reference for the second vertex
                for i_ref in self.referencing_lines[p_old as usize].iter() {
                    if i_ref.get() == index_of_referencing_line as i32 {
                        i_ref.set(new_line);
                    }
                }

                // track the line index for the new point
                let new_line = Rc::new(Cell::new(new_line));

                let a = ArrayVec::from([
                    Rc::new(Cell::new(index_of_referencing_line as i32)),
                    Rc::clone(&new_line),
                ]);
                line_index = Some(IndexViewLine::new(new_line));
                self.referencing_lines[v_new] = a;
            }
        }

        (IndexViewPoint::new(point_index), line_index)
    }

    /// Remove the currently selected object.
    ///
    /// Panics if the selection (i.e. the underlying index) is invalid.
    ///
    /// If the selection is empty (`Selection::Empty`) this is a no-op.\
    /// If a point is selected (`Selection::Point`) the point is removed and all the edges which belong to it.\
    /// If an edge is selected (`Selection::Edge`) only the edge is removed.
    pub fn remove(&mut self, selected: Selection) {
        match selected {
            Selection::Empty => {}
            Selection::Point { i } => {
                let index_of_referencing_point = i
                    .try_get()
                    .expect("Selected point's index should be valid!");
                // index of the vertex to be removed
                let v;
                {
                    let vertex_indices = &self
                        .storage
                        .get_points_mut()
                        .swap_remove(index_of_referencing_point);
                    v = vertex_indices[0] as usize;
                }

                // remove the lines which correspond to the selected point
                // we copy the indices, because we have to make sure our reference layout stays intact
                // otherwise updating the references later will become hell
                while let Some(top) = self.referencing_lines[v].pop() {
                    self.remove_line(top.get() as usize);
                    // declare `top` as invalid
                    top.set(-1);
                }

                // Remove the vertex and update the references
                let vertices = self.storage.get_vertices_mut();
                let v_update_index = vertices.len() - 1;

                if let Some(updated_ref) = &mut self.referencing_points[v_update_index] {
                    updated_ref.set(v as i32);
                }

                vertices.swap_remove(v);
                self.referencing_points.swap_remove(v);
                i.set(-1);
                self.referencing_lines.swap_remove(v);
            }
            Selection::Line { i } => {
                let index_of_referencing_line =
                    i.try_get().expect("Selected line's index should be valid!");

                self.remove_line(index_of_referencing_line);
                self.referencing_lines
                    .swap_remove(index_of_referencing_line);
                i.set(-1);
            }
        }
    }

    /// Query this path for the point corresponding to the given index.
    ///
    /// Returns the location of the point. The sice of the slice depends on the number of components used for initialization.
    ///
    /// Panics if the given index is invalid.
    pub fn query_point(&self, index: &IndexViewPoint) -> &[f32] {
        let i = self.vertex_index_from_index_view_point(index);
        &self.storage.get_vertices()[i]
    }

    /// Query this path for the point corresponding to the given index.
    ///
    /// Returns the mutable location of the point. The sice of the slice depends on the number of components used for initialization.
    ///
    /// Panics if the given index is invalid.
    pub fn query_point_mut(&mut self, index: &IndexViewPoint) -> &mut [f32] {
        let i = self.vertex_index_from_index_view_point(index);
        &mut self.storage.get_vertices_mut()[i]
    }

    /// Query this path for the edge corresponding to the given index.
    ///
    /// Returns the location of the start and end point.
    /// The sice of each slice depends on the number of components used for initialization.
    ///
    /// Panics if the given index is invalid.
    pub fn query_line(&self, index: &IndexViewLine) -> (&[f32], &[f32]) {
        let (i, j) = self.vertex_index_from_index_view_line(index);
        let vertices = self.storage.get_vertices();
        (&vertices[i], &vertices[j])
    }

    /// Query this path for the given selection and apply the given function to each vertex associated.
    ///
    /// Panics if the given index is invalid.
    pub fn apply<F: Fn(&mut [f32])>(&mut self, selection: &Selection, func: F) {
        match selection {
            Selection::Empty => {}
            Selection::Point { i } => {
                let j = self.vertex_index_from_index_view_point(i);
                func(&mut self.storage.get_vertices_mut()[j]);
            }
            Selection::Line { i } => {
                let (j, k) = self.vertex_index_from_index_view_line(i);
                let vertices = self.storage.get_vertices_mut();
                func(&mut vertices[j]);
                func(&mut vertices[k]);
            }
        }
    }

    /// Finds the indices of all the vertices associated with the given selection.
    ///
    /// If the selection is a point a single index is returned.\
    /// If a line is selected two indices are returned.
    pub fn query_vertex_indices(&self, selection: &Selection) -> ArrayVec<[u16; 2]> {
        let mut rv = ArrayVec::new();
        match selection {
            Selection::Point { i } => {
                let i = self.vertex_index_from_index_view_point(i);
                rv.push(i as u16);
            }
            Selection::Line { i } => {
                let (i, j) = self.vertex_index_from_index_view_line(i);
                rv.push(i as u16);
                rv.push(j as u16);
            }
            Selection::Empty => {}
        };

        rv
    }

    /// Attempt to find an edge between the points given.
    ///
    /// Panics if either index is invalid.
    pub fn find_line(
        &self,
        point1: &IndexViewPoint,
        point2: &IndexViewPoint,
    ) -> Option<IndexViewLine> {
        panic!("Not implemented");
    }

    /// Remove the line at index `line_index` in `lines` and update all references.
    ///
    /// This operation only removes from `self.lines`. It requires all other structures to be intact!
    ///
    /// This is an O(1) operation.
    fn remove_line(&mut self, line_index: usize) {
        let lines = self.storage.get_lines_mut();
        let update_index = (lines.len() - 1) as i32;
        // note that the following moves the last element of `lines` to the index where we remove
        let vertex_indices = lines.swap_remove(line_index);
        let line_index = line_index as i32;

        // Remove the line from the backward references
        for &v_i in vertex_indices.iter() {
            self.referencing_lines[v_i as usize].retain(|el| el.get() != line_index);
        }

        // Update the backward references for the moved line (which now resides at `line_index` and previously was at `update_index`)
        if update_index != line_index {
            let update_vertex_indices = &lines[line_index as usize];
            for &v_i in update_vertex_indices.iter() {
                for ref_line in self.referencing_lines[v_i as usize].iter() {
                    if ref_line.get() == update_index {
                        ref_line.set(line_index);
                    }
                }
            }
        }
    }

    #[inline]
    /// Unwraps the given IndexView and returns the index into `self.storage.vertices` which corresponds to it
    fn vertex_index_from_index_view_point(&self, index: &IndexViewPoint) -> usize {
        let index_of_referencing_point = index.try_get().expect("Index should be valid!");

        self.storage.get_points()[index_of_referencing_point][0] as usize
    }

    #[inline]
    /// Unwraps the given IndexView and returns the indices into `self.storage.vertices` which correspond to it
    fn vertex_index_from_index_view_line(&self, index: &IndexViewLine) -> (usize, usize) {
        let index_of_referencing_line = index.try_get().expect("Index should be valid!");

        let line_index = &self.storage.get_lines()[index_of_referencing_line];
        let j = line_index[0] as usize;
        let k = line_index[1] as usize;

        (k, j)
    }
}

impl<S: GeomStorage<f32, u16>> GeomStorage<f32, u16> for Path<S> {
    fn get_vertices(&self) -> &NVec<f32> {
        self.storage.get_vertices()
    }
    fn get_points(&self) -> &NVec<u16> {
        self.storage.get_points()
    }
    fn get_lines(&self) -> &NVec<u16> {
        self.storage.get_lines()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geom_storage::DefaultStorage;
    use std::collections::HashSet;
    use std::hash::Hash;
    use std::iter::FromIterator;

    fn set_equal<S: Hash + Eq, I: IntoIterator<Item = S>>(iter1: I, iter2: I) -> bool {
        let h1: HashSet<S> = HashSet::from_iter(iter1);
        let h2: HashSet<S> = HashSet::from_iter(iter2);

        h1 == h2
    }

    fn bootstrap_edge() -> (
        Path<DefaultStorage<f32, u16>>,
        [IndexViewPoint; 2],
        IndexViewLine,
    ) {
        let mut vertices = NVec::new(1);
        vertices.push(&[1.0]);
        vertices.push(&[3.0]);

        let mut points = NVec::new(1);
        points.push(&[0u16]);
        points.push(&[1u16]);

        let mut lines = NVec::new(2);
        lines.push(&[0u16, 1u16]);

        let storage = DefaultStorage::from_vertices(vertices, points, lines);
        let p = Path::from_vertices(storage);
        let p_indices = [
            IndexViewPoint {
                value: Rc::clone(&p.referencing_points[0].as_ref().unwrap()),
            },
            IndexViewPoint {
                value: Rc::clone(&p.referencing_points[1].as_ref().unwrap()),
            },
        ];
        // the next one is a little bit hacky, we abuse the fact that the points at the end of our "double" edge only reference to one line
        let l_index = IndexViewLine {
            value: Rc::clone(&p.referencing_lines[0][0]),
        };

        (p, p_indices, l_index)
    }

    #[test]
    fn from_vertices_sets_different_references() {
        let (path, _, _) = bootstrap_edge();

        path.referencing_points[0].as_ref().unwrap().set(10);
        path.referencing_points[1].as_ref().unwrap().set(11);
        path.referencing_lines[0][0].set(12);
        path.referencing_lines[1][0].set(13);

        let p1 = path.referencing_points[0].as_ref().unwrap().get();
        let p2 = path.referencing_points[1].as_ref().unwrap().get();
        let l1 = path.referencing_lines[0][0].get();
        let l2 = path.referencing_lines[1][0].get();

        assert_eq!(p1, 10);
        assert_eq!(p2, 11);
        assert_eq!(l1, 12);
        assert_eq!(l2, 13);
    }

    #[test]
    fn add_point_with_empty_selection() {
        let (mut path, [_p0, _p1], _e0) = bootstrap_edge();
        let selected = Selection::Empty;

        let (p_new, e_new) = path.add_point(&[3.0], &selected);

        // Without an active selection, no edge should have been added
        assert!(e_new.is_none());

        let vertices = path.get_vertices();
        let points = path.get_points();

        // Check that all vital components were added:
        assert_eq!(vertices.len(), 3);
        assert_eq!(path.referencing_lines.len(), 3);
        assert_eq!(path.referencing_points.len(), 3);

        // Check that the point was added
        assert_eq!(points.len(), 3);
        assert_eq!(points[2][0], 2);

        // Check that the back-ref is correct
        let back_ref = path.referencing_points[2]
            .as_ref()
            .expect("Back-ref should have been added");
        assert_eq!(back_ref.get(), 2);

        // Check that the returned reference is actually pointing to the new point
        assert_eq!(back_ref, &p_new.value);
    }

    #[test]
    fn add_point_with_point_selection() {
        let (mut path, [p0, _p1], _e0) = bootstrap_edge();
        let selected = Selection::Point { i: p0 };

        let (p_new, e_new) = path.add_point(&[3.0], &selected);

        let e_new = e_new.expect("An edge should have beend added");

        let vertices = path.get_vertices();
        let points = path.get_points();
        let lines = path.get_lines();

        // Check that all vital components were added:
        assert_eq!(vertices.len(), 3);
        assert_eq!(path.referencing_lines.len(), 3);
        assert_eq!(path.referencing_points.len(), 3);

        // Check that the point was added
        assert_eq!(points.len(), 3);
        assert_eq!(points[2][0], 2);

        // Check that the line was added
        assert_eq!(lines.len(), 2);
        assert!(set_equal(&lines[1], &[0, 2]));

        // Check that the back-ref is correct
        let back_ref_point = path.referencing_points[2]
            .as_ref()
            .expect("Back-ref should have been added");
        assert_eq!(back_ref_point.get(), 2);

        let back_ref_line: Vec<i32> = path.referencing_lines[2].iter().map(|x| x.get()).collect();
        assert_eq!(back_ref_line, &[1]);

        // Check that the back-ref for the selected point was added
        let back_ref_line_other: Vec<i32> =
            path.referencing_lines[0].iter().map(|x| x.get()).collect();
        assert!(set_equal(back_ref_line_other, [1, 0].to_vec()));

        // Check that the returned reference is actually pointing to the new point
        assert_eq!(back_ref_point, &p_new.value);
        assert_eq!(&path.referencing_lines[2][0], &e_new.value);
    }

    #[test]
    fn add_point_with_edge_selection() {
        let (mut path, [_p0, _p1], e0) = bootstrap_edge();
        let selected = Selection::Line { i: e0 };

        let (p_new, e_new) = path.add_point(&[3.0], &selected);

        let e_new = e_new.expect("An edge should have beend added");

        let vertices = path.get_vertices();
        let points = path.get_points();
        let lines = path.get_lines();

        // Check that all vital components were added:
        assert_eq!(vertices.len(), 3);
        assert_eq!(path.referencing_lines.len(), 3);
        assert_eq!(path.referencing_points.len(), 3);

        // Check that the point was added
        assert_eq!(points.len(), 3);
        assert_eq!(points[2][0], 2);

        // Check that the line was added
        assert_eq!(lines.len(), 2);
        // either the first or the second point were retained, but not both
        let scenario_1 = set_equal(&lines[1], &[2, 1]);
        let scenario_2 = set_equal(&lines[1], &[2, 0]);
        assert!(scenario_1 || scenario_2);

        // Check that the back-ref is correct
        let back_ref_point = path.referencing_points[2]
            .as_ref()
            .expect("Back-ref should have been added");
        assert_eq!(back_ref_point.get(), 2);
        let back_ref_line: Vec<i32> = path.referencing_lines[2].iter().map(|x| x.get()).collect();
        // the new point is now part of two edges
        assert!(set_equal(&back_ref_line, &[1, 0].to_vec()));

        // Check that the back-refs for the selected edge were updated
        let back_ref_line_other1: Vec<i32> =
            path.referencing_lines[0].iter().map(|x| x.get()).collect();
        let back_ref_line_other2: Vec<i32> =
            path.referencing_lines[1].iter().map(|x| x.get()).collect();

        if scenario_1 {
            assert!(
                set_equal(&back_ref_line_other1, &[0].to_vec())
                    && set_equal(&back_ref_line_other2, &[1].to_vec())
            );
        } else if scenario_2 {
            assert!(
                set_equal(&back_ref_line_other1, &[1].to_vec())
                    && set_equal(&back_ref_line_other2, &[0].to_vec())
            );
        }

        // Check that the returned reference is actually pointing to the new point
        assert_eq!(back_ref_point, &p_new.value);
        if scenario_1 {
            assert!(path.referencing_lines[2][0] == e_new.value);
        } else if scenario_2 {
            assert!(path.referencing_lines[2][1] == e_new.value)
        }
    }

    #[test]
    fn remove_with_point_selected_and_two_affected_edges() {
        let (mut path, [_, p1], _) = bootstrap_edge();

        let selected = Selection::Point { i: p1 };

        let (new_point, _) = path.add_point(&[4.0], &selected);
        path.remove(selected);

        assert_eq!(path.get_lines().len(), 0);
        assert_eq!(path.referencing_lines[0].len(), 0);
        assert_eq!(path.referencing_lines[1].len(), 0);

        assert_eq!(new_point.try_get().unwrap(), 1);
    }

    #[test]
    fn remove_with_point_selected_and_one_affected_edge() {
        let (mut path, [_, p1], _) = bootstrap_edge();

        let (new_point, _) = path.add_point(&[4.0], &Selection::Empty);
        path.remove(Selection::Point { i: p1 });

        assert_eq!(path.get_lines().len(), 0);
        assert_eq!(path.referencing_lines[0].len(), 0);

        assert_eq!(new_point.try_get().unwrap(), 1);
    }

    #[test]
    fn remove_with_point_selected_removes_all_corresponding_information() {
        let (mut path, [_, p1], _) = bootstrap_edge();

        let (new_point, _) = path.add_point(&[4.0], &Selection::Empty);
        let (_, _) = path.add_point(&[6.0], &Selection::Point { i: p1 });
        path.remove(Selection::Point { i: new_point });

        assert_eq!(path.get_vertices().len(), 3);
        assert_eq!(path.get_points().len(), 3);
        assert_eq!(path.get_lines().len(), 2);
        assert_eq!(path.referencing_points.len(), 3);
        assert_eq!(path.referencing_lines.len(), 3);
    }

    #[test]
    fn remove_with_point_selected_and_no_affected_edge() {
        let (mut path, [_, p1], _) = bootstrap_edge();

        let (new_point, _) = path.add_point(&[4.0], &Selection::Empty);
        let (_, _) = path.add_point(&[6.0], &Selection::Point { i: p1 });
        path.remove(Selection::Point { i: new_point });

        // The trailing line is organized correctly
        assert_eq!(path.referencing_lines[2].len(), 1);
        assert_eq!(path.referencing_lines[2][0].get(), 1);

        // The reference of the point got updated correctly
        assert_eq!(path.referencing_points[2].as_ref().unwrap().get(), 2);
    }

    fn setup_empty_path_2d() -> Path<DefaultStorage<f32, u16>> {
        let storage = DefaultStorage::new(2);
        Path::new(storage)
    }

    #[test]
    fn query_point_queries_correct_point() {
        let mut path = setup_empty_path_2d();
        let (p1, _) = path.add_point(&[1.0, 2.0], &Selection::Empty);
        let (p2, _) = path.add_point(&[3.0, 4.0], &Selection::Point { i: p1 });

        assert_eq!(path.query_point(&p2), &[3.0, 4.0]);
    }

    #[test]
    fn query_point_mut_queries_correct_point() {
        let mut path = setup_empty_path_2d();
        let (p1, _) = path.add_point(&[1.0, 2.0], &Selection::Empty);
        let (p2, _) = path.add_point(&[3.0, 4.0], &Selection::Point { i: p1 });

        assert_eq!(path.query_point_mut(&p2), &[3.0, 4.0]);
    }

    #[test]
    fn query_line_queries_correct_points() {
        let mut path = setup_empty_path_2d();
        let (p1, _) = path.add_point(&[1.0, 2.0], &Selection::Empty);
        let (_p2, line) = path.add_point(&[3.0, 4.0], &Selection::Point { i: p1 });
        let line = line.unwrap();

        let first = path.query_line(&line) == (&[1.0, 2.0], &[3.0, 4.0]);
        let second = path.query_line(&line) == (&[3.0, 4.0], &[1.0, 2.0]);

        assert!(second || first);
    }

    #[test]
    fn query_vertex_indices_queries_correct_points_and_lines_if_point_selected() {
        let mut path = setup_empty_path_2d();

        let (p1, _) = path.add_point(&[1.0, 2.0], &Selection::Empty);
        let (_, line1) = path.add_point(&[3.0, 4.0], &Selection::Point { i: p1 });
        let (p3, _) = path.add_point(&[5.0, 6.0], &Selection::Line { i: line1.unwrap() });

        let indices = path.query_vertex_indices(&Selection::Point { i: p3 });
        // we queried for a point so each index should only represent single point
        assert_eq!(indices.len(), 1);
        let i = indices[0] as usize;
        assert_eq!(&path.get_vertices()[i], &[5.0, 6.0]);
    }

    #[test]
    fn query_vertex_indices_queries_correct_points_and_lines_if_line_selected() {
        let mut path = setup_empty_path_2d();

        let (p1, _) = path.add_point(&[1.0, 2.0], &Selection::Empty);
        let (_, line1) = path.add_point(&[3.0, 4.0], &Selection::Point { i: p1 });

        let indices = path.query_vertex_indices(&Selection::Line { i: line1.unwrap() });
        // we queried for a line so each index should represent two points
        assert_eq!(indices.len(), 2);
        let i1 = indices[0] as usize;
        let i2 = indices[1] as usize;

        let v = path.get_vertices();

        let first = v[i1] == [1.0, 2.0] && v[i2] == [3.0, 4.0];
        let second = v[i2] == [1.0, 2.0] && v[i1] == [3.0, 4.0];

        assert!(first || second);
    }

    #[test]
    fn query_vertex_indices_queries_nothing_if_empty_selection() {
        let mut path = setup_empty_path_2d();
        let (_, _) = path.add_point(&[1.0, 2.0], &Selection::Empty);
        assert_eq!(path.query_vertex_indices(&Selection::Empty).len(), 0);
    }

    #[test]
    fn apply_applies_to_correct_points_and_lines_if_point_selected() {
        let mut path = setup_empty_path_2d();

        let (p1, _) = path.add_point(&[1.0, 2.0], &Selection::Empty);
        let (p2, line1) = path.add_point(&[3.0, 4.0], &Selection::Point { i: p1.clone() });
        let (p3, _) = path.add_point(&[5.0, 6.0], &Selection::Line { i: line1.unwrap() });

        let selection = Selection::Point { i: p3.clone() };
        path.apply(&selection, |v| v[0] = -1.0);
        assert_eq!(path.query_point(&p3), &[-1.0, 6.0]);
        assert_eq!(path.query_point(&p1), &[1.0, 2.0]);
        assert_eq!(path.query_point(&p2), &[3.0, 4.0]);
    }

    #[test]
    fn apply_applies_to_correct_points_and_lines_if_line_selected() {
        let mut path = setup_empty_path_2d();

        let (p1, _) = path.add_point(&[1.0, 2.0], &Selection::Empty);
        let (p2, line1) = path.add_point(&[3.0, 4.0], &Selection::Point { i: p1.clone() });

        path.apply(&Selection::Line { i: line1.unwrap() }, |v| v[0] *= -1.);
        assert_eq!(path.query_point(&p1), &[-1.0, 2.0]);
        assert_eq!(path.query_point(&p2), &[-3.0, 4.0]);
    }
}
