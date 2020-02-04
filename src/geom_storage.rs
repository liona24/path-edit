//! Provides traits required to be implemented by vertex storages to be used as backend storage for
//! a `Path` object.
//!
//! Also provides a simple storage container `SimpleStorage` to make your life easier.
use crate::n_vec::NVec;

/// Trait which should be implemented by geometry storages. Provides ways of accessing the stored data.
pub trait GeomStorage<V, I> {
    /// Get a reference to the vertices stored.
    fn get_vertices(&self) -> &NVec<V>;
    /// Get a reference to the points stored.
    fn get_points(&self) -> &NVec<I>;
    /// Get a reference to the lines stored.
    fn get_lines(&self) -> &NVec<I>;
}

/// Trait which can be implemented by geometry storages. Provides ways of manipulating the stored data.
pub trait GeomStorageMut<V, I> {
    /// Get a mutable reference to the vertices stored.
    fn get_vertices_mut(&mut self) -> &mut NVec<V>;
    /// Get a mutable reference to the points stored.
    fn get_points_mut(&mut self) -> &mut NVec<I>;
    /// Get a mutable reference to the lines stored.
    fn get_lines_mut(&mut self) -> &mut NVec<I>;
}

/// A convenient structure holding geometry data, i.e. vertices and indices for points and lines
pub struct SimpleStorage<V, I> {
    vertices: NVec<V>,
    points: NVec<I>,
    lines: NVec<I>,
}

impl<V, I> SimpleStorage<V, I> {
    /// Construct an empty simple vertex storage for vertices with the given number of components
    pub fn new(num_components: usize) -> Self {
        Self {
            vertices: NVec::new(num_components),
            points: NVec::new(1),
            lines: NVec::new(2),
        }
    }

    /// Construct a simple vertex storage filling it with the given data.
    ///
    /// Note: Make sure that points is a collection of 1-d vectors and lines a collection of 2-d vectors
    pub fn from_vertices(vertices: NVec<V>, points: NVec<I>, lines: NVec<I>) -> Self {
        assert_eq!(points.num_components(), 1);
        assert_eq!(lines.num_components(), 2);

        Self {
            vertices,
            points,
            lines,
        }
    }
}

impl<V, I> GeomStorage<V, I> for SimpleStorage<V, I> {
    fn get_vertices(&self) -> &NVec<V> {
        &self.vertices
    }
    fn get_points(&self) -> &NVec<I> {
        &self.points
    }
    fn get_lines(&self) -> &NVec<I> {
        &self.lines
    }
}

impl<V, I> GeomStorageMut<V, I> for SimpleStorage<V, I> {
    fn get_vertices_mut(&mut self) -> &mut NVec<V> {
        &mut self.vertices
    }
    fn get_points_mut(&mut self) -> &mut NVec<I> {
        &mut self.points
    }
    fn get_lines_mut(&mut self) -> &mut NVec<I> {
        &mut self.lines
    }
}
