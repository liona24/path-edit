//! Module containing several operations for collections and a flat-stored multi-dimensional Vector storage
use std::ops::{Index, IndexMut};
use std::slice::{Chunks, ChunksMut};

/// A convenience data struct which stores n-dimensional vectors in a single flattened array for trivial mapping to the graphics card.
pub struct NVec<T> {
    values: Vec<T>,
    num_components: usize,
}

/// Provides push operations for a container type
pub trait PushVector<T> {
    /// Stores the given vector. Consumes only `num_components` elements. Returns the new number of vectors stored.
    fn push<I: IntoIterator<Item = T>>(&mut self, vector: I) -> usize;
}

/// Provides pop operations for a container type
pub trait PopVector<T> {
    /// Remove the last element, if any, and return it
    fn pop(&mut self) -> Option<Vec<T>>;
}

/// Trait to be implemented by collections which allow counting of stored objects
pub trait Count {
    /// Get the total number of elements
    fn num_elements(&self) -> usize;
}

impl<T> NVec<T> {
    /// Initialize an empty storage for vectory of the given dimension `num_components` with the default initial capacity.
    pub fn new(num_components: usize) -> Self {
        Self {
            values: Vec::new(),
            num_components,
        }
    }

    /// Get the number of components for each vector stored
    pub fn num_components(&self) -> usize {
        self.num_components
    }

    /// Initialize an empty storage for vector of the given dimension `num_components` with the given initial capacity.
    pub fn with_capacity(num_components: usize, capacity: usize) -> Self {
        Self {
            values: Vec::with_capacity(capacity),
            num_components,
        }
    }

    /// Returns the underlying flattened array
    pub fn raw(&self) -> &Vec<T> {
        &self.values
    }

    /// Returns the underlying flattened array mutable
    pub fn raw_mut(&mut self) -> &mut Vec<T> {
        &mut self.values
    }

    /// Return an iterator over each vector stored.
    pub fn iter(&self) -> Chunks<T> {
        self.values.chunks(self.num_components)
    }

    /// Return an mutable iterator over each vector stored.
    pub fn iter_mut(&mut self) -> ChunksMut<T> {
        self.values.chunks_mut(self.num_components)
    }

    /// Return the number of vectors stored
    pub fn len(&self) -> usize {
        self.values.len() / self.num_components
    }

    /// Check whether this container is empty
    pub fn is_empty(&self) -> bool {
        self.values.len() == 0
    }
}

impl<T: Clone> NVec<T> {
    /// Creates a new NVec with the given number of components and copies the given values.
    pub fn from_slice(num_components: usize, values: &[T]) -> Self {
        let values = values.to_vec();
        assert_eq!(values.len() % num_components, 0);

        Self {
            num_components,
            values,
        }
    }
}

impl<T: Default + Copy> NVec<T> {
    /// Removes the vector located at the given index and replaces it with the last element.
    ///
    /// Returns the removed vector. It will have length `num_components`
    pub fn swap_remove(&mut self, index: usize) -> Vec<T> {
        let mut rv: Vec<T> = vec![T::default(); self.num_components];
        let i = index * self.num_components;
        for j in 1..=self.num_components {
            rv[self.num_components - j] = self.values.swap_remove(i + self.num_components - j);
        }
        rv
    }
}

impl<T> Count for NVec<T> {
    fn num_elements(&self) -> usize {
        self.values.len()
    }
}

impl<T> PushVector<T> for NVec<T> {
    fn push<I: IntoIterator<Item = T>>(&mut self, vector: I) -> usize {
        if cfg!(debug_assertions) {
            let vector: Vec<T> = vector.into_iter().take(self.num_components).collect();
            debug_assert_eq!(vector.len(), self.num_components);
            self.values.extend(vector);
        } else {
            self.values
                .extend(vector.into_iter().take(self.num_components));
        }
        self.len()
    }
}

impl<T> PopVector<T> for NVec<T> {
    fn pop(&mut self) -> Option<Vec<T>> {
        if self.values.is_empty() {
            None
        } else {
            let mut v = Vec::with_capacity(self.num_components);
            for _ in 0..self.num_components {
                // if the other logic is correct we should have at least num_components elements
                v.push(self.values.pop().unwrap());
            }
            v.reverse();
            Some(v)
        }
    }
}

impl<'a, T: 'a + Copy> PushVector<&'a T> for NVec<T> {
    fn push<I: IntoIterator<Item = &'a T>>(&mut self, vector: I) -> usize {
        if cfg!(debug_assertions) {
            let vector: Vec<T> = vector
                .into_iter()
                .take(self.num_components)
                .cloned()
                .collect();
            debug_assert_eq!(vector.len(), self.num_components);
            self.values.extend(vector);
        } else {
            self.values
                .extend(vector.into_iter().take(self.num_components));
        }
        self.len()
    }
}

impl<T> Index<usize> for NVec<T> {
    type Output = [T];

    fn index(&self, index: usize) -> &Self::Output {
        let i = index * self.num_components;
        &self.values[i..i + self.num_components]
    }
}

impl<T> IndexMut<usize> for NVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let i = index * self.num_components;
        &mut self.values[i..i + self.num_components]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_swap_remove() {
        let mut x: NVec<usize> = NVec::new(3);
        x.push(&[1, 2, 3]);
        x.push(&[4, 5, 6]);
        x.push(&[7, 8, 9]);
        x.push(&[10, 11, 12]);

        let removed = x.swap_remove(1);

        assert_eq!(removed, &[4, 5, 6]);
        assert_eq!(x.raw(), &[1, 2, 3, 10, 11, 12, 7, 8, 9]);
    }

    #[test]
    fn test_pop() {
        let mut x: NVec<usize> = NVec::new(3);
        x.push(&[1, 2, 3]);
        x.push(&[4, 5, 6]);

        let removed = x.pop().expect("Should be able to pop element");

        assert_eq!(removed, &[4, 5, 6]);
        assert_eq!(x.raw(), &[1, 2, 3]);
    }

    #[test]
    fn test_index() {
        let mut x: NVec<usize> = NVec::new(5);
        x.push(&[1, 2, 3, 4, 5]);
        x.push(&[6, 7, 8, 9, 10]);

        assert_eq!(&x[1], &[6, 7, 8, 9, 10]);
    }

    #[test]
    fn test_index_mut() {
        let mut x: NVec<usize> = NVec::new(5);
        x.push(&[1, 2, 3, 4, 5]);
        x.push(&[6, 7, 8, 9, 10]);

        assert_eq!(&mut x[1], &[6, 7, 8, 9, 10]);
    }
}
