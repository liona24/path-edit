//! TODO Crate documentation
#![warn(missing_docs)]

pub mod geom_storage;
pub mod index;
pub mod n_vec;
mod path;

pub use path::Path;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
