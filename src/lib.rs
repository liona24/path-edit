//! TODO Crate documentation
#![warn(missing_docs)]

mod n_vec;
mod path;
mod index;

pub use n_vec::{NVec, PushVector};
pub use index::{Index, IndexViewLine, IndexViewPoint};
pub use path::Path;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
