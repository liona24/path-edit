//! TODO Crate documentation
#![warn(missing_docs)]

mod n_vec;
mod path;

pub use n_vec::{NVec, PushVector};
pub use path::Path;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
