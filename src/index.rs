use std::cell::Cell;
use std::rc::Rc;

/// An index into a vertex array
///
/// To match WebGl capabilities it supports indices in the range of `u16`
///
/// If the index is negative it has to be considered invalid and the reference can be dropped.\
/// Conventionally to mark an index as invalid set it to -1.
pub type Index = Rc<Cell<i32>>;

/// Index of a point.
///
/// It can be used to select the indexed object. It is agnostic of underlying mutations of the corresponding `Path`
#[derive(Debug, Clone, PartialEq)]
pub struct IndexViewPoint {
    pub(crate) value: Index,
}

/// Index of a line.
///
/// It can be used to select the indexed object. It is agnostic of underlying mutations of the corresponding `Path`
#[derive(Debug, Clone, PartialEq)]
pub struct IndexViewLine {
    pub(crate) value: Index,
}

macro_rules! implement_view {
    ($t:ty) => {
        impl $t {
            /// Borrow the actual value\
            /// This returns `None` if the index is invalid.
            #[inline]
            pub(crate) fn try_get(&self) -> Option<usize> {
                let value = self.value.get();
                if value >= 0 {
                    Some(value as usize)
                } else {
                    None
                }
            }

            /// Set the internal value
            #[inline]
            pub(crate) fn set(&self, v : i32) {
                self.value.set(v);
            }

            #[inline]
            pub(crate) fn new(value : Index) -> Self {
                Self {
                    value
                }
            }

            /// Check if the underlying index is still valid.
            #[inline]
            pub fn is_valid(&self) -> bool {
                self.value.get() >= 0
            }
        }
    };
}

implement_view!(IndexViewPoint);
implement_view!(IndexViewLine);
