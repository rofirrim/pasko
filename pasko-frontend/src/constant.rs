use std::cmp::{Eq, PartialEq};
use std::convert::From;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub enum Constant {
    Integer(i64),
    Real(f64),
    Bool(bool),
}

impl From<i64> for Constant {
    fn from(v: i64) -> Constant {
        Constant::Integer(v)
    }
}

impl From<f64> for Constant {
    fn from(v: f64) -> Constant {
        assert!(!v.is_nan());
        Constant::Real(v)
    }
}

impl From<bool> for Constant {
    fn from(v: bool) -> Constant {
        Constant::Bool(v)
    }
}

impl ToString for Constant {
    fn to_string(&self) -> String {
        match self {
            Constant::Integer(x) => x.to_string(),
            Constant::Real(x) => x.to_string(),
            Constant::Bool(x) => x.to_string(),
        }
    }
}

// Manually implement PartialEq due to f64.
impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Constant::Integer(x), Constant::Integer(y)) => x.eq(y),
            (Constant::Bool(x), Constant::Bool(y)) => x.eq(y),
            (Constant::Real(x), Constant::Real(y)) => {
                // We do not expect nans here.
                debug_assert!(!x.is_nan() && !y.is_nan());
                x.to_bits().eq(&y.to_bits())
            }
            _ => false,
        }
    }
}

impl Eq for Constant {}

// Allow constants be hashable.
impl Hash for Constant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Constant::Integer(x) => x.hash(state),
            Constant::Bool(b) => b.hash(state),
            Constant::Real(x) => {
                debug_assert!(!x.is_nan());
                x.to_bits().hash(state);
            }
        }
    }
}
