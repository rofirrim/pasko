use std::cmp::{Eq, Ordering, PartialEq, PartialOrd};
use std::convert::From;
use std::fmt::Display;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub enum Constant {
    Integer(i64),
    Real(f64),
    Bool(bool),
    String(String),
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

impl From<String> for Constant {
    fn from(v: String) -> Constant {
        Constant::String(v.clone())
    }
}

impl From<&String> for Constant {
    fn from(v: &String) -> Constant {
        Constant::String(v.clone())
    }
}

impl From<&str> for Constant {
    fn from(v: &str) -> Constant {
        Constant::String(v.to_string())
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Integer(x) => write!(f, "{}", x),
            Constant::Real(x) => write!(f, "{}", x),
            Constant::Bool(x) => write!(f, "{}", x),
            Constant::String(x) => write!(f, "{}", x),
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
            (Constant::String(x), Constant::String(y)) => x.eq(y),
            _ => false,
        }
    }
}

impl Eq for Constant {}

// Manually implement PartialOrd
impl PartialOrd for Constant {
    fn partial_cmp(&self, other: &Constant) -> Option<Ordering> {
        match (self, other) {
            (Constant::Integer(x), Constant::Integer(y)) => x.partial_cmp(y),
            (Constant::Bool(x), Constant::Bool(y)) => x.partial_cmp(y),
            (Constant::Real(x), Constant::Real(y)) => x.partial_cmp(y),
            (Constant::String(x), Constant::String(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

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
            Constant::String(s) => s.hash(state),
        }
    }
}
