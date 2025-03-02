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

impl Constant {
    pub fn is_zero(&self) -> bool {
        match self {
            Constant::Integer(x) => *x == 0,
            Constant::Real(x) => *x == 0.0,
            _ => {
                return false;
            }
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            Constant::Integer(x) => *x > 0,
            Constant::Real(x) => *x > 0.0,
            _ => {
                return false;
            }
        }
    }

    pub fn is_negative(&self) -> bool {
        !self.is_zero() && !self.is_positive()
    }

    // Boolean operations.
    pub fn and(a: &Constant, b: &Constant) -> Option<Constant> {
        if let (Constant::Bool(a_val), Constant::Bool(b_val)) = (a, b) {
            Some(Constant::Bool(*a_val && *b_val))
        } else {
            None
        }
    }

    pub fn or(a: &Constant, b: &Constant) -> Option<Constant> {
        if let (Constant::Bool(a_val), Constant::Bool(b_val)) = (a, b) {
            Some(Constant::Bool(*a_val || *b_val))
        } else {
            None
        }
    }

    pub fn not(a: &Constant) -> Option<Constant> {
        if let Constant::Bool(a_val) = a {
            Some(Constant::Bool(!*a_val))
        } else {
            None
        }
    }

    // Relational operations.
    pub fn equal(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Bool(a_val), Constant::Bool(b_val)) => Some(Constant::Bool(a_val == b_val)),
            (Constant::Real(a_val), Constant::Real(b_val)) => Some(Constant::Bool(a_val == b_val)),
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                Some(Constant::Bool(a_val == b_val))
            }
            _ => None,
        }
    }

    pub fn not_equal(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Bool(a_val), Constant::Bool(b_val)) => Some(Constant::Bool(a_val != b_val)),
            (Constant::Real(a_val), Constant::Real(b_val)) => Some(Constant::Bool(a_val != b_val)),
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                Some(Constant::Bool(a_val != b_val))
            }
            _ => None,
        }
    }

    pub fn less(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Real(a_val), Constant::Real(b_val)) => Some(Constant::Bool(a_val < b_val)),
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                Some(Constant::Bool(a_val < b_val))
            }
            _ => None,
        }
    }

    pub fn less_or_equal(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Real(a_val), Constant::Real(b_val)) => Some(Constant::Bool(a_val <= b_val)),
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                Some(Constant::Bool(a_val <= b_val))
            }
            _ => None,
        }
    }

    pub fn greater(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Real(a_val), Constant::Real(b_val)) => Some(Constant::Bool(a_val > b_val)),
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                Some(Constant::Bool(a_val > b_val))
            }
            _ => None,
        }
    }

    pub fn greater_or_equal(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Real(a_val), Constant::Real(b_val)) => Some(Constant::Bool(a_val >= b_val)),
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                Some(Constant::Bool(a_val >= b_val))
            }
            _ => None,
        }
    }

    pub fn normalize_real(x: f64) -> Option<f64> {
        if x.is_nan() || x.is_infinite() {
            None
        } else {
            Some(x)
        }
    }

    // Basic arithmetic.
    pub fn add(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Real(a_val), Constant::Real(b_val)) => {
                Self::normalize_real(a_val + b_val).map(Constant::Real)
            }
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                a_val.checked_add(*b_val).map(Constant::Integer)
            }
            _ => None,
        }
    }

    pub fn sub(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Real(a_val), Constant::Real(b_val)) => {
                Self::normalize_real(a_val - b_val).map(Constant::Real)
            }
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                a_val.checked_sub(*b_val).map(Constant::Integer)
            }
            _ => None,
        }
    }

    pub fn mul(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Real(a_val), Constant::Real(b_val)) => {
                Self::normalize_real(a_val * b_val).map(Constant::Real)
            }
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                a_val.checked_mul(*b_val).map(Constant::Integer)
            }
            _ => None,
        }
    }

    pub fn idiv(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Integer(a_val), Constant::Integer(b_val)) => {
                a_val.checked_div(*b_val).map(Constant::Integer)
            }
            _ => None,
        }
    }

    pub fn div(a: &Constant, b: &Constant) -> Option<Constant> {
        match (a, b) {
            (Constant::Real(a_val), Constant::Real(b_val)) => {
                Self::normalize_real(a_val / b_val).map(Constant::Real)
            }
            _ => None,
        }
    }
}
