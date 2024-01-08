use std::convert::From;

#[derive(Debug, Clone)]
pub enum Constant {
    // FIXME: we use isize where we should use i64
    Integer(isize),
    Real(f64),
    Bool(bool),
}

impl From<isize> for Constant {
    fn from(v: isize) -> Constant {
        Constant::Integer(v)
    }
}

impl From<f64> for Constant {
    fn from(v: f64) -> Constant {
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
