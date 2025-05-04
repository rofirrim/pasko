use std::hash::Hash;
use std::sync::Mutex;

lazy_static! {
    static ref GLOBAL_ID: Mutex<usize> = Mutex::new(0usize);
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Identifier(usize);

fn next_id() -> usize {
    let mut c = GLOBAL_ID.lock().unwrap();
    let result = *c;
    *c += 1;
    result
}

pub fn new_id() -> Identifier {
    Identifier(next_id())
}

// Use this only for debugging.
pub fn to_id(id: usize) -> Identifier {
    Identifier(id)
}

impl Identifier {
    // Use this only for debugging purposes.
    pub fn get_number(&self) -> usize {
        self.0
    }
}
