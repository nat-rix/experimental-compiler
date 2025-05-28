pub struct Codegen {
    cursor: u64,
}

impl Codegen {
    pub fn new(entry: u64) -> Self {
        Self { cursor: entry }
    }
}
