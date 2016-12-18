#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum DropFlagState {
    Present, // i.e. initialized
    Absent, // i.e. deinitialized or "moved"
}

impl DropFlagState {
    fn value(self) -> bool {
        match self {
            DropFlagState::Present => true,
            DropFlagState::Absent => false
        }
    }
}
