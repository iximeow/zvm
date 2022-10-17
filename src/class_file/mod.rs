pub mod unvalidated;
pub mod validated;

pub mod synthetic {
    pub use crate::virtual_machine::jvm::synthetic::SyntheticClassBuilder;
}
