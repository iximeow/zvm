pub mod attribute;
pub mod instruction;
pub mod read;
pub mod class;
pub mod field;
pub mod method;
pub mod constant;

pub use instruction::Instruction;

use crate::class_file::unvalidated::attribute::Attribute;

pub use crate::class_file::unvalidated::attribute::AttributeInfo;
pub use crate::class_file::unvalidated::attribute::ExceptionTableRecord;
pub use crate::class_file::unvalidated::class::AccessFlags;
pub use crate::class_file::unvalidated::class::ClassFile;
pub use crate::class_file::unvalidated::constant::Constant;
pub use crate::class_file::unvalidated::constant::ConstantIdx;
pub use crate::class_file::unvalidated::field::FieldAccessFlags;
pub use crate::class_file::unvalidated::field::FieldInfo;
pub use crate::class_file::unvalidated::method::MethodAccessFlags;
pub use crate::class_file::unvalidated::method::MethodHandle;
pub use crate::class_file::unvalidated::method::MethodHandleBehavior;
pub use crate::class_file::unvalidated::method::MethodInfo;

#[derive(Debug)]
pub enum Error {
    BadMagic,
    BadIndex,
    ClassFileError(&'static str),
    EOF,
    BadInstruction(u8, bool),
    Str(&'static str),
    Unsupported(&'static str),
}

impl From<std::io::Error> for Error {
    fn from(_err: std::io::Error) -> Self {
        // TODO handle errors that aren't _actually_ end of file?
        Error::EOF
    }
}
