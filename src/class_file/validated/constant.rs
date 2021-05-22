use std::fmt;

use crate::class_file::unvalidated;
use crate::class_file::unvalidated::{ClassFile as UnvalidatedClassFile};
use crate::class_file::validated::ValidationError;

#[derive(Debug)]
pub enum Constant {
    Class(String),
    String(String),
    Integer(u32),
    Float(f32),
    Long(u64),
    Double(f64),
}

impl Constant {
    pub fn validate(raw_class: &UnvalidatedClassFile, raw_const: &unvalidated::Constant) -> Result<Constant, ValidationError> {
        let v = match raw_const {
            unvalidated::Constant::Integer(v) => Constant::Integer(*v),
            unvalidated::Constant::Float(v) => Constant::Float(*v),
            unvalidated::Constant::Long(v) => Constant::Long(*v),
            unvalidated::Constant::Double(v) => Constant::Double(*v),
            unvalidated::Constant::String(idx) => {
                Constant::String(raw_class.get_str(*idx).unwrap().to_string())
            }
            _ => { return Err(ValidationError::BadString) }
        };

        Ok(v)
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Class(v) => write!(f, "class<{}>", v),
            Constant::String(v) => write!(f, "{}", v),
            Constant::Integer(v) => write!(f, "{}", v),
            Constant::Float(v) => write!(f, "{}", v),
            Constant::Long(v) => write!(f, "{}", v),
            Constant::Double(v) => write!(f, "{}", v),
        }
    }
}
