use crate::class_file::unvalidated;
use crate::class_file::unvalidated::{ClassFile as UnvalidatedClassFile};
use crate::class_file::validated::ValidationError;

#[derive(Debug)]
pub struct FieldRef {
    pub(crate) class_name: String,
    pub(crate) name: String,
    pub(crate) desc: String,
}

#[derive(Debug)]
pub struct FieldHandle {
    // TODO:
    pub(crate) access_flags: unvalidated::FieldAccessFlags,
    pub(crate) name: String,
    pub(crate) desc: String,
    // flags, attributes, ..
}

impl FieldHandle {
    pub fn validate(raw_class: &UnvalidatedClassFile, raw_field: &unvalidated::FieldInfo) -> Result<FieldHandle, ValidationError> {
        let name = raw_class.checked_const(raw_field.name_index).and_then(|c| c.as_utf8())?.to_string();
        let desc = raw_class.checked_const(raw_field.descriptor_index).and_then(|c| c.as_utf8())?.to_string();
        Ok(FieldHandle { access_flags: raw_field.access_flags, name, desc })
    }
}
