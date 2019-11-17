use std::rc::Rc;

use crate::class_file::unvalidated::Attribute;
use crate::class_file::unvalidated::AttributeInfo;
use crate::class_file::unvalidated::ConstantIdx;

// TODO: helper to consistency check flags
#[derive(Debug, Clone, Copy)]
pub struct MethodAccessFlags {
    pub flags: u16,
}

#[allow(dead_code)]
impl MethodAccessFlags {
    pub fn is_public(&self) -> bool {
        (self.flags & 0x0001) == 0x0001
    }
    pub fn is_private(&self) -> bool {
        (self.flags & 0x0002) == 0x0002
    }
    pub fn is_protected(&self) -> bool {
        (self.flags & 0x0004) == 0x0004
    }
    pub fn is_static(&self) -> bool {
        (self.flags & 0x0008) == 0x0008
    }
    pub fn is_final(&self) -> bool {
        (self.flags & 0x0010) == 0x0010
    }
    pub fn is_synchronized(&self) -> bool {
        (self.flags & 0x0020) == 0x0020
    }
    pub fn is_bridge(&self) -> bool {
        (self.flags & 0x0040) == 0x0040
    }
    pub fn is_varargs(&self) -> bool {
        (self.flags & 0x0080) == 0x0080
    }
    pub fn is_native(&self) -> bool {
        (self.flags & 0x0100) == 0x0100
    }
    pub fn is_abstract(&self) -> bool {
        (self.flags & 0x0400) == 0x0400
    }
    pub fn is_strict(&self) -> bool {
        (self.flags & 0x0800) == 0x0800
    }
    pub fn is_synthetic(&self) -> bool {
        (self.flags & 0x1000) == 0x1000
    }
}

#[derive(Debug)]
pub struct MethodInfo {
    pub access_flags: MethodAccessFlags,
    pub name_index: ConstantIdx,
    pub descriptor_index: ConstantIdx,
    pub attributes: Vec<AttributeInfo>,
}

#[derive(Debug)]
pub struct MethodHandle {
    pub access_flags: MethodAccessFlags,
    pub name: String,
    pub descriptor: String,
    pub attributes: Vec<Rc<Attribute>>,
}

impl MethodHandle {
    pub fn access(&self) -> &MethodAccessFlags {
        &self.access_flags
    }

    pub fn body(&self) -> Option<Rc<Attribute>> {
        for attr in self.attributes.iter() {
            let attr_ref: &Attribute = &*attr;
            if let Attribute::Code(_, _, _, _, _) = attr_ref {
                return Some(Rc::clone(&attr));
            }
        }

        None
    }
}

#[derive(Debug)]
pub enum MethodHandleBehavior {
    GetField,
    GetStatic,
    PutField,
    PutStatic,
    InvokeVirtual,
    InvokeStatic,
    InvokeSpecial,
    NewInvokeSpecial,
    InvokeInterface,
    Other(u8),
}
