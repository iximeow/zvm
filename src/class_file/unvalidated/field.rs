use crate::class_file::unvalidated::AttributeInfo;
use crate::class_file::unvalidated::ConstantIdx;

// TODO: helper to consistency check flags
#[derive(Debug, Clone, Copy)]
pub struct FieldAccessFlags {
    pub flags: u16,
}

#[allow(dead_code)]
impl FieldAccessFlags {
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
    pub fn is_volatile(&self) -> bool {
        (self.flags & 0x0040) == 0x0040
    }
    pub fn is_transient(&self) -> bool {
        (self.flags & 0x0080) == 0x0080
    }
    pub fn is_synthetic(&self) -> bool {
        (self.flags & 0x1000) == 0x1000
    }
    pub fn is_enum(&self) -> bool {
        (self.flags & 0x4000) == 0x4000
    }
}

#[derive(Debug)]
pub struct FieldInfo {
    pub(crate) access_flags: FieldAccessFlags,
    pub(crate) name_index: ConstantIdx,
    pub(crate) descriptor_index: ConstantIdx,
    pub(crate) attributes: Vec<AttributeInfo>,
}
