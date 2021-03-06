use crate::virtual_machine::VirtualMachine;
use crate::virtual_machine::VMError;
use crate::virtual_machine::VMState;
use crate::class_file::unvalidated::AttributeInfo;
use crate::class_file::unvalidated::Constant;
use crate::class_file::unvalidated::ConstantIdx;
use crate::class_file::unvalidated::Error;
use crate::class_file::unvalidated::FieldInfo;
use crate::class_file::unvalidated::MethodHandle;
use crate::class_file::unvalidated::MethodInfo;

use std::fmt;
use std::rc::Rc;
use std::hash::{Hash, Hasher};
use std::collections::HashMap;

#[derive(Debug)]
pub struct AccessFlags {
    pub(crate) flags: u16,
}

#[allow(dead_code)]
impl AccessFlags {
    pub fn is_public(&self) -> bool {
        (self.flags & 0x0001) == 0x0001
    }
    pub fn is_final(&self) -> bool {
        (self.flags & 0x0010) == 0x0010
    }
    pub fn is_super(&self) -> bool {
        (self.flags & 0x0020) == 0x0020
    }
    pub fn is_interface(&self) -> bool {
        (self.flags & 0x0200) == 0x0200
    }
    pub fn is_abstract(&self) -> bool {
        (self.flags & 0x0400) == 0x0400
    }
    pub fn is_synthetic(&self) -> bool {
        (self.flags & 0x1000) == 0x1000
    }
    pub fn is_annotation(&self) -> bool {
        (self.flags & 0x2000) == 0x2000
    }
    pub fn is_enum(&self) -> bool {
        (self.flags & 0x4000) == 0x4000
    }
}

/// As cute as a zero-copy class file parse would be, I really don't want to think about DSTs,
/// which are all over the place.
pub struct ClassFile {
    pub(crate) minor_version: u16,
    pub(crate) major_version: u16,
    pub(crate) constant_pool: Vec<Constant>,
    pub(crate) access_flags: AccessFlags,
    pub(crate) this_class: ConstantIdx,
    pub(crate) super_class: Option<ConstantIdx>,
    pub(crate) interfaces: Vec<ConstantIdx>,
    pub(crate) fields: Vec<FieldInfo>,
    pub(crate) methods: Vec<MethodInfo>,
    pub(crate) attributes: Vec<AttributeInfo>,
    pub(crate) native_methods:
        HashMap<String, fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>>,
}

impl fmt::Debug for ClassFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ClassFile {{ major: {}, minor: {}, constants: {:?}, access_flags: {:?}, this_class: {:?}, super_class: {:?}, interfaces: {:?}, fields: {:?}, methods: {:?}, attributes: {:?}, native_methods: {:?} }}",
           self.minor_version,
           self.major_version,
           self.constant_pool,
           self.access_flags,
           self.this_class,
           self.super_class,
           self.interfaces,
           self.fields,
           self.methods,
           self.attributes,
           self.native_methods.keys()
        )
    }
}

pub(crate) struct ClassFileRef(Rc<ClassFile>);

#[allow(dead_code)]
impl ClassFileRef {
    pub fn of(reference: &Rc<ClassFile>) -> Self {
        ClassFileRef(Rc::clone(reference))
    }
}

impl Hash for ClassFileRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe {
            let ptr = Rc::into_raw(Rc::clone(&self.0));
            ptr.hash(state);
            Rc::from_raw(ptr);
        }
    }
}

impl Eq for ClassFileRef {}

impl PartialEq for ClassFileRef {
    fn eq(&self, other: &ClassFileRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl ClassFile {
    pub fn get_str(&self, idx: ConstantIdx) -> Option<&str> {
        if let Some(Constant::Utf8(bytes)) = self.get_const(idx) {
            std::str::from_utf8(bytes).ok()
        } else {
            None
        }
    }

    pub fn get_raw_str(&self, idx: ConstantIdx) -> Option<&[u8]> {
        if let Some(Constant::Utf8(bytes)) = self.get_const(idx) {
            Some(bytes)
        } else {
            None
        }
    }

    pub fn get_const(&self, idx: ConstantIdx) -> Option<&Constant> {
        self.constant_pool.get((idx.inner() - 1) as usize)
    }

    fn display_const(&self, idx: ConstantIdx) -> String {
        format!("{}", self.get_const(idx).expect("valid index").display(self))
    }

    pub fn get_methods(&self, name: &str) -> Result<Vec<Rc<MethodHandle>>, Error> {
        let mut methods = Vec::new();
        for method in self.methods.iter() {
            let method_name = self.get_str(method.name_index).unwrap();
            if method_name == name {
                methods.push(Rc::new(MethodHandle {
                    access_flags: method.access_flags,
                    name: method_name.to_string(),
                    descriptor: self.get_str(method.descriptor_index).unwrap().to_string(),
                    attributes: method
                        .attributes
                        .iter()
                        .map(|attr| Rc::new(attr.materialize(self).unwrap()))
                        .collect(),
                }));
            }
        }

        Ok(methods)
    }

    pub fn get_method(&self, name: &str, desc: &str) -> Result<Rc<MethodHandle>, Error> {
        for method in self.methods.iter() {
            let method_name = self.get_str(method.name_index).unwrap();
            let method_desc = self.get_str(method.descriptor_index).unwrap();
            if method_name == name && method_desc == desc {
                let handle = MethodHandle {
                    access_flags: method.access_flags,
                    name: method_name.to_string(),
                    descriptor: method_desc.to_string(),
                    attributes: method
                        .attributes
                        .iter()
                        .map(|attr| Rc::new(attr.materialize(self).unwrap()))
                        .collect(),
                };

                return Ok(Rc::new(handle));
            }
        }

        Err(Error::Str("Failed to look up method"))
    }

    pub fn has_static_field(&self, name: &str) -> bool {
        for field in self.fields.iter() {
            if field.access_flags.is_static() {
                if self.get_str(field.name_index) == Some(name) {
                    return true;
                }
            }
        }

        false
    }
}

impl fmt::Display for ClassFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "version {}.{} {}",
            self.major_version,
            self.minor_version,
            self.display_const(self.this_class)
        )?;
        writeln!(f, "  {:?}", self.access_flags)?;
        writeln!(f, "  {:?}", self.interfaces)?;
        writeln!(f, "  {:?}", self.fields)?;
        for c in self.constant_pool.iter() {
            writeln!(f, "  const {:?} {}", c, c.display(self),)?;
        }
        for method in self.methods.iter() {
            writeln!(
                f,
                "  method {:?} {}{}",
                method.access_flags,
                self.get_str(method.descriptor_index).unwrap(),
                self.get_str(method.name_index).unwrap()
            )?;
            for attr in method.attributes.iter() {
                if let Ok(attr) = attr.materialize(self) {
                    writeln!(f, "    method attr {}", attr.display(self))?;
                } else {
                    writeln!(
                        f,
                        "    method attr (failed to materialize) {}",
                        attr.display(self)
                    )?;
                }
            }
        }
        for attribute in self.attributes.iter() {
            writeln!(f, "  attribute {}", attribute.display(self))?;
        }
        Ok(())
    }
}
