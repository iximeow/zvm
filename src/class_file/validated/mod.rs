use std::hash::{Hasher, Hash};
use std::rc::Rc;

use crate::class_file::unvalidated;
use crate::class_file::unvalidated::ConstantIdx;
use crate::class_file::unvalidated::{ClassFile as UnvalidatedClassFile};

mod instruction;
pub use instruction::Instruction;

struct MethodRef<'cls> {
    class_name: &'cls str,
    name: &'cls str,
    desc: &'cls str,
}

struct MethodHandle<'cls> {
    method_name: &'cls str,
    method_body: Option<&'cls [u8]>,
}

struct FieldRef<'cls> {
    class_name: &'cls str,
    name: &'cls str,
    desc: &'cls str,
}

pub enum Constant<'cls> {
    String(&'cls str),
    Integer(u32),
    Float(f32),
    Long(u64),
    Double(f64),
}

pub struct ClassFile<'cls> {
    this_class: &'cls str,
    super_class: Option<&'cls str>,
    constants: Vec<Constant<'cls>>,
    interfaces: Vec<&'cls str>,
    fields: Vec<FieldRef<'cls>>,
    methods: Vec<MethodHandle<'cls>>,
    // currently support no attributes on classes
    attributes: Vec<()>,
}

pub enum ValidationError<'cls> {
    BadString,
    BadIndex(ConstantIdx),
    BadConst(&'cls str, &'cls str),
    Unimplemented
}

impl unvalidated::Constant {
    fn as_utf8(&self) -> Result<&str, ValidationError> {
        if let unvalidated::Constant::Utf8(data) = self {
            if let Some(s) = std::str::from_utf8(data).ok() {
                Ok(s)
            } else {
                Err(ValidationError::BadString)
            }
        } else {
            Err(ValidationError::BadConst(self.type_name(), "Utf8"))
        }
    }
}

impl UnvalidatedClassFile {
    fn checked_const<'cls>(&'cls self, idx: ConstantIdx) -> Result<&'cls unvalidated::Constant, ValidationError> {
        self.get_const(self.this_class)
            .ok_or(ValidationError::BadIndex(self.this_class))
    }
}

impl<'cls> ClassFile<'cls> {
    pub fn validate(raw_class: &'cls UnvalidatedClassFile) -> Result<ClassFile<'cls>, ValidationError<'cls>> {
        // first, `this_class` must be a constant index to a Utf8, containing a well-formed type
        // descriptor.
        // TODO: verify type descriptor
        let this_class = raw_class.checked_const(raw_class.this_class).and_then(|c| c.as_utf8())?;
        let super_class = match raw_class.super_class.map(|sup| {
            raw_class.checked_const(sup).and_then(|c| c.as_utf8())
        }) {
            Some(Ok(sup)) => Some(sup),
            Some(Err(e)) => { return Err(e); },
            None => None
        };

        // TODO
        let mut constants = Vec::new();
        let mut interfaces = Vec::new();
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut attributes = Vec::new();

        Ok(ClassFile {
            this_class,
            super_class,
            constants,
            interfaces,
            fields,
            methods,
            attributes,
        })
    }
}

pub(crate) struct ClassFileRef<'cls>(Rc<ClassFile<'cls>>);

impl<'cls> ClassFileRef<'cls> {
    pub fn of(reference: &Rc<ClassFile<'cls>>) -> Self {
        ClassFileRef(Rc::clone(reference))
    }
}

impl<'cls> Hash for ClassFileRef<'cls> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe {
            let ptr = Rc::into_raw(Rc::clone(&self.0));
            ptr.hash(state);
            Rc::from_raw(ptr);
        }
    }
}

impl<'cls> Eq for ClassFileRef<'cls> {}

impl<'cls> PartialEq for ClassFileRef<'cls> {
    fn eq(&self, other: &ClassFileRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

