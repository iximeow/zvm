use std::collections::HashMap;
use std::hash::{Hasher, Hash};
use std::io::Cursor;
use std::rc::Rc;
use std::fmt;

use crate::class_file::unvalidated;
use crate::class_file::unvalidated::AttributeInfo;
use crate::class_file::unvalidated::ConstantIdx;
use crate::class_file::unvalidated::attribute::ExceptionTableRecord;
use crate::class_file::unvalidated::{ClassFile as UnvalidatedClassFile};
use crate::class_file::unvalidated::read::FromReader;

mod instruction;
pub use instruction::Instruction;

pub struct MethodRef<'cls> {
    class_name: &'cls str,
    name: &'cls str,
    desc: &'cls str,
}

pub struct FieldRef<'cls> {
    class_name: &'cls str,
    name: &'cls str,
    desc: &'cls str,
}

pub struct MethodBody<'cls> {
    max_stack: u16,
    max_locals: u16,
    bytes: &'cls [u8],
    // TODO: validate exception table records
    exception_info: Vec<unvalidated::attribute::ExceptionTableRecord>,
    field_refs: HashMap<u32, FieldRef<'cls>>,
    method_refs: HashMap<u32, MethodRef<'cls>>,
    constants: HashMap<u32, Constant<'cls>>,
    // TODO: method attributes
}

pub struct MethodHandle<'cls> {
    name: &'cls str,
    desc: &'cls str,
    body: Option<MethodBody<'cls>>,
    // flags, attributes, ..
}

pub struct MethodInstrIter<'handle, 'cls> {
    handle: &'handle MethodBody<'cls>,
    offset: usize,
}

impl<'body, 'cls> Iterator for MethodInstrIter<'body, 'cls> {
    type Item = Instruction<'body, 'cls>;

    fn next(&mut self) -> Option<Instruction<'body, 'cls>> {
        let mut cursor = Cursor::new(&self.handle.bytes[self.offset..]);
        let raw_inst = match unvalidated::Instruction::read_from(&mut cursor) {
            Ok(inst) => inst,
            Err(_) => { return None; }
        };

        self.offset += cursor.position() as usize;

        None
    }
}

fn make_refs<'validation, 'cls>(
    inst: &unvalidated::Instruction,
    position: u32,
    raw_class: &'cls UnvalidatedClassFile,
    field_refs: &'validation mut HashMap<u32, FieldRef<'cls>>,
    method_refs: &'validation mut HashMap<u32, MethodRef<'cls>>,
    constants: &'validation mut HashMap<u32, Constant<'cls>>
) -> Result<(), ValidationError<'cls>> {
    use unvalidated::Instruction;
    match inst {
        Instruction::IStore(local_idx) |
        Instruction::LStore(local_idx) |
        Instruction::FStore(local_idx) |
        Instruction::DStore(local_idx) |
        Instruction::AStore(local_idx) |
        Instruction::ILoad(local_idx) |
        Instruction::LLoad(local_idx) |
        Instruction::FLoad(local_idx) |
        Instruction::DLoad(local_idx) |
        Instruction::ALoad(local_idx) => {
            // TODO: check that local_idx < max_locals
        }
        Instruction::Ldc(const_idx) |
        Instruction::LdcW(const_idx) => {
            match raw_class.checked_const(*const_idx)? {
                unvalidated::Constant::String(idx) => {
                    let s = raw_class.checked_const(*idx)?.as_utf8()?;
                    constants.insert(position as u32, Constant::String(s));
                }
                unvalidated::Constant::Integer(i) => {
                    constants.insert(position as u32, Constant::Integer(*i));
                }
                unvalidated::Constant::Float(f) => {
                    constants.insert(position as u32, Constant::Float(*f));
                }
                c => {
                    return Err(ValidationError::BadConst(c.type_name(), "String, Integer, or Float"));
                }
            }
        }
        Instruction::Ldc2W(const_idx) => {
            match raw_class.checked_const(*const_idx)? {
                unvalidated::Constant::Long(l) => {
                    constants.insert(position as u32, Constant::Long(*l));
                }
                unvalidated::Constant::Double(d) => {
                    constants.insert(position as u32, Constant::Double(*d));
                }
                c => {
                    return Err(ValidationError::BadConst(c.type_name(), "Long or Double"));
                }
            }
        }
        Instruction::PutStatic(field_idx) |
        Instruction::PutField(field_idx) |
        Instruction::GetStatic(field_idx) |
        Instruction::GetField(field_idx) => {
            if let Some(unvalidated::Constant::Fieldref(class_idx, name_and_type_idx)) =
                raw_class.get_const(*field_idx)
            {
                let referent_class_name = match raw_class.checked_const(*class_idx)? {
                    unvalidated::Constant::Class(class_name_idx) => {
                        raw_class.get_str(*class_name_idx).unwrap()
                    }
                    o => {
                        return Err(ValidationError::BadConst(o.type_name(), "Class"));
                    }
                };

                // and now the name/type
                let (name, desc) = match raw_class.checked_const(*name_and_type_idx)? {
                    unvalidated::Constant::NameAndType(name_idx, type_idx) => {
                        (
                            raw_class.get_str(*name_idx).unwrap(),
                            raw_class.get_str(*type_idx).unwrap(),
                        )
                    }
                    o => {
                        return Err(ValidationError::BadConst(o.type_name(), "NameAndType"));
                    }
                };
                let field_ref = FieldRef {
                    class_name: referent_class_name,
                    name,
                    desc,
                };
                // TODO: check position < u32::max
                field_refs.insert(position as u32, field_ref);
            }
        }
        Instruction::InvokeVirtual(method_idx) |
        Instruction::InvokeSpecial(method_idx) |
        Instruction::InvokeStatic(method_idx) => {
            panic!("invoke* is not yet validated");
        }
        Instruction::InvokeInterface(method_idx, count) => {
            panic!("invokeinterface is not yet validated");
        }
        Instruction::InvokeDynamic(method_idx) => {
            panic!("invokedynamic is not yet validated");
        }
        Instruction::New(class_idx) => {
            panic!("new not yet validated");
        }
        Instruction::NewArray(tpe) => {
            panic!("newarray not yet validated");
        }
        Instruction::CheckCast(idx) => {
            panic!("checkcast not yet validated");
        }
        _ => { /* no validation necessary */ }
    }

    Ok(())
}

impl<'cls> MethodHandle<'cls> {
    fn validate(raw_class: &'cls UnvalidatedClassFile, raw_method: &'cls unvalidated::MethodInfo) -> Result<MethodHandle<'cls>, ValidationError<'cls>> {
        let name = raw_class.checked_const(raw_method.name_index).and_then(|c| c.as_utf8())?;
        let desc = raw_class.checked_const(raw_method.descriptor_index).and_then(|c| c.as_utf8())?;
        let mut code_attrs = raw_method.attributes.iter().filter(|a| raw_class.get_str(a.name_index) == Some("Code"));
        let code_attr = code_attrs.next();
        if code_attrs.next().is_some() {
            return Err(ValidationError::InvalidMethod("Multiple code attributes"));
        }
        let body = match code_attr {
            Some(attr) => {
                let data = &mut attr.data.as_slice();
                let max_stack = u16::read_from(data).unwrap();
                let max_locals = u16::read_from(data).unwrap();
                let code_length = u32::read_from(data).unwrap();
                let body_start = attr.data.len() - data.len();
                for _ in 0..code_length {
                    u8::read_from(data).unwrap();
                }
                let exceptions_length = u16::read_from(data).unwrap();
                let mut exceptions: Vec<ExceptionTableRecord> = Vec::new();
                for _ in 0..exceptions_length {
                    exceptions.push(ExceptionTableRecord::read_from(data).unwrap());
                }
                let attr_length = u16::read_from(data).unwrap();
                let mut attrs: Vec<AttributeInfo> = Vec::new();
                for _ in 0..attr_length {
                    attrs.push(AttributeInfo::read_from(data).unwrap());
                }

                let method_body = &attr.data[(body_start as usize)..][..(code_length as usize)];
                let mut body_cursor = Cursor::new(method_body);

                let mut field_refs = HashMap::new();
                let mut method_refs = HashMap::new();
                let mut constants = HashMap::new();

                while let Ok(inst) = unvalidated::Instruction::read_from(&mut body_cursor) {
                    make_refs(&inst, body_cursor.position() as u32, raw_class, &mut field_refs, &mut method_refs, &mut constants)?;
                }

                Some(MethodBody {
                    max_stack,
                    max_locals,
                    bytes: method_body,
                    exception_info: exceptions,
                    field_refs,
                    method_refs,
                    constants,
                })
            },
            None => None
        };

        Ok(MethodHandle { name, desc, body })
    }
}

struct FieldHandle<'cls> {
    name: &'cls str,
    desc: &'cls str,
    // flags, attributes, ..
}

pub enum Constant<'cls> {
    String(&'cls str),
    Integer(u32),
    Float(f32),
    Long(u64),
    Double(f64),
}

impl<'cls> fmt::Display for Constant<'cls> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::String(v) => write!(f, "{}", v),
            Constant::Integer(v) => write!(f, "{}", v),
            Constant::Float(v) => write!(f, "{}", v),
            Constant::Long(v) => write!(f, "{}", v),
            Constant::Double(v) => write!(f, "{}", v),
        }
    }
}

pub struct ClassFile<'cls> {
    this_class: &'cls str,
    super_class: Option<&'cls str>,
    constants: Vec<Constant<'cls>>,
    interfaces: Vec<&'cls str>,
    fields: Vec<FieldHandle<'cls>>,
    methods: Vec<MethodHandle<'cls>>,
    // currently support no attributes on classes
    attributes: Vec<()>,
}

pub enum ValidationError<'cls> {
    BadString,
    BadIndex(ConstantIdx),
    BadConst(&'cls str, &'cls str),
    Unimplemented,
    InvalidMethod(&'static str),
    BadAttribute(&'static str),
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
        for raw_method in raw_class.methods.iter() {
            methods.push(MethodHandle::validate(raw_class, raw_method)?);
        }
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

