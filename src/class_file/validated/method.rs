use crate::class_file::unvalidated::read::FromReader;
use crate::class_file::unvalidated;
use crate::class_file::unvalidated::AttributeInfo;
use crate::class_file::unvalidated::{ClassFile as UnvalidatedClassFile};
use crate::class_file::unvalidated::ExceptionTableRecord;
use crate::class_file::validated::validate_inst;
use crate::class_file::validated::Constant;
use crate::class_file::validated::FieldRef;
use crate::class_file::validated::Instruction;
use crate::class_file::validated::ValidationError;

use std::collections::HashMap;
use std::rc::Rc;
use std::io::Cursor;

#[derive(Debug)]
pub struct MethodRef {
    pub(crate) class_name: String,
    pub(crate) name: String,
    pub(crate) desc: String,
}

#[derive(Debug)]
pub struct MethodBody {
    max_stack: u16,
    pub(crate) max_locals: u16,
    bytes: Box<[u8]>,
    // TODO: validate exception table records
    exception_info: Vec<unvalidated::attribute::ExceptionTableRecord>,
    pub(crate) class_refs: HashMap<u32, Rc<String>>,
    pub(crate) field_refs: HashMap<u32, Rc<FieldRef>>,
    pub(crate) method_refs: HashMap<u32, Rc<MethodRef>>,
    pub(crate) const_refs: HashMap<u32, Rc<Constant>>,
    // TODO: method attributes
}

impl MethodBody {
    pub fn iter_from(&self, offset: u32) -> MethodInstrIter {
        MethodInstrIter {
            handle: self,
            offset: offset as usize
        }
    }
}

#[derive(Debug)]
pub struct MethodHandle {
    // TODO:
    access_flags: unvalidated::MethodAccessFlags,
    pub(crate) name: String,
    pub(crate) desc: String,
    pub(crate) body: Option<Rc<MethodBody>>,
    // flags, attributes, ..
}

pub struct MethodInstrIter<'handle> {
    handle: &'handle MethodBody,
    pub(crate) offset: usize,
}

impl<'handle> Iterator for MethodInstrIter<'handle> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Instruction> {
        let mut cursor = Cursor::new(&self.handle.bytes[self.offset..]);
        let raw_inst = match unvalidated::Instruction::read_from(&mut cursor) {
            Ok(inst) => inst,
            Err(_) => { return None; }
        };

        let next = validate_inst(self.handle, self.offset as u32, &raw_inst);

        self.offset += cursor.position() as usize;

        next
    }
}

fn make_refs<'validation>(
    inst: &unvalidated::Instruction,
    max_locals: u16,
    position: u32,
    raw_class: &UnvalidatedClassFile,
    class_refs: &'validation mut HashMap<u32, Rc<String>>,
    field_refs: &'validation mut HashMap<u32, Rc<FieldRef>>,
    method_refs: &'validation mut HashMap<u32, Rc<MethodRef>>,
    const_refs: &'validation mut HashMap<u32, Rc<Constant>>
) -> Result<(), ValidationError> {
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
            if *local_idx >= max_locals {
                return Err(ValidationError::InvalidMethod("invalid local index"));
            }
        }
        Instruction::New(class_idx) |
        Instruction::CheckCast(class_idx) |
        Instruction::InstanceOf(class_idx) => {
            match raw_class.checked_const(*class_idx)? {
                unvalidated::Constant::Class(idx) => {
                    class_refs.insert(position as u32, Rc::new(raw_class.get_str(*idx).unwrap().to_string()));
                }
                _ => { panic!("bad idx"); }
            }
        },
        Instruction::Ldc(const_idx) |
        Instruction::LdcW(const_idx) => {
            match raw_class.checked_const(*const_idx)? {
                unvalidated::Constant::String(idx) => {
                    let s = raw_class.checked_const(*idx)?.as_utf8()?;
                    const_refs.insert(position as u32, Rc::new(Constant::String(s.to_string())));
                }
                unvalidated::Constant::Integer(i) => {
                    const_refs.insert(position as u32, Rc::new(Constant::Integer(*i)));
                }
                unvalidated::Constant::Float(f) => {
                    const_refs.insert(position as u32, Rc::new(Constant::Float(*f)));
                }
                c => {
                    return Err(ValidationError::BadConst(c.type_name().to_string(), "String, Integer, or Float".to_string()));
                }
            }
        }
        Instruction::Ldc2W(const_idx) => {
            match raw_class.checked_const(*const_idx)? {
                unvalidated::Constant::Long(l) => {
                    const_refs.insert(position as u32, Rc::new(Constant::Long(*l)));
                }
                unvalidated::Constant::Double(d) => {
                    const_refs.insert(position as u32, Rc::new(Constant::Double(*d)));
                }
                c => {
                    return Err(ValidationError::BadConst(c.type_name().to_string(), "Long or Double".to_string()));
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
                        return Err(ValidationError::BadConst(o.type_name().to_string(), "Class".to_string()));
                    }
                };

                // and now the name/type
                let (name, desc) = match raw_class.checked_const(*name_and_type_idx)? {
                    unvalidated::Constant::NameAndType(name_idx, type_idx) => {
                        (
                            raw_class.get_str(*name_idx).unwrap().to_string(),
                            raw_class.get_str(*type_idx).unwrap().to_string(),
                        )
                    }
                    o => {
                        return Err(ValidationError::BadConst(o.type_name().to_string(), "NameAndType".to_string()));
                    }
                };
                let field_ref = FieldRef {
                    class_name: referent_class_name.to_string(),
                    name,
                    desc,
                };
                // TODO: check position < u32::max
                field_refs.insert(position as u32, Rc::new(field_ref));
            }
        }
        Instruction::InvokeVirtual(method_idx) |
        Instruction::InvokeSpecial(method_idx) |
        Instruction::InvokeStatic(method_idx) => {
            if let Some(unvalidated::Constant::Methodref(class_idx, name_and_type_idx)) =
                raw_class.get_const(*method_idx)
            {
                let referent_class_name = match raw_class.checked_const(*class_idx)? {
                    unvalidated::Constant::Class(class_name_idx) => {
                        raw_class.get_str(*class_name_idx).unwrap()
                    }
                    o => {
                        return Err(ValidationError::BadConst(o.type_name().to_string(), "Class".to_string()));
                    }
                };

                // and now the name/type
                let (name, desc) = match raw_class.checked_const(*name_and_type_idx)? {
                    unvalidated::Constant::NameAndType(name_idx, type_idx) => {
                        (
                            raw_class.get_str(*name_idx).unwrap().to_string(),
                            raw_class.get_str(*type_idx).unwrap().to_string(),
                        )
                    }
                    o => {
                        return Err(ValidationError::BadConst(o.type_name().to_string(), "NameAndType".to_string()));
                    }
                };
                let method_ref = MethodRef {
                    class_name: referent_class_name.to_string(),
                    name,
                    desc,
                };
                // TODO: check position < u32::max
                method_refs.insert(position as u32, Rc::new(method_ref));
            }
        }
        Instruction::InvokeInterface(_method_idx, _count) => {
//            panic!("invokeinterface is not yet validated");
        }
        Instruction::InvokeDynamic(_method_idx) => {
            panic!("invokedynamic is not yet validated");
        }
        Instruction::NewArray(_tpe) => {
            panic!("newarray not yet validated");
        }
        _ => { /* no validation necessary */ }
    }

    Ok(())
}

impl MethodHandle {
    pub fn access(&self) -> &unvalidated::MethodAccessFlags {
        &self.access_flags
    }

    pub fn validate(raw_class: &UnvalidatedClassFile, raw_method: &unvalidated::MethodInfo) -> Result<MethodHandle, ValidationError> {
        let name = raw_class.checked_const(raw_method.name_index).and_then(|c| c.as_utf8())?.to_string();
        let desc = raw_class.checked_const(raw_method.descriptor_index).and_then(|c| c.as_utf8())?.to_string();
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

                let mut class_refs = HashMap::new();
                let mut field_refs = HashMap::new();
                let mut method_refs = HashMap::new();
                let mut const_refs = HashMap::new();

                loop {
                    let position = body_cursor.position() as u32;
                    let inst = match unvalidated::Instruction::read_from(&mut body_cursor) {
                        Ok(inst) => inst,
                        _ => { break; }
                    };
                    make_refs(&inst, max_locals, position, raw_class, &mut class_refs, &mut field_refs, &mut method_refs, &mut const_refs)?;
                }

                Some(Rc::new(MethodBody {
                    max_stack,
                    max_locals,
                    bytes: method_body.to_vec().into_boxed_slice(),
                    exception_info: exceptions,
                    class_refs,
                    field_refs,
                    method_refs,
                    const_refs,
                }))
            },
            None => None
        };

        Ok(MethodHandle { access_flags: raw_method.access_flags, name, desc, body })
    }
}
