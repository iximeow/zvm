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

use crate::virtual_machine::VMError;
use crate::virtual_machine::VMState;
use crate::virtual_machine::VirtualMachine;

mod instruction;
pub use instruction::Instruction;

#[derive(Debug)]
pub struct MethodRef {
    pub(crate) class_name: String,
    pub(crate) name: String,
    pub(crate) desc: String,
}

#[derive(Debug)]
pub struct FieldRef {
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
    class_refs: HashMap<u32, Rc<String>>,
    field_refs: HashMap<u32, Rc<FieldRef>>,
    method_refs: HashMap<u32, Rc<MethodRef>>,
    const_refs: HashMap<u32, Rc<Constant>>,
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

fn validate_inst(handle: &MethodBody, position: u32, raw_inst: &unvalidated::Instruction) -> Option<Instruction> {
    let res = match raw_inst {
        unvalidated::Instruction::Nop => Instruction::Nop,
        unvalidated::Instruction::AConstNull => Instruction::AConstNull,
        unvalidated::Instruction::IConstM1 => Instruction::IConstM1,
        unvalidated::Instruction::IConst0 => Instruction::IConst0,
        unvalidated::Instruction::IConst1 => Instruction::IConst1,
        unvalidated::Instruction::IConst2 => Instruction::IConst2,
        unvalidated::Instruction::IConst3 => Instruction::IConst3,
        unvalidated::Instruction::IConst4 => Instruction::IConst4,
        unvalidated::Instruction::IConst5 => Instruction::IConst5,
        unvalidated::Instruction::LConst0 => Instruction::LConst0,
        unvalidated::Instruction::LConst1 => Instruction::LConst1,
        unvalidated::Instruction::FConst0 => Instruction::FConst0,
        unvalidated::Instruction::FConst1 => Instruction::FConst1,
        unvalidated::Instruction::FConst2 => Instruction::FConst2,
        unvalidated::Instruction::DConst0 => Instruction::DConst0,
        unvalidated::Instruction::DConst1 => Instruction::DConst1,
        unvalidated::Instruction::BIPush(v) => Instruction::BIPush(*v),
        unvalidated::Instruction::SIPush(v) => Instruction::SIPush(*v),
        unvalidated::Instruction::Ldc(v) => Instruction::Ldc(Rc::clone(&handle.const_refs[&position])),
        unvalidated::Instruction::LdcW(v) => Instruction::LdcW(Rc::clone(&handle.const_refs[&position])),
        unvalidated::Instruction::Ldc2W(v) => Instruction::Ldc2W(Rc::clone(&handle.const_refs[&position])),
        unvalidated::Instruction::ILoad(v) => Instruction::ILoad(*v),
        unvalidated::Instruction::LLoad(v) => Instruction::LLoad(*v),
        unvalidated::Instruction::FLoad(v) => Instruction::FLoad(*v),
        unvalidated::Instruction::DLoad(v) => Instruction::DLoad(*v),
        unvalidated::Instruction::ALoad(v) => Instruction::ALoad(*v),
        unvalidated::Instruction::ILoad0 => Instruction::ILoad0,
        unvalidated::Instruction::ILoad1 => Instruction::ILoad1,
        unvalidated::Instruction::ILoad2 => Instruction::ILoad2,
        unvalidated::Instruction::ILoad3 => Instruction::ILoad3,
        unvalidated::Instruction::LLoad0 => Instruction::LLoad0,
        unvalidated::Instruction::LLoad1 => Instruction::LLoad1,
        unvalidated::Instruction::LLoad2 => Instruction::LLoad2,
        unvalidated::Instruction::LLoad3 => Instruction::LLoad3,
        unvalidated::Instruction::FLoad0 => Instruction::FLoad0,
        unvalidated::Instruction::FLoad1 => Instruction::FLoad1,
        unvalidated::Instruction::FLoad2 => Instruction::FLoad2,
        unvalidated::Instruction::FLoad3 => Instruction::FLoad3,
        unvalidated::Instruction::DLoad0 => Instruction::DLoad0,
        unvalidated::Instruction::DLoad1 => Instruction::DLoad1,
        unvalidated::Instruction::DLoad2 => Instruction::DLoad2,
        unvalidated::Instruction::DLoad3 => Instruction::DLoad3,
        unvalidated::Instruction::ALoad0 => Instruction::ALoad0,
        unvalidated::Instruction::ALoad1 => Instruction::ALoad1,
        unvalidated::Instruction::ALoad2 => Instruction::ALoad2,
        unvalidated::Instruction::ALoad3 => Instruction::ALoad3,
        unvalidated::Instruction::IAStore => Instruction::IAStore,
        unvalidated::Instruction::IALoad => Instruction::IALoad,
        unvalidated::Instruction::LALoad => Instruction::LALoad,
        unvalidated::Instruction::FALoad => Instruction::FALoad,
        unvalidated::Instruction::DALoad => Instruction::DALoad,
        unvalidated::Instruction::AALoad => Instruction::AALoad,
        unvalidated::Instruction::BALoad => Instruction::BALoad,
        unvalidated::Instruction::CALoad => Instruction::CALoad,
        unvalidated::Instruction::SALoad => Instruction::SALoad,
        unvalidated::Instruction::IStore(v) => Instruction::IStore(*v),
        unvalidated::Instruction::LStore(v) => Instruction::LStore(*v),
        unvalidated::Instruction::FStore(v) => Instruction::FStore(*v),
        unvalidated::Instruction::DStore(v) => Instruction::DStore(*v),
        unvalidated::Instruction::AStore(v) => Instruction::AStore(*v),
        unvalidated::Instruction::IStore0 => Instruction::IStore0,
        unvalidated::Instruction::IStore1 => Instruction::IStore1,
        unvalidated::Instruction::IStore2 => Instruction::IStore2,
        unvalidated::Instruction::IStore3 => Instruction::IStore3,
        unvalidated::Instruction::LStore0 => Instruction::LStore0,
        unvalidated::Instruction::LStore1 => Instruction::LStore1,
        unvalidated::Instruction::LStore2 => Instruction::LStore2,
        unvalidated::Instruction::LStore3 => Instruction::LStore3,
        unvalidated::Instruction::FStore0 => Instruction::FStore0,
        unvalidated::Instruction::FStore1 => Instruction::FStore1,
        unvalidated::Instruction::FStore2 => Instruction::FStore2,
        unvalidated::Instruction::FStore3 => Instruction::FStore3,
        unvalidated::Instruction::DStore0 => Instruction::DStore0,
        unvalidated::Instruction::DStore1 => Instruction::DStore1,
        unvalidated::Instruction::DStore2 => Instruction::DStore2,
        unvalidated::Instruction::DStore3 => Instruction::DStore3,
        unvalidated::Instruction::AStore0 => Instruction::AStore0,
        unvalidated::Instruction::AStore1 => Instruction::AStore1,
        unvalidated::Instruction::AStore2 => Instruction::AStore2,
        unvalidated::Instruction::AStore3 => Instruction::AStore3,
        unvalidated::Instruction::LAStore => Instruction::LAStore,
        unvalidated::Instruction::FAStore => Instruction::FAStore,
        unvalidated::Instruction::DAStore => Instruction::DAStore,
        unvalidated::Instruction::AAStore => Instruction::AAStore,
        unvalidated::Instruction::BAStore => Instruction::BAStore,
        unvalidated::Instruction::CAStore => Instruction::CAStore,
        unvalidated::Instruction::SAStore => Instruction::SAStore,
        unvalidated::Instruction::Pop => Instruction::Pop,
        unvalidated::Instruction::Pop2 => Instruction::Pop2,
        unvalidated::Instruction::Dup => Instruction::Dup,
        unvalidated::Instruction::DupX1 => Instruction::DupX1,
        unvalidated::Instruction::DupX2 => Instruction::DupX2,
        unvalidated::Instruction::Dup2 => Instruction::Dup2,
        unvalidated::Instruction::Dup2X1 => Instruction::Dup2X1,
        unvalidated::Instruction::Dup2X2 => Instruction::Dup2X2,
        unvalidated::Instruction::Swap => Instruction::Swap,
        unvalidated::Instruction::IAdd => Instruction::IAdd,
        unvalidated::Instruction::LAdd => Instruction::LAdd,
        unvalidated::Instruction::FAdd => Instruction::FAdd,
        unvalidated::Instruction::DAdd => Instruction::DAdd,
        unvalidated::Instruction::ISub => Instruction::ISub,
        unvalidated::Instruction::LSub => Instruction::LSub,
        unvalidated::Instruction::FSub => Instruction::FSub,
        unvalidated::Instruction::DSub => Instruction::DSub,
        unvalidated::Instruction::IMul => Instruction::IMul,
        unvalidated::Instruction::LMul => Instruction::LMul,
        unvalidated::Instruction::FMul => Instruction::FMul,
        unvalidated::Instruction::DMul => Instruction::DMul,
        unvalidated::Instruction::IDiv => Instruction::IDiv,
        unvalidated::Instruction::LDiv => Instruction::LDiv,
        unvalidated::Instruction::FDiv => Instruction::FDiv,
        unvalidated::Instruction::DDiv => Instruction::DDiv,
        unvalidated::Instruction::IRem => Instruction::IRem,
        unvalidated::Instruction::LRem => Instruction::LRem,
        unvalidated::Instruction::FRem => Instruction::FRem,
        unvalidated::Instruction::DRem => Instruction::DRem,
        unvalidated::Instruction::INeg => Instruction::INeg,
        unvalidated::Instruction::LNeg => Instruction::LNeg,
        unvalidated::Instruction::FNeg => Instruction::FNeg,
        unvalidated::Instruction::DNeg => Instruction::DNeg,
        unvalidated::Instruction::IShl => Instruction::IShl,
        unvalidated::Instruction::LShl => Instruction::LShl,
        unvalidated::Instruction::IShr => Instruction::IShr,
        unvalidated::Instruction::LShr => Instruction::LShr,
        unvalidated::Instruction::IUshr => Instruction::IUshr,
        unvalidated::Instruction::LUshr => Instruction::LUshr,
        unvalidated::Instruction::IAnd => Instruction::IAnd,
        unvalidated::Instruction::LAnd => Instruction::LAnd,
        unvalidated::Instruction::IOr => Instruction::IOr,
        unvalidated::Instruction::LOr => Instruction::LOr,
        unvalidated::Instruction::IXor => Instruction::IXor,
        unvalidated::Instruction::LXor => Instruction::LXor,
        unvalidated::Instruction::IInc(v1, v2) => Instruction::IInc(*v1, *v2),
        unvalidated::Instruction::I2L => Instruction::I2L,
        unvalidated::Instruction::I2F => Instruction::I2F,
        unvalidated::Instruction::I2D => Instruction::I2D,
        unvalidated::Instruction::L2I => Instruction::L2I,
        unvalidated::Instruction::L2F => Instruction::L2F,
        unvalidated::Instruction::L2D => Instruction::L2D,
        unvalidated::Instruction::F2I => Instruction::F2I,
        unvalidated::Instruction::F2L => Instruction::F2L,
        unvalidated::Instruction::F2D => Instruction::F2D,
        unvalidated::Instruction::D2I => Instruction::D2I,
        unvalidated::Instruction::D2L => Instruction::D2L,
        unvalidated::Instruction::D2F => Instruction::D2F,
        unvalidated::Instruction::I2B => Instruction::I2B,
        unvalidated::Instruction::I2C => Instruction::I2C,
        unvalidated::Instruction::I2S => Instruction::I2S,
        unvalidated::Instruction::ICmp => Instruction::ICmp,
        unvalidated::Instruction::FCmpL => Instruction::FCmpL,
        unvalidated::Instruction::FCmpG => Instruction::FCmpG,
        unvalidated::Instruction::DCmpL => Instruction::DCmpL,
        unvalidated::Instruction::DCmpG => Instruction::DCmpG,
        unvalidated::Instruction::IfEq(v) => Instruction::IfEq(*v),
        unvalidated::Instruction::IfNe(v) => Instruction::IfNe(*v),
        unvalidated::Instruction::IfLt(v) => Instruction::IfLt(*v),
        unvalidated::Instruction::IfGe(v) => Instruction::IfGe(*v),
        unvalidated::Instruction::IfGt(v) => Instruction::IfGt(*v),
        unvalidated::Instruction::IfLe(v) => Instruction::IfLe(*v),
        unvalidated::Instruction::IfIcmpEq(v) => Instruction::IfIcmpEq(*v),
        unvalidated::Instruction::IfIcmpNe(v) => Instruction::IfIcmpNe(*v),
        unvalidated::Instruction::IfIcmpLt(v) => Instruction::IfIcmpLt(*v),
        unvalidated::Instruction::IfIcmpGe(v) => Instruction::IfIcmpGe(*v),
        unvalidated::Instruction::IfIcmpGt(v) => Instruction::IfIcmpGt(*v),
        unvalidated::Instruction::IfIcmpLe(v) => Instruction::IfIcmpLe(*v),
        unvalidated::Instruction::IfAcmpEq(v) => Instruction::IfAcmpEq(*v),
        unvalidated::Instruction::IfAcmpNe(v) => Instruction::IfAcmpNe(*v),
        unvalidated::Instruction::Goto(v) => Instruction::Goto(*v),
        unvalidated::Instruction::Jsr(v) => Instruction::Jsr(*v),
        unvalidated::Instruction::Ret(v) => Instruction::Ret(*v),
        unvalidated::Instruction::TableSwitch(v1, v2, v3, v4) => Instruction::TableSwitch(*v1, *v2, *v3, v4.clone()),
        unvalidated::Instruction::LookupSwitch(v1, v2) => Instruction::LookupSwitch(*v1, v2.clone()),
        unvalidated::Instruction::IReturn => Instruction::IReturn,
        unvalidated::Instruction::LReturn => Instruction::LReturn,
        unvalidated::Instruction::FReturn => Instruction::FReturn,
        unvalidated::Instruction::DReturn => Instruction::DReturn,
        unvalidated::Instruction::AReturn => Instruction::AReturn,
        unvalidated::Instruction::Return => Instruction::Return,
        unvalidated::Instruction::GetStatic(_) => Instruction::GetStatic(Rc::clone(&handle.field_refs[&position])),
        unvalidated::Instruction::PutStatic(_) => Instruction::PutStatic(Rc::clone(&handle.field_refs[&position])),
        unvalidated::Instruction::GetField(_) => Instruction::GetField(Rc::clone(&handle.field_refs[&position])),
        unvalidated::Instruction::PutField(_) => Instruction::PutField(Rc::clone(&handle.field_refs[&position])),
        unvalidated::Instruction::InvokeVirtual(_) => Instruction::InvokeVirtual(Rc::clone(&handle.method_refs[&position])),
        unvalidated::Instruction::InvokeSpecial(_) => Instruction::InvokeSpecial(Rc::clone(&handle.method_refs[&position])),
        unvalidated::Instruction::InvokeStatic(_) => Instruction::InvokeStatic(Rc::clone(&handle.method_refs[&position])),
        unvalidated::Instruction::InvokeInterface(v1, v2) => Instruction::InvokeInterface(*v1, *v2),
        unvalidated::Instruction::InvokeDynamic(v) => Instruction::InvokeDynamic(*v),
        unvalidated::Instruction::New(_) => Instruction::New(Rc::clone(&handle.class_refs[&position])),
        unvalidated::Instruction::NewArray(v) => Instruction::NewArray(*v),
        unvalidated::Instruction::ANewArray => Instruction::ANewArray,
        unvalidated::Instruction::ArrayLength => Instruction::ArrayLength,
        unvalidated::Instruction::AThrow => Instruction::AThrow,
        unvalidated::Instruction::CheckCast(_) => Instruction::CheckCast(Rc::clone(&handle.class_refs[&position])),
        unvalidated::Instruction::InstanceOf(_) => Instruction::InstanceOf(Rc::clone(&handle.class_refs[&position])),
        unvalidated::Instruction::MonitorEnter => Instruction::MonitorEnter,
        unvalidated::Instruction::MonitorExit => Instruction::MonitorExit,
        unvalidated::Instruction::MultiANewArray(_, v) => Instruction::MultiANewArray(Rc::clone(&handle.class_refs[&position]), *v),
        unvalidated::Instruction::IfNull(v) => Instruction::IfNull(*v),
        unvalidated::Instruction::IfNonNull(v) => Instruction::IfNonNull(*v),
        unvalidated::Instruction::GotoW(v) => Instruction::GotoW(*v),
        unvalidated::Instruction::JsrW(v) => Instruction::JsrW(*v),
    };

    Some(res)
}

fn make_refs<'validation>(
    inst: &unvalidated::Instruction,
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
            // TODO: check that local_idx < max_locals
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

impl MethodHandle {
    pub fn access(&self) -> &unvalidated::MethodAccessFlags {
        &self.access_flags
    }

    fn validate(raw_class: &UnvalidatedClassFile, raw_method: &unvalidated::MethodInfo) -> Result<MethodHandle, ValidationError> {
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
                    make_refs(&inst, position, raw_class, &mut class_refs, &mut field_refs, &mut method_refs, &mut const_refs)?;
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

#[derive(Debug)]
pub struct FieldHandle {
    // TODO:
    access_flags: unvalidated::FieldAccessFlags,
    pub(crate) name: String,
    pub(crate) desc: String,
    // flags, attributes, ..
}

impl FieldHandle {
    fn validate(raw_class: &UnvalidatedClassFile, raw_field: &unvalidated::FieldInfo) -> Result<FieldHandle, ValidationError> {
        let name = raw_class.checked_const(raw_field.name_index).and_then(|c| c.as_utf8())?.to_string();
        let desc = raw_class.checked_const(raw_field.descriptor_index).and_then(|c| c.as_utf8())?.to_string();
        Ok(FieldHandle { access_flags: raw_field.access_flags, name, desc })
    }
}

#[derive(Debug)]
pub enum Constant {
    String(String),
    Integer(u32),
    Float(f32),
    Long(u64),
    Double(f64),
}

impl Constant {
    fn validate(raw_class: &UnvalidatedClassFile, raw_const: &unvalidated::Constant) -> Result<Constant, ValidationError> {
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
            Constant::String(v) => write!(f, "{}", v),
            Constant::Integer(v) => write!(f, "{}", v),
            Constant::Float(v) => write!(f, "{}", v),
            Constant::Long(v) => write!(f, "{}", v),
            Constant::Double(v) => write!(f, "{}", v),
        }
    }
}

pub struct ClassFile {
    pub(crate) this_class: String,
    super_class: Option<String>,
    constants: Vec<Rc<Constant>>,
    interfaces: Vec<String>,
    pub(crate) fields: Vec<Rc<FieldHandle>>,
    methods: Vec<Rc<MethodHandle>>,
    // currently support no attributes on classes
    attributes: Vec<()>,
    pub(crate) native_methods: HashMap<String, fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>>
}

impl fmt::Debug for ClassFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ClassFile {{ this_class: {:?}, super_class: {:?}, constants: {:?}, interfaces: {:?}, fields: {:?}, methods: {:?}, attributes: {:?}, native_methods: {:?} }}",
           self.this_class,
           self.super_class,
           self.constants,
           self.interfaces,
           self.fields,
           self.methods,
           self.attributes,
           self.native_methods.keys()
        )
    }
}

#[derive(Debug)]
pub enum ValidationError {
    BadString,
    BadIndex(ConstantIdx),
    BadConst(String, String),
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
            panic!("uh oh");
            Err(ValidationError::BadConst(self.type_name().to_string(), "Utf8".to_string()))
        }
    }
}

impl UnvalidatedClassFile {
    fn checked_const<'cls>(&'cls self, idx: ConstantIdx) -> Result<&'cls unvalidated::Constant, ValidationError> {
        self.get_const(idx)
            .ok_or(ValidationError::BadIndex(self.this_class))
    }
}

impl ClassFile {
    pub fn get_method(&self, name: &str, desc: &str) -> Option<Rc<MethodHandle>> {
        for method in self.methods.iter() {
            if method.name == name && method.desc == desc {
                return Some(Rc::clone(method));
            }
        }

        None
    }

    pub fn get_methods(&self, name: &str) -> Vec<Rc<MethodHandle>> {
        let mut methods = Vec::new();
        for method in self.methods.iter() {
            if method.name == name {
                methods.push(Rc::clone(method));
            }
        }
        methods
    }


    pub fn has_static_field(&self, name: &str) -> bool {
        for field in self.fields.iter() {
            if field.access_flags.is_static() {
                if field.name == name {
                    return true;
                }
            }
        }

        false
    }

    pub fn validate(raw_class: &UnvalidatedClassFile) -> Result<ClassFile, ValidationError> {
        // first, `this_class` must be a constant index to a Utf8, containing a well-formed type
        // descriptor.
        // TODO: verify type descriptor
        let this_class = raw_class.checked_const(raw_class.this_class).and_then(|c| match c {
            unvalidated::Constant::Class(class) => {
                raw_class.checked_const(*class)
            },
            _ => { panic!("aaa"); }
        }).and_then(|c| c.as_utf8())?;
        let super_class = match raw_class.super_class.map(|sup| {
            raw_class.checked_const(sup).and_then(|c| match c{
                unvalidated::Constant::Class(class) => {
                    raw_class.checked_const(*class)
                },
                _ => { panic!("aaa"); }
            }).and_then(|c| c.as_utf8())
        }) {
            Some(Ok(sup)) => Some(sup),
            Some(Err(e)) => { return Err(e); },
            None => None
        };

        // TODO
        let mut constants = Vec::new();
        for raw_const in raw_class.constant_pool.iter() {
            match raw_const {
                unvalidated::Constant::String(_) |
                unvalidated::Constant::Integer(_) |
                unvalidated::Constant::Long(_) |
                unvalidated::Constant::Float(_) |
                unvalidated::Constant::Double(_) => {
                    constants.push(Rc::new(Constant::validate(raw_class, raw_const)?));
                }
                _ => {
                    // preserve indices for ldc and friends
                    constants.push(Rc::new(Constant::Integer(0)));
                }
            }
        }
        let mut interfaces = Vec::new();
        let mut fields = Vec::new();
        for raw_field in raw_class.fields.iter() {
            fields.push(Rc::new(FieldHandle::validate(raw_class, raw_field)?));
        }
        let mut methods = Vec::new();
        for raw_method in raw_class.methods.iter() {
            methods.push(Rc::new(MethodHandle::validate(raw_class, raw_method)?));
        }
        let mut attributes = Vec::new();

        Ok(ClassFile {
            this_class: this_class.to_string(),
            super_class: super_class.map(|x| x.to_string()),
            constants,
            interfaces,
            fields,
            methods,
            attributes,
            native_methods: raw_class.native_methods.clone(),
        })
    }
}

pub(crate) struct ClassFileRef(Rc<ClassFile>);

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

