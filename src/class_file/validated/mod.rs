use std::collections::HashMap;
use std::hash::{Hasher, Hash};
use std::rc::Rc;
use std::fmt;
use std::io::{Cursor, Write};

use crate::class_file::unvalidated;
use crate::class_file::unvalidated::ConstantIdx;
use crate::class_file::unvalidated::{ClassFile as UnvalidatedClassFile};

mod instruction;
pub use instruction::Instruction;

mod constant;
pub use constant::Constant;

mod method;
pub use method::MethodBody;
pub use method::MethodHandle;
pub use method::MethodInstrIter;
pub use method::MethodRef;

mod field;
pub use field::FieldHandle;
pub use field::FieldRef;

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
        unvalidated::Instruction::Ldc(_idx) => {
            if let Some(const_ref) = handle.const_refs.get(&position) {
                Instruction::Ldc(Rc::clone(const_ref))
            } else {
                Instruction::Ldc(Rc::new(Constant::Class(handle.class_refs[&position].to_string())))
            }
        },
        unvalidated::Instruction::LdcW(_idx) => {
            if let Some(const_ref) = handle.const_refs.get(&position) {
                Instruction::LdcW(Rc::clone(&const_ref))
            } else {
                Instruction::LdcW(Rc::new(Constant::Class(handle.class_refs[&position].to_string())))
            }
        }
        unvalidated::Instruction::Ldc2W(_) => Instruction::Ldc2W(Rc::clone(&handle.const_refs[&position])),
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
        unvalidated::Instruction::LCmp => Instruction::LCmp,
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
        unvalidated::Instruction::InvokeInterface(_, v2) => Instruction::InvokeInterface(Rc::clone(&handle.method_refs[&position]), *v2),
        unvalidated::Instruction::InvokeDynamic(v) => Instruction::InvokeDynamic(*v),
        unvalidated::Instruction::New(_) => Instruction::New(Rc::clone(&handle.class_refs[&position])),
        unvalidated::Instruction::ANewArray(v) => Instruction::ANewArray(*v),
        unvalidated::Instruction::NewArray(v) => {
            match v {
                4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 => {
                    // these are ok atype values (bool through long)
                }
                atype => {
                    panic!("invalid atype for newarray: {}", atype);
                }
            }
            Instruction::NewArray(*v)
        }
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

pub struct ClassFile {
    pub(crate) this_class: String,
    pub(crate) super_class: Option<String>,
    constants: Vec<Rc<Constant>>,
    pub(crate) interfaces: Vec<String>,
    pub(crate) fields: Vec<Rc<FieldHandle>>,
    methods: Vec<Rc<MethodHandle>>,
    // currently support no attributes on classes
    attributes: Vec<()>,
}

impl fmt::Debug for ClassFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ClassFile {{ this_class: {:?}, super_class: {:?}, constants: {:?}, interfaces: {:?}, fields: {:?}, methods: {:?}, attributes: {:?} }}",
           self.this_class,
           self.super_class,
           self.constants,
           self.interfaces,
           self.fields,
           self.methods,
           self.attributes,
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
    pub fn method_ref(&self, name: &str, desc: &str) -> Option<MethodRef> {
        self.get_method(name, desc)
            .map(|handle| {
                MethodRef {
                    class_name: self.this_class.clone(),
                    name: handle.name.clone(),
                    desc: handle.desc.clone(),
                }
            })
    }

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

    pub fn has_instance_field(&self, name: &str) -> bool {
        for field in self.fields.iter() {
            if !field.access_flags.is_static() {
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
            unvalidated::Constant::Utf8(bytes) => {
                panic!("const is a utf8 string: {:?}", std::str::from_utf8(bytes).unwrap());
            },
            other => { panic!("validate unknown constant {:?}", other); }
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
        for raw_interface in raw_class.interfaces.iter() {
            let interface_const = raw_class.checked_const(*raw_interface)
                .expect("class file constant pool indices are valid");
            let interface_name = match interface_const {
                unvalidated::Constant::Class(class) => {
                    raw_class.checked_const(*class)
                        .expect("class file constant pool indices are valid")
                        .as_utf8()?
                        .to_string()
                },
                other => {
                    return Err(
                        ValidationError::BadConst(
                            format!("{:?}", other),
                            "Class".to_string()
                        )
                    );
                }
            };
            interfaces.push(interface_name);
        }
        let mut fields = Vec::new();
        for raw_field in raw_class.fields.iter() {
            fields.push(Rc::new(FieldHandle::validate(raw_class, raw_field)?));
        }
        let mut methods = Vec::new();
        for raw_method in raw_class.methods.iter() {
            methods.push(Rc::new(MethodHandle::validate(raw_class, raw_method)?));
        }
        let attributes = Vec::new();

        Ok(ClassFile {
            this_class: this_class.to_string(),
            super_class: super_class.map(|x| x.to_string()),
            constants,
            interfaces,
            fields,
            methods,
            attributes,
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

fn assemble_into(inst: crate::class_file::validated::Instruction, bytes: &mut Cursor<Vec<u8>>, method_body: &mut MethodBody) {
    use crate::class_file::validated::Instruction::*;

    let write_res = match inst {
        Nop => { bytes.write_all(&[0x00]) },
        AConstNull => { bytes.write_all(&[0x01]) },
        IConstM1 => { bytes.write_all(&[0x02]) },
        IConst0 => { bytes.write_all(&[0x03]) },
        IConst1 => { bytes.write_all(&[0x04]) },
        IConst2 => { bytes.write_all(&[0x05]) },
        IConst3 => { bytes.write_all(&[0x06]) },
        IConst4 => { bytes.write_all(&[0x07]) },
        IConst5 => { bytes.write_all(&[0x08]) },
        LConst0 => { bytes.write_all(&[0x09]) },
        LConst1 => { bytes.write_all(&[0x0a]) },
        FConst0 => { bytes.write_all(&[0x0b]) },
        FConst1 => { bytes.write_all(&[0x0c]) },
        FConst2 => { bytes.write_all(&[0x0d]) },
        DConst0 => { bytes.write_all(&[0x0e]) },
        DConst1 => { bytes.write_all(&[0x0f]) },
        /*
        BIPush(i8::read_from(data)?) => { bytes.write_all(&[0x10]) },
        SIPush(i16::read_from(data)?) => { bytes.write_all(&[0x11]) },
        Ldc(ConstantIdx::new(u8::read_from(data)? as u16).unwrap()) => { bytes.write_all(&[0x12]) },
        LdcW(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0x13]) },
        Ldc2W(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0x14]) },
        ILoad(read_idx(data, wide)?) => { bytes.write_all(&[0x15]) },
        LLoad(read_idx(data, wide)?) => { bytes.write_all(&[0x16]) },
        FLoad(read_idx(data, wide)?) => { bytes.write_all(&[0x17]) },
        DLoad(read_idx(data, wide)?) => { bytes.write_all(&[0x18]) },
        ALoad(read_idx(data, wide)?) => { bytes.write_all(&[0x19]) },
        */
        ILoad0 => { bytes.write_all(&[0x1a]) },
        ILoad1 => { bytes.write_all(&[0x1b]) },
        ILoad2 => { bytes.write_all(&[0x1c]) },
        ILoad3 => { bytes.write_all(&[0x1d]) },
        LLoad0 => { bytes.write_all(&[0x1e]) },
        LLoad1 => { bytes.write_all(&[0x1f]) },
        LLoad2 => { bytes.write_all(&[0x20]) },
        LLoad3 => { bytes.write_all(&[0x21]) },
        FLoad0 => { bytes.write_all(&[0x22]) },
        FLoad1 => { bytes.write_all(&[0x23]) },
        FLoad2 => { bytes.write_all(&[0x24]) },
        FLoad3 => { bytes.write_all(&[0x25]) },
        DLoad0 => { bytes.write_all(&[0x26]) },
        DLoad1 => { bytes.write_all(&[0x27]) },
        DLoad2 => { bytes.write_all(&[0x28]) },
        DLoad3 => { bytes.write_all(&[0x29]) },
        ALoad0 => { bytes.write_all(&[0x2a]) },
        ALoad1 => { bytes.write_all(&[0x2b]) },
        ALoad2 => { bytes.write_all(&[0x2c]) },
        ALoad3 => { bytes.write_all(&[0x2d]) },
        IALoad => { bytes.write_all(&[0x2e]) },
        LALoad => { bytes.write_all(&[0x2f]) },
        FALoad => { bytes.write_all(&[0x30]) },
        DALoad => { bytes.write_all(&[0x31]) },
        AALoad => { bytes.write_all(&[0x32]) },
        BALoad => { bytes.write_all(&[0x33]) },
        CALoad => { bytes.write_all(&[0x34]) },
        SALoad => { bytes.write_all(&[0x35]) },
        /*
        IStore(read_idx(data, wide)?) => { bytes.write_all(&[0x36]) },
        LStore(read_idx(data, wide)?) => { bytes.write_all(&[0x37]) },
        FStore(read_idx(data, wide)?) => { bytes.write_all(&[0x38]) },
        DStore(read_idx(data, wide)?) => { bytes.write_all(&[0x39]) },
        AStore(read_idx(data, wide)?) => { bytes.write_all(&[0x3a]) },
        */
        IStore0 => { bytes.write_all(&[0x3b]) },
        IStore1 => { bytes.write_all(&[0x3c]) },
        IStore2 => { bytes.write_all(&[0x3d]) },
        IStore3 => { bytes.write_all(&[0x3e]) },
        LStore0 => { bytes.write_all(&[0x3f]) },
        LStore1 => { bytes.write_all(&[0x40]) },
        LStore2 => { bytes.write_all(&[0x41]) },
        LStore3 => { bytes.write_all(&[0x42]) },
        FStore0 => { bytes.write_all(&[0x43]) },
        FStore1 => { bytes.write_all(&[0x44]) },
        FStore2 => { bytes.write_all(&[0x45]) },
        FStore3 => { bytes.write_all(&[0x46]) },
        DStore0 => { bytes.write_all(&[0x47]) },
        DStore1 => { bytes.write_all(&[0x48]) },
        DStore2 => { bytes.write_all(&[0x49]) },
        DStore3 => { bytes.write_all(&[0x4a]) },
        AStore0 => { bytes.write_all(&[0x4b]) },
        AStore1 => { bytes.write_all(&[0x4c]) },
        AStore2 => { bytes.write_all(&[0x4d]) },
        AStore3 => { bytes.write_all(&[0x4e]) },
        IAStore => { bytes.write_all(&[0x4f]) },
        LAStore => { bytes.write_all(&[0x50]) },
        FAStore => { bytes.write_all(&[0x51]) },
        DAStore => { bytes.write_all(&[0x52]) },
        AAStore => { bytes.write_all(&[0x53]) },
        BAStore => { bytes.write_all(&[0x54]) },
        CAStore => { bytes.write_all(&[0x55]) },
        SAStore => { bytes.write_all(&[0x56]) },
        Pop => { bytes.write_all(&[0x57]) },
        Pop2 => { bytes.write_all(&[0x58]) },
        Dup => { bytes.write_all(&[0x59]) },
        DupX1 => { bytes.write_all(&[0x5a]) },
        DupX2 => { bytes.write_all(&[0x5b]) },
        Dup2 => { bytes.write_all(&[0x5c]) },
        Dup2X1 => { bytes.write_all(&[0x5d]) },
        Dup2X2 => { bytes.write_all(&[0x5e]) },
        Swap => { bytes.write_all(&[0x5f]) },
        IAdd => { bytes.write_all(&[0x60]) },
        LAdd => { bytes.write_all(&[0x61]) },
        FAdd => { bytes.write_all(&[0x62]) },
        DAdd => { bytes.write_all(&[0x63]) },
        ISub => { bytes.write_all(&[0x64]) },
        LSub => { bytes.write_all(&[0x65]) },
        FSub => { bytes.write_all(&[0x66]) },
        DSub => { bytes.write_all(&[0x67]) },
        IMul => { bytes.write_all(&[0x68]) },
        LMul => { bytes.write_all(&[0x69]) },
        FMul => { bytes.write_all(&[0x6a]) },
        DMul => { bytes.write_all(&[0x6b]) },
        IDiv => { bytes.write_all(&[0x6c]) },
        LDiv => { bytes.write_all(&[0x6d]) },
        FDiv => { bytes.write_all(&[0x6e]) },
        DDiv => { bytes.write_all(&[0x6f]) },
        IRem => { bytes.write_all(&[0x70]) },
        LRem => { bytes.write_all(&[0x71]) },
        FRem => { bytes.write_all(&[0x72]) },
        DRem => { bytes.write_all(&[0x73]) },
        INeg => { bytes.write_all(&[0x74]) },
        LNeg => { bytes.write_all(&[0x75]) },
        FNeg => { bytes.write_all(&[0x76]) },
        DNeg => { bytes.write_all(&[0x77]) },
        IShl => { bytes.write_all(&[0x78]) },
        LShl => { bytes.write_all(&[0x79]) },
        IShr => { bytes.write_all(&[0x7a]) },
        LShr => { bytes.write_all(&[0x7b]) },
        IUshr => { bytes.write_all(&[0x7c]) },
        LUshr => { bytes.write_all(&[0x7d]) },
        IAnd => { bytes.write_all(&[0x7e]) },
        LAnd => { bytes.write_all(&[0x7f]) },
        IOr => { bytes.write_all(&[0x80]) },
        LOr => { bytes.write_all(&[0x81]) },
        IXor => { bytes.write_all(&[0x82]) },
        LXor => { bytes.write_all(&[0x83]) },
        // IInc(read_idx(data, wide)?, read_idx(data, wide)? as i16) => { bytes.write_all(&[0x84]) },
        I2L => { bytes.write_all(&[0x85]) },
        I2F => { bytes.write_all(&[0x86]) },
        I2D => { bytes.write_all(&[0x87]) },
        L2I => { bytes.write_all(&[0x88]) },
        L2F => { bytes.write_all(&[0x89]) },
        L2D => { bytes.write_all(&[0x8a]) },
        F2I => { bytes.write_all(&[0x8b]) },
        F2L => { bytes.write_all(&[0x8c]) },
        F2D => { bytes.write_all(&[0x8d]) },
        D2I => { bytes.write_all(&[0x8e]) },
        D2L => { bytes.write_all(&[0x8f]) },
        D2F => { bytes.write_all(&[0x90]) },
        I2B => { bytes.write_all(&[0x91]) },
        I2C => { bytes.write_all(&[0x92]) },
        I2S => { bytes.write_all(&[0x93]) },
        LCmp => { bytes.write_all(&[0x94]) },
        FCmpL => { bytes.write_all(&[0x95]) },
        FCmpG => { bytes.write_all(&[0x96]) },
        DCmpL => { bytes.write_all(&[0x97]) },
        DCmpG => { bytes.write_all(&[0x98]) },
        IfEq(offset) => {
            bytes.write_all(&[0x99])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfNe(offset) => {
            bytes.write_all(&[0x9a])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfLt(offset) => {
            bytes.write_all(&[0x9b])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfGe(offset) => {
            bytes.write_all(&[0x9c])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfGt(offset) => {
            bytes.write_all(&[0x9d])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfLe(offset) => {
            bytes.write_all(&[0x9e])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfIcmpEq(offset) => {
            bytes.write_all(&[0x9f])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfIcmpNe(offset) => {
            bytes.write_all(&[0xa0])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfIcmpLt(offset) => {
            bytes.write_all(&[0xa1])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfIcmpGe(offset) => {
            bytes.write_all(&[0xa2])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfIcmpGt(offset) => {
            bytes.write_all(&[0xa3])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfIcmpLe(offset) => {
            bytes.write_all(&[0xa4])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfAcmpEq(offset) => {
            bytes.write_all(&[0xa5])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfAcmpNe(offset) => {
            bytes.write_all(&[0xa6])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        Goto(offset) => {
            bytes.write_all(&[0xa7])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        Jsr(offset) => {
            bytes.write_all(&[0xa8])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        /*
        Ret(read_idx(data, wide)?) => { bytes.write_all(&[0xa9]) },
                0xaa => {
                    while data.seek(SeekFrom::Current(0))? % 4 != 0 {
                        let _ = u8::read_from(data)?;
                    }
                    let default = i32::read_from(data)?;
                    let low = i32::read_from(data)?;
                    let high = i32::read_from(data)?;
                    let mut entries = Vec::new();
                    for _ in low..=high {
                        entries.push(i32::read_from(data)?);
                    }
                    Instruction::TableSwitch(default, low, high, entries)
                }
                0xab => {
                    while data.seek(SeekFrom::Current(0))? % 4 != 0 {
                        let _ = u8::read_from(data)?;
                    }
                    let default = i32::read_from(data)?;
                    // Not a bug!
                    // "Immediately after the padding follow a series of signed 32-bit
                    // values: default, npairs, and then npairs pairs of signed 32-bit values."
                    let count = i32::read_from(data)?;
                    let mut entries = Vec::new();
                    for _ in 0..count {
                        entries.push((u32::read_from(data)?, i32::read_from(data)?));
                    }
                    Instruction::LookupSwitch(default, entries)
                }
        */
        IReturn => { bytes.write_all(&[0xac]) },
        LReturn => { bytes.write_all(&[0xad]) },
        FReturn => { bytes.write_all(&[0xae]) },
        DReturn => { bytes.write_all(&[0xaf]) },
        AReturn => { bytes.write_all(&[0xb0]) },
        Return => { bytes.write_all(&[0xb1]) },
        // TODO: assembly needs to include assembling to a sink that holds fieldrefs etc
        GetField(fieldref) => {
            method_body.field_refs.insert(method_body.field_refs.len() as u32 + 1, fieldref);
            let field_id = method_body.field_refs.len();
            bytes.write_all(&[0xb4])
                .and_then(|_| {
                    bytes.write_all(&field_id.to_le_bytes())
                })
        },
        /*
        GetStatic(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0xb2]) },
        PutStatic(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0xb3]) },
        PutField(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0xb5]) },
        InvokeVirtual(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0xb6]) },
        */
        InvokeSpecial(method_ref) => {
            method_body.method_refs.insert(bytes.position() as u32, method_ref);
            bytes.write_all(&[0xb7])
                .and_then(|_| {
                    bytes.write_all(&u16::MAX.to_le_bytes())
                })
        },
        /*
        InvokeStatic(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0xb8]) },
                0xb9 => Instruction::InvokeInterface(
                    ConstantIdx::read_from(data)?,
                    u8::read_from(data)?,
                ),
        InvokeDynamic(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0xba]) },
        */
        New(cls_name) => {
            method_body.class_refs.insert(bytes.position() as u32, cls_name);
            bytes.write_all(&[0xbb])
                .and_then(|_| {
                    bytes.write_all(&u16::MAX.to_le_bytes())
                })
        },
        /*
        NewArray(u8::read_from(data)?) => { bytes.write_all(&[0xbc]) },
        ANewArray(u16::read_from(data)?) => { bytes.write_all(&[0xbd]) },
        */
        ArrayLength => { bytes.write_all(&[0xbe]) },
        AThrow => { bytes.write_all(&[0xbf]) },
        /*
        CheckCast(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0xc0]) },
        InstanceOf(ConstantIdx::read_from(data)?) => { bytes.write_all(&[0xc1]) },
        */
        MonitorEnter => { bytes.write_all(&[0xc2]) },
        MonitorExit => { bytes.write_all(&[0xc3]) },
        /*
                0xc4 => {
                    return Err(Error::BadInstruction(0xc4, wide));
                }
                0xc5 => {
                    Instruction::MultiANewArray(ConstantIdx::read_from(data)?, u8::read_from(data)?)
                }
        */
        IfNull(offset) => {
            bytes.write_all(&[0xc6])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        IfNonNull(offset) => {
            bytes.write_all(&[0xc7])
                .and_then(|_| {
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        GotoW(offset) => {
            bytes.write_all(&[0xc8])
                .and_then(|_| {
                    debug_assert!(offset.to_le_bytes().len() == 4);
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        JsrW(offset) => {
            bytes.write_all(&[0xc9])
                .and_then(|_| {
                    debug_assert!(offset.to_le_bytes().len() == 4);
                    bytes.write_all(&offset.to_le_bytes())
                })
        },
        other => {
            panic!("unsupported inst: {}", other);
        }
    };
    write_res.expect("write succeeds");
}

pub fn assemble(insts: Vec<crate::class_file::validated::Instruction>) -> MethodBody {
    let mut result = MethodBody {
        max_stack: 65535,
        max_locals: 65535,
        bytes: Vec::new().into_boxed_slice(),
        exception_info: Vec::new(),
        class_refs: HashMap::new(),
        field_refs: HashMap::new(),
        method_refs: HashMap::new(),
        const_refs: HashMap::new(),
    };

    let mut cursor = Cursor::new(Vec::new());
    for inst in insts {
        assemble_into(inst, &mut cursor, &mut result);
    }
    result.bytes = cursor.into_inner().into_boxed_slice();
    result
}
