use std::collections::HashMap;
use std::hash::{Hasher, Hash};
use std::rc::Rc;
use std::fmt;

use crate::class_file::unvalidated;
use crate::class_file::unvalidated::ConstantIdx;
use crate::class_file::unvalidated::{ClassFile as UnvalidatedClassFile};

use crate::virtual_machine::VMError;
use crate::virtual_machine::VMState;
use crate::virtual_machine::VirtualMachine;

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
        unvalidated::Instruction::Ldc(_) => Instruction::Ldc(Rc::clone(&handle.const_refs[&position])),
        unvalidated::Instruction::LdcW(_) => Instruction::LdcW(Rc::clone(&handle.const_refs[&position])),
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
        unvalidated::Instruction::ANewArray(v) => Instruction::ANewArray(*v),
        unvalidated::Instruction::NewArray => Instruction::NewArray,
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
        let interfaces = Vec::new();
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

