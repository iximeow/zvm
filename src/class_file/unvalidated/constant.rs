use crate::class_file::unvalidated::ClassFile;
use crate::class_file::unvalidated::MethodHandleBehavior;

use std::fmt;

#[derive(Debug)]
pub enum Constant {
    Utf8(Vec<u8>),
    Integer(u32),
    Float(f32),
    Long(u64),
    Double(f64),
    Class(ConstantIdx),
    String(ConstantIdx),
    Fieldref(ConstantIdx, ConstantIdx),
    Methodref(ConstantIdx, ConstantIdx),
    InterfaceMethodref(ConstantIdx, ConstantIdx),
    NameAndType(ConstantIdx, ConstantIdx),
    MethodHandle(MethodHandleBehavior, ConstantIdx),
    MethodType(ConstantIdx),
    InvokeDynamic(u16, ConstantIdx),
}

impl Constant {
    pub(crate) fn type_name(&self) -> &str {
        match self {
            Constant::Utf8(_) => "Utf8",
            Constant::Integer(_) => "Integer",
            Constant::Float(_) => "Float",
            Constant::Long(_) => "Long",
            Constant::Double(_) => "Double",
            Constant::Class(_) => "Class",
            Constant::String(_) => "String",
            Constant::Fieldref(_, _) => "Fieldref",
            Constant::Methodref(_, _) => "Methodref",
            Constant::InterfaceMethodref(_, _) => "InterfaceMethodref",
            Constant::NameAndType(_, _) => "NameAndType",
            Constant::MethodHandle(_, _) => "MethodHandle",
            Constant::MethodType(_) => "MethodType",
            Constant::InvokeDynamic(_, _) => "InvokeDynamic",
        }
    }

    pub fn display<'a, 'b>(&'a self, class_file: &'b ClassFile) -> ConstantDisplay<'a, 'b> {
        ConstantDisplay {
            constant: self,
            class_file,
        }
    }
}

pub struct ConstantDisplay<'a, 'b> {
    constant: &'a Constant,
    class_file: &'b ClassFile,
}

impl<'a, 'b> fmt::Display for ConstantDisplay<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.constant {
            Constant::Utf8(data) => {
                if let Some(s) = std::str::from_utf8(data).ok() {
                    write!(f, "{}", s)
                } else {
                    write!(f, "malformed string: {:?}", data)
                }
            }
            Constant::Integer(value) => write!(f, "{}", value),
            Constant::Float(value) => write!(f, "{}", value),
            Constant::Long(value) => write!(f, "{}", value),
            Constant::Double(value) => write!(f, "{}", value),
            Constant::Class(idx) => write!(
                f,
                "class {}",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file)
            ),
            Constant::String(idx) => write!(
                f,
                "{}",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file)
            ),
            Constant::Fieldref(class_index, name_and_type_index) => write!(
                f,
                "field {}.{}",
                self.class_file
                    .get_const(*class_index)
                    .unwrap()
                    .display(self.class_file),
                self.class_file
                    .get_const(*name_and_type_index)
                    .unwrap()
                    .display(self.class_file),
            ),
            Constant::Methodref(class_index, name_and_type_index) => write!(
                f,
                "method {}.{}",
                self.class_file
                    .get_const(*class_index)
                    .unwrap()
                    .display(self.class_file),
                self.class_file
                    .get_const(*name_and_type_index)
                    .unwrap()
                    .display(self.class_file),
            ),
            Constant::InterfaceMethodref(class_index, name_and_type_index) => write!(
                f,
                "interfacemethod {}.{}",
                self.class_file
                    .get_const(*class_index)
                    .unwrap()
                    .display(self.class_file),
                self.class_file
                    .get_const(*name_and_type_index)
                    .unwrap()
                    .display(self.class_file),
            ),
            Constant::NameAndType(name_index, descriptor_index) => write!(
                f,
                "{} {}",
                self.class_file
                    .get_const(*name_index)
                    .unwrap()
                    .display(self.class_file),
                self.class_file
                    .get_const(*descriptor_index)
                    .unwrap()
                    .display(self.class_file),
            ),
            Constant::MethodHandle(behavior, idx) => {
                match behavior {
                    MethodHandleBehavior::GetField => write!(f, "REF_getField"),
                    MethodHandleBehavior::GetStatic => write!(f, "REF_getStatic"),
                    MethodHandleBehavior::PutField => write!(f, "REF_putField"),
                    MethodHandleBehavior::PutStatic => write!(f, "REF_putStatic"),
                    MethodHandleBehavior::InvokeVirtual => write!(f, "REF_invokeVirtual"),
                    MethodHandleBehavior::InvokeStatic => write!(f, "REF_invokeStatic"),
                    MethodHandleBehavior::InvokeSpecial => write!(f, "REF_invokeSpecial"),
                    MethodHandleBehavior::NewInvokeSpecial => write!(f, "REF_newInvokeSpecial"),
                    MethodHandleBehavior::InvokeInterface => write!(f, "REF_invokeInterface"),
                    MethodHandleBehavior::Other(other) => {
                        write!(f, "invalid behavior: {}", *other as u16)
                    }
                }?;

                write!(
                    f,
                    " {}",
                    self.class_file
                        .get_const(*idx)
                        .unwrap()
                        .display(self.class_file),
                )
            }
            Constant::MethodType(descriptor_index) => write!(
                f,
                "{}",
                self.class_file
                    .get_const(*descriptor_index)
                    .unwrap()
                    .display(self.class_file),
            ),
            Constant::InvokeDynamic(bootstrap_index, name_and_type_index) => write!(
                f,
                "{} {}",
                bootstrap_index,
                self.class_file
                    .get_const(*name_and_type_index)
                    .unwrap()
                    .display(self.class_file),
            ),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ConstantIdx {
    /// In a just world, this would be `NonZero<u16>`.
    idx: u16,
}

impl ConstantIdx {
    pub(crate) fn inner(&self) -> u16 {
        self.idx
    }

    pub fn new(idx: u16) -> Option<ConstantIdx> {
        if idx == 0 {
            None
        } else {
            Some(ConstantIdx { idx })
        }
    }
}
