pub mod attribute;
pub mod instruction;
pub mod read;

use read::FromReader;

use crate::class_file::attribute::Attribute;
use crate::class_file::attribute::ExceptionTableRecord;
use crate::class_file::attribute::LineNumberEntry;

use std::fmt;
use std::rc::Rc;

// TODO: helper to consistency check flags
#[derive(Debug, Clone, Copy)]
pub struct MethodAccessFlags {
    flags: u16,
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
pub struct AccessFlags {
    flags: u16,
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

#[derive(Debug)]
pub struct AttributeInfo {
    name_index: ConstantIdx,
    data: Vec<u8>,
}

impl AttributeInfo {
    pub fn materialize(&self, class_file: &ClassFile) -> Result<Attribute, Error> {
        match class_file.get_raw_str(self.name_index) {
            Some(b"ConstantValue") => Ok(Attribute::ConstantValue(ConstantIdx::read_from(
                &mut self.data.as_slice(),
            )?)),
            Some(b"Code") => {
                let data = &mut self.data.as_slice();
                let max_stack = u16::read_from(data)?;
                let max_locals = u16::read_from(data)?;
                let code_length = u32::read_from(data)?;
                let mut code: Vec<u8> = Vec::new();
                for _ in 0..code_length {
                    code.push(u8::read_from(data)?);
                }
                let exceptions_length = u16::read_from(data)?;
                let mut exceptions: Vec<ExceptionTableRecord> = Vec::new();
                for _ in 0..exceptions_length {
                    exceptions.push(ExceptionTableRecord::read_from(data)?);
                }
                let attr_length = u16::read_from(data)?;
                let mut attrs: Vec<AttributeInfo> = Vec::new();
                for _ in 0..attr_length {
                    attrs.push(AttributeInfo::read_from(data)?);
                }
                Ok(Attribute::Code(
                    max_stack, max_locals, code, exceptions, attrs,
                ))
            }
            Some(b"LineNumberTable") => {
                let data = &mut self.data.as_slice();
                let lineno_length = u16::read_from(data)?;
                let mut entries: Vec<LineNumberEntry> = Vec::new();
                for _ in 0..lineno_length {
                    entries.push(LineNumberEntry::read_from(data)?);
                }
                Ok(Attribute::LineNumberTable(entries))
            }
            Some(_) => Err(Error::Unsupported("unsupported attribute type")),
            None => Err(Error::ClassFileError(
                "bad constant pool index - not a utf8",
            )),
        }
    }
}

pub struct AttributeInfoDisplay<'a, 'b> {
    attribute: &'a AttributeInfo,
    class_file: &'b ClassFile,
}

impl AttributeInfo {
    pub fn display<'a, 'b>(&'a self, class_file: &'b ClassFile) -> AttributeInfoDisplay<'a, 'b> {
        AttributeInfoDisplay {
            attribute: self,
            class_file,
        }
    }
}

impl<'a, 'b> fmt::Display for AttributeInfoDisplay<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}, {:?}",
            self.class_file.get_str(self.attribute.name_index).unwrap(),
            self.attribute.data
        )
    }
}

#[derive(Debug)]
struct FieldInfo {}

#[derive(Debug)]
struct MethodInfo {
    access_flags: MethodAccessFlags,
    name_index: ConstantIdx,
    descriptor_index: ConstantIdx,
    attributes: Vec<AttributeInfo>,
}

#[derive(Debug)]
pub struct MethodHandle {
    access_flags: MethodAccessFlags,
    name: String,
    descriptor: String,
    attributes: Vec<Rc<Attribute>>,
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
    fn display<'a, 'b>(&'a self, class_file: &'b ClassFile) -> ConstantDisplay<'a, 'b> {
        ConstantDisplay {
            constant: self,
            class_file,
        }
    }
}

struct ConstantDisplay<'a, 'b> {
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

#[derive(Debug)]
pub enum Error {
    BadMagic,
    BadIndex,
    ClassFileError(&'static str),
    EOF,
    BadInstruction(u8, bool),
    Str(&'static str),
    Unsupported(&'static str),
}

impl From<std::io::Error> for Error {
    fn from(_err: std::io::Error) -> Self {
        // TODO handle errors that aren't _actually_ end of file?
        Error::EOF
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ConstantIdx {
    /// In a just world, this would be `NonZero<u16>`.
    idx: u16,
}

impl ConstantIdx {
    fn inner(&self) -> u16 {
        self.idx
    }
}

/// As cute as a zero-copy class file parse would be, I really don't want to think about DSTs,
/// which are all over the place.
#[derive(Debug)]
pub struct ClassFile {
    minor_version: u16,
    major_version: u16,
    constant_pool: Vec<Constant>,
    access_flags: AccessFlags,
    this_class: ConstantIdx,
    super_class: Option<ConstantIdx>,
    interfaces: Vec<ConstantIdx>,
    fields: Vec<FieldInfo>,
    methods: Vec<MethodInfo>,
    attributes: Vec<AttributeInfo>,
}

impl ClassFile {
    fn get_str(&self, idx: ConstantIdx) -> Option<&str> {
        if let Some(Constant::Utf8(bytes)) = self.constant_pool.get((idx.idx - 1) as usize) {
            std::str::from_utf8(bytes).ok()
        } else {
            None
        }
    }

    fn get_raw_str(&self, idx: ConstantIdx) -> Option<&[u8]> {
        if let Some(Constant::Utf8(bytes)) = self.constant_pool.get((idx.idx - 1) as usize) {
            Some(bytes)
        } else {
            None
        }
    }

    fn get_const(&self, idx: ConstantIdx) -> Option<&Constant> {
        self.constant_pool.get((idx.idx - 1) as usize)
    }

    fn display_const(&self, idx: ConstantIdx) -> String {
        match self.constant_pool[(idx.idx - 1) as usize] {
            Constant::Class(idx) => format!("class {}", self.get_str(idx).unwrap()),
            _ => {
                unimplemented!("display_const");
            }
        }
    }

    pub fn get_method(&self, name: &str) -> Result<Rc<MethodHandle>, Error> {
        for method in self.methods.iter() {
            let method_name = self.get_str(method.name_index).unwrap();
            if method_name == name {
                let handle = MethodHandle {
                    access_flags: method.access_flags,
                    name: method_name.to_string(),
                    descriptor: self.get_str(method.descriptor_index).unwrap().to_string(),
                    attributes: method.attributes.iter().map(|attr| {
                        Rc::new(attr.materialize(self).unwrap())
                    }).collect()
                };

                return Ok(Rc::new(handle));
            }
        }

        Err(Error::Str("Failed to look up method"))
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
