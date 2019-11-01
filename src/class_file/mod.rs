pub mod read;

use std::fmt;

// TODO: helper to consistency check flags
#[derive(Debug)]
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
struct AttributeInfo {
    name_index: ConstantIdx,
    data: Vec<u8>,
}

struct AttributeInfoDisplay<'a, 'b> {
    attribute: &'a AttributeInfo,
    class_file: &'b ClassFile,
}

impl AttributeInfo {
    fn display<'a, 'b>(&'a self, class_file: &'b ClassFile) -> AttributeInfoDisplay<'a, 'b> {
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
    MethodType(ConstantIdx, ConstantIdx),
    InvokeDynamic(ConstantIdx, ConstantIdx),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum MethodHandleBehavior {
    TODO,
}

#[derive(Debug)]
pub enum Error {
    BadMagic,
    BadIndex,
    #[allow(dead_code)] // shhh bad class files aren't rejected yet
    ClassFileError,
    EOF,
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

    fn display_const(&self, idx: ConstantIdx) -> String {
        match self.constant_pool[(idx.idx - 1) as usize] {
            Constant::Class(idx) => format!("class {}", self.get_str(idx).unwrap()),
            _ => {
                unimplemented!("display_const");
            }
        }
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
                writeln!(f, "    method attr {}", attr.display(self))?;
            }
        }
        for attribute in self.attributes.iter() {
            writeln!(f, "  attribute {}", attribute.display(self))?;
        }
        Ok(())
    }
}
