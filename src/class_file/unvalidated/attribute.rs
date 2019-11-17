use crate::class_file::unvalidated::instruction::Instruction;
use crate::class_file::unvalidated::ClassFile;
use crate::class_file::unvalidated::ConstantIdx;
use crate::class_file::unvalidated::Error;

use crate::class_file::unvalidated::read::FromReader;

use std::fmt;
use std::io::{Cursor, Read};

#[allow(dead_code)]
#[derive(Debug)]
pub struct ExceptionTableRecord {
    start_pc: u16,
    end_pc: u16,
    handler_pc: u16,
    catch_type: u16,
}

impl<R: Read> FromReader<R> for ExceptionTableRecord {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(ExceptionTableRecord {
            start_pc: u16::read_from(data)?,
            end_pc: u16::read_from(data)?,
            handler_pc: u16::read_from(data)?,
            catch_type: u16::read_from(data)?,
        })
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct LineNumberEntry {
    start_pc: u16,
    line_number: u16,
}

impl<R: Read> FromReader<R> for LineNumberEntry {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(LineNumberEntry {
            start_pc: u16::read_from(data)?,
            line_number: u16::read_from(data)?,
        })
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum VerificationTypeInfo {
    Top,
    Integer,
    Float,
    Long,
    Double,
    Null,
    UninitializedThis,
    Object(ConstantIdx),
    Uninitialized(u16),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum StackMapFrame {
    SameFrame,
    SameLocals1StackItemFrame(VerificationTypeInfo),
    SameLocals1StackItemFrameExtended(u16, VerificationTypeInfo),
    ChopFrame(u16),
    SameFrameExtended(u16),
    AppendFrame(u16, Vec<VerificationTypeInfo>),
    FullFrame(u16, Vec<VerificationTypeInfo>, Vec<VerificationTypeInfo>),
}

#[derive(Debug)]
pub enum Attribute {
    // constantvalue_index
    ConstantValue(ConstantIdx),
    Code(
        u16,
        u16,
        Vec<u8>,
        Vec<ExceptionTableRecord>,
        Vec<AttributeInfo>,
    ),
    #[allow(dead_code)]
    StackMapTable(Vec<StackMapFrame>),
    LineNumberTable(Vec<LineNumberEntry>),
}

impl Attribute {
    pub fn display<'a, 'b>(&'a self, class_file: &'b ClassFile) -> AttributeDisplay<'a, 'b> {
        AttributeDisplay {
            attribute: self,
            class_file,
        }
    }
}

pub struct AttributeDisplay<'a, 'b> {
    attribute: &'a Attribute,
    class_file: &'b ClassFile,
}

impl<'a, 'b> fmt::Display for AttributeDisplay<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.attribute {
            Attribute::ConstantValue(idx) => {
                // TODO: .. not debug
                writeln!(f, "constant {:?}", self.class_file.get_const(*idx).unwrap())
            }
            Attribute::Code(max_stack, max_locals, code, exceptions, attributes) => {
                writeln!(f, "code")?;
                writeln!(f, "  max_stack: {}", max_stack)?;
                writeln!(f, "  max_locals: {}", max_locals)?;
                writeln!(f, "  instructions:")?;
                let mut code = Cursor::new(code.as_slice());
                while let Ok(inst) = Instruction::read_from(&mut code) {
                    writeln!(f, "    {}", inst.display(self.class_file))?;
                }
                if exceptions.len() > 0 {
                    writeln!(
                        f,
                        "  exceptions: {} records (bodies TODO)",
                        exceptions.len()
                    )?;
                }
                if attributes.len() > 0 {
                    writeln!(f, "  attributes:")?;
                    for attr in attributes.iter() {
                        if let Ok(attr) = attr.materialize(self.class_file) {
                            writeln!(f, "    {}", attr.display(self.class_file))?;
                        } else {
                            writeln!(
                                f,
                                "    (attr failed to materialize) {}",
                                attr.display(self.class_file)
                            )?;
                        }
                    }
                }
                Ok(())
            }
            Attribute::StackMapTable(_frames) => {
                writeln!(f, "stack_map_table")?;
                writeln!(f, "  frames: TODO")
            }
            Attribute::LineNumberTable(entries) => {
                writeln!(f, "line_number_table")?;
                for entry in entries {
                    writeln!(f, "  {:?}", entry)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct AttributeInfo {
    pub(crate) name_index: ConstantIdx,
    pub(crate)data: Vec<u8>,
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
