use crate::class_file::unvalidated::AccessFlags;
use crate::class_file::unvalidated::AttributeInfo;
use crate::class_file::unvalidated::ClassFile;
use crate::class_file::unvalidated::Constant;
use crate::class_file::unvalidated::ConstantIdx;
use crate::class_file::unvalidated::Error;
use crate::class_file::unvalidated::FieldAccessFlags;
use crate::class_file::unvalidated::FieldInfo;
use crate::class_file::unvalidated::MethodAccessFlags;
use crate::class_file::unvalidated::MethodHandleBehavior;
use crate::class_file::unvalidated::MethodInfo;

use byteorder::{BigEndian, ReadBytesExt};
use std::collections::HashMap;
use std::io::Read;

const CLASS_MAGIC: u32 = 0xcafebabe;

pub trait FromReader<R>: Sized {
    fn read_from(data: &mut R) -> Result<Self, Error>;
}

impl<R: Read> FromReader<R> for u8 {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(data.read_u8()?)
    }
}

impl<R: Read> FromReader<R> for i8 {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(data.read_i8()?)
    }
}

impl<R: Read> FromReader<R> for FieldAccessFlags {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(FieldAccessFlags {
            flags: data.read_u16::<BigEndian>()?,
        })
    }
}

impl<R: Read> FromReader<R> for MethodAccessFlags {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(MethodAccessFlags {
            flags: data.read_u16::<BigEndian>()?,
        })
    }
}

impl<R: Read> FromReader<R> for u16 {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(data.read_u16::<BigEndian>()?)
    }
}

impl<R: Read> FromReader<R> for i16 {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(data.read_i16::<BigEndian>()?)
    }
}

impl<R: Read> FromReader<R> for u32 {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(data.read_u32::<BigEndian>()?)
    }
}

impl<R: Read> FromReader<R> for i32 {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(data.read_i32::<BigEndian>()?)
    }
}

impl<R: Read> FromReader<R> for f32 {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(data.read_f32::<BigEndian>()?)
    }
}

impl<R: Read> FromReader<R> for u64 {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(data.read_u64::<BigEndian>()?)
    }
}

impl<R: Read> FromReader<R> for f64 {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        Ok(data.read_f64::<BigEndian>()?)
    }
}

impl<R: Read> FromReader<R> for AttributeInfo {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        let name_index = ConstantIdx::read_from(data)?;
        let attr_length = u32::read_from(data)?;
        let mut attr_data: Vec<u8> = Vec::new();
        for _ in 0..attr_length {
            attr_data.push(u8::read_from(data)?);
        }
        Ok(AttributeInfo {
            name_index,
            data: attr_data,
        })
    }
}

impl<R: Read> FromReader<R> for FieldInfo {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        let access_flags = FieldAccessFlags::read_from(data)?;
        let name_index = ConstantIdx::read_from(data)?;
        let descriptor_index = ConstantIdx::read_from(data)?;
        let attributes = read_prefixed_array::<_, AttributeInfo>(0, data)?;
        Ok(FieldInfo {
            access_flags,
            name_index,
            descriptor_index,
            attributes,
        })
    }
}

impl<R: Read> FromReader<R> for MethodInfo {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        let access_flags = MethodAccessFlags::read_from(data)?;
        let name_index = ConstantIdx::read_from(data)?;
        let descriptor_index = ConstantIdx::read_from(data)?;
        let attributes = read_prefixed_array::<_, AttributeInfo>(0, data)?;
        Ok(MethodInfo {
            access_flags,
            name_index,
            descriptor_index,
            attributes,
        })
    }
}

impl<R: Read> FromReader<R> for MethodHandleBehavior {
    fn read_from(_data: &mut R) -> Result<Self, Error> {
        Err(Error::Unsupported("MethodHandleBehavior"))
    }
}

impl<R: Read> FromReader<R> for ConstantIdx {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        let idx = data.read_u16::<BigEndian>()?;
        if idx == 0 {
            return Err(Error::BadIndex);
        }

        Ok(ConstantIdx { idx })
    }
}

impl<R: Read> FromReader<R> for Option<ConstantIdx> {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        let idx = data.read_u16::<BigEndian>()?;
        if idx == 0 {
            return Ok(None);
        }

        Ok(Some(ConstantIdx { idx }))
    }
}

impl<R: Read> FromReader<R> for Constant {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        let tag = data.read_u8()?;
        let entry = match tag {
            // TODO: check -inf, +inf, NaN
            1 => Constant::Utf8(read_prefixed_array(0, data)?),
            3 => Constant::Integer(u32::read_from(data)?),
            4 => Constant::Float(f32::read_from(data)?),
            5 => Constant::Long(u64::read_from(data)?),
            6 => Constant::Double(f64::read_from(data)?),
            7 => Constant::Class(ConstantIdx::read_from(data)?),
            8 => Constant::String(ConstantIdx::read_from(data)?),
            9 => Constant::Fieldref(ConstantIdx::read_from(data)?, ConstantIdx::read_from(data)?),
            10 => Constant::Methodref(ConstantIdx::read_from(data)?, ConstantIdx::read_from(data)?),
            11 => Constant::InterfaceMethodref(
                ConstantIdx::read_from(data)?,
                ConstantIdx::read_from(data)?,
            ),
            12 => {
                Constant::NameAndType(ConstantIdx::read_from(data)?, ConstantIdx::read_from(data)?)
            }
            15 => Constant::MethodHandle(
                MethodHandleBehavior::read_from(data)?,
                ConstantIdx::read_from(data)?,
            ),
            16 => Constant::MethodType(ConstantIdx::read_from(data)?),
            18 => Constant::InvokeDynamic(u16::read_from(data)?, ConstantIdx::read_from(data)?),
            _ => {
                return Err(Error::Str("Invalid constant pool entry tag"));
            }
        };
        Ok(entry)
    }
}

fn read_prefixed_array<R: Read, T: FromReader<R>>(
    start: u16,
    data: &mut R,
) -> Result<Vec<T>, Error> {
    let count = data.read_u16::<BigEndian>().map_err(|_| Error::EOF)?;
    if count < start {
        return Err(Error::Str("Invalid array length"));
    }

    let mut elements = Vec::new();
    for _ in start..count {
        elements.push(T::read_from(data)?);
    }
    Ok(elements)
}

pub fn class_header<R: Read>(data: &mut R) -> Result<ClassFile, Error> {
    let magic = data.read_u32::<BigEndian>()?;
    if magic != CLASS_MAGIC {
        return Err(Error::BadMagic);
    }

    let minor = data.read_u16::<BigEndian>()?;
    let major = data.read_u16::<BigEndian>()?;
    let constants = read_prefixed_array::<_, Constant>(1, data)?;

    let access_flags = data.read_u16::<BigEndian>()?;
    let access_flags = AccessFlags {
        flags: access_flags,
    };

    let this_class = ConstantIdx::read_from(data)?;
    let super_class = Option::<ConstantIdx>::read_from(data)?;

    let interfaces = read_prefixed_array::<_, ConstantIdx>(0, data)?;
    let fields = read_prefixed_array::<_, FieldInfo>(0, data)?;
    let methods = read_prefixed_array::<_, MethodInfo>(0, data)?;
    let attributes = read_prefixed_array::<_, AttributeInfo>(0, data)?;

    Ok(ClassFile {
        minor_version: minor,
        major_version: major,
        constant_pool: constants,
        access_flags,
        this_class,
        super_class,
        interfaces,
        fields,
        methods,
        attributes,
        // native methods don't come with associated native functions. if a method is native and
        // loaded from an external class, we'll go through JNI resolution mechanisms to find it.
        native_methods: HashMap::new(),
    })
}
