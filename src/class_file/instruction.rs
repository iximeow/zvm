use crate::class_file::read::FromReader;
use crate::class_file::ClassFile;
use crate::class_file::ConstantIdx;
use crate::class_file::Error;
use std::fmt;
use std::io::{Read, Seek, SeekFrom};

pub struct InstructionDisplay<'a, 'b> {
    instruction: &'a Instruction,
    class_file: &'b ClassFile,
}

impl<'a, 'b> fmt::Display for InstructionDisplay<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.instruction {
            Instruction::Nop => write!(f, "nop"),
            Instruction::AConstNull => write!(f, "aconstnull"),
            Instruction::IConstM1 => write!(f, "iconstm1"),
            Instruction::IConst0 => write!(f, "iconst0"),
            Instruction::IConst1 => write!(f, "iconst1"),
            Instruction::IConst2 => write!(f, "iconst2"),
            Instruction::IConst3 => write!(f, "iconst3"),
            Instruction::IConst4 => write!(f, "iconst4"),
            Instruction::IConst5 => write!(f, "iconst5"),
            Instruction::LConst0 => write!(f, "lconst0"),
            Instruction::LConst1 => write!(f, "lconst1"),
            Instruction::FConst0 => write!(f, "fconst0"),
            Instruction::FConst1 => write!(f, "fconst1"),
            Instruction::FConst2 => write!(f, "fconst2"),
            Instruction::DConst0 => write!(f, "dconst0"),
            Instruction::DConst1 => write!(f, "dconst1"),
            Instruction::BIPush => write!(f, "bipush"),
            Instruction::SIPush => write!(f, "sipush"),
            Instruction::Ldc(idx) => write!(
                f,
                "ldc {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::LdcW(idx) => write!(
                f,
                "ldcw {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::Ldc2W(idx) => write!(
                f,
                "ldc2w {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::ILoad(idx) => write!(f, "iload {}", idx),
            Instruction::LLoad(idx) => write!(f, "lload {}", idx),
            Instruction::FLoad(idx) => write!(f, "fload {}", idx),
            Instruction::DLoad(idx) => write!(f, "dload {}", idx),
            Instruction::ALoad => write!(f, "aload"),
            Instruction::ILoad0 => write!(f, "iload0"),
            Instruction::ILoad1 => write!(f, "iload1"),
            Instruction::ILoad2 => write!(f, "iload2"),
            Instruction::ILoad3 => write!(f, "iload3"),
            Instruction::LLoad0 => write!(f, "lload0"),
            Instruction::LLoad1 => write!(f, "lload1"),
            Instruction::LLoad2 => write!(f, "lload2"),
            Instruction::LLoad3 => write!(f, "lload3"),
            Instruction::FLoad0 => write!(f, "fload0"),
            Instruction::FLoad1 => write!(f, "fload1"),
            Instruction::FLoad2 => write!(f, "fload2"),
            Instruction::FLoad3 => write!(f, "fload3"),
            Instruction::DLoad0 => write!(f, "dload0"),
            Instruction::DLoad1 => write!(f, "dload1"),
            Instruction::DLoad2 => write!(f, "dload2"),
            Instruction::DLoad3 => write!(f, "dload3"),
            Instruction::ALoad0 => write!(f, "aload0"),
            Instruction::ALoad1 => write!(f, "aload1"),
            Instruction::ALoad2 => write!(f, "aload2"),
            Instruction::ALoad3 => write!(f, "aload3"),
            Instruction::IAStore => write!(f, "iastore"),
            Instruction::IALoad => write!(f, "iaload"),
            Instruction::FALoad => write!(f, "faload"),
            Instruction::DALoad => write!(f, "daload"),
            Instruction::AALoad => write!(f, "aaload"),
            Instruction::BALoad => write!(f, "baload"),
            Instruction::CALoad => write!(f, "caload"),
            Instruction::SALoad => write!(f, "saload"),
            Instruction::IStore(idx) => write!(f, "istore {}", idx),
            Instruction::LStore(idx) => write!(f, "lstore {}", idx),
            Instruction::FStore(idx) => write!(f, "fstore {}", idx),
            Instruction::DStore(idx) => write!(f, "dstore {}", idx),
            Instruction::AStore => write!(f, "astore"),
            Instruction::IStore0 => write!(f, "istore0"),
            Instruction::IStore1 => write!(f, "istore1"),
            Instruction::IStore2 => write!(f, "istore2"),
            Instruction::IStore3 => write!(f, "istore3"),
            Instruction::LStore0 => write!(f, "lstore0"),
            Instruction::LStore1 => write!(f, "lstore1"),
            Instruction::LStore2 => write!(f, "lstore2"),
            Instruction::LStore3 => write!(f, "lstore3"),
            Instruction::FStore0 => write!(f, "fstore0"),
            Instruction::FStore1 => write!(f, "fstore1"),
            Instruction::FStore2 => write!(f, "fstore2"),
            Instruction::FStore3 => write!(f, "fstore3"),
            Instruction::DStore0 => write!(f, "dstore0"),
            Instruction::DStore1 => write!(f, "dstore1"),
            Instruction::DStore2 => write!(f, "dstore2"),
            Instruction::DStore3 => write!(f, "dstore3"),
            Instruction::AStore0 => write!(f, "astore0"),
            Instruction::AStore1 => write!(f, "astore1"),
            Instruction::AStore2 => write!(f, "astore2"),
            Instruction::AStore3 => write!(f, "astore3"),
            Instruction::LAStore => write!(f, "lastore"),
            Instruction::FAStore => write!(f, "fastore"),
            Instruction::DAStore => write!(f, "dastore"),
            Instruction::AAStore => write!(f, "aastore"),
            Instruction::BAStore => write!(f, "bastore"),
            Instruction::CAStore => write!(f, "castore"),
            Instruction::SAStore => write!(f, "sastore"),
            Instruction::Pop => write!(f, "pop"),
            Instruction::Pop2 => write!(f, "pop2"),
            Instruction::Dup => write!(f, "dup"),
            Instruction::DupX1 => write!(f, "dupx1"),
            Instruction::DupX2 => write!(f, "dupx2"),
            Instruction::Dup2 => write!(f, "dup2"),
            Instruction::Dup2X1 => write!(f, "dup2x1"),
            Instruction::Dup2X2 => write!(f, "dup2x2"),
            Instruction::Swap => write!(f, "swap"),
            Instruction::IAdd => write!(f, "iadd"),
            Instruction::FAdd => write!(f, "fadd"),
            Instruction::DAdd => write!(f, "dadd"),
            Instruction::ISub => write!(f, "isub"),
            Instruction::LSub => write!(f, "lsub"),
            Instruction::FSub => write!(f, "fsub"),
            Instruction::DSub => write!(f, "dsub"),
            Instruction::IMul => write!(f, "imul"),
            Instruction::LMul => write!(f, "lmul"),
            Instruction::FMul => write!(f, "fmul"),
            Instruction::DMul => write!(f, "dmul"),
            Instruction::IDiv => write!(f, "idiv"),
            Instruction::LDiv => write!(f, "ldiv"),
            Instruction::FDiv => write!(f, "fdiv"),
            Instruction::DDiv => write!(f, "ddiv"),
            Instruction::IRem => write!(f, "irem"),
            Instruction::LRem => write!(f, "lrem"),
            Instruction::FRem => write!(f, "frem"),
            Instruction::DRem => write!(f, "drem"),
            Instruction::INeg => write!(f, "ineg"),
            Instruction::LNeg => write!(f, "lneg"),
            Instruction::FNeg => write!(f, "fneg"),
            Instruction::DNeg => write!(f, "dneg"),
            Instruction::IShl => write!(f, "ishl"),
            Instruction::LShl => write!(f, "lshl"),
            Instruction::IShr => write!(f, "ishr"),
            Instruction::LShr => write!(f, "lshr"),
            Instruction::IUshr => write!(f, "iushr"),
            Instruction::LUshr => write!(f, "lushr"),
            Instruction::IAnd => write!(f, "iand"),
            Instruction::LAnd => write!(f, "land"),
            Instruction::IOr => write!(f, "ior"),
            Instruction::LOr => write!(f, "lor"),
            Instruction::IXor => write!(f, "ixor"),
            Instruction::LXor => write!(f, "lxor"),
            Instruction::IInc(idx, inc) => write!(f, "iinc {}, {}", idx, inc),
            Instruction::I2L => write!(f, "i2l"),
            Instruction::I2F => write!(f, "i2f"),
            Instruction::I2D => write!(f, "i2d"),
            Instruction::L2I => write!(f, "l2i"),
            Instruction::L2F => write!(f, "l2f"),
            Instruction::L2D => write!(f, "l2d"),
            Instruction::F2I => write!(f, "f2i"),
            Instruction::F2L => write!(f, "f2l"),
            Instruction::F2D => write!(f, "f2d"),
            Instruction::D2I => write!(f, "d2i"),
            Instruction::D2L => write!(f, "d2l"),
            Instruction::D2F => write!(f, "d2f"),
            Instruction::I2B => write!(f, "i2b"),
            Instruction::I2C => write!(f, "i2c"),
            Instruction::I2S => write!(f, "i2s"),
            Instruction::ICmp => write!(f, "icmp"),
            Instruction::FCmpL => write!(f, "fcmpl"),
            Instruction::FCmpG => write!(f, "fcmpg"),
            Instruction::DCmpL => write!(f, "dcmpl"),
            Instruction::DCmpG => write!(f, "dcmpg"),
            Instruction::IfEq(offset) => write!(f, "ifeq {}", offset),
            Instruction::IfNe(offset) => write!(f, "ifne {}", offset),
            Instruction::IfLt(offset) => write!(f, "iflt {}", offset),
            Instruction::IfGe(offset) => write!(f, "ifge {}", offset),
            Instruction::IfGt(offset) => write!(f, "ifgt {}", offset),
            Instruction::IfLe(offset) => write!(f, "ifle {}", offset),
            Instruction::IfIcmpEq(offset) => write!(f, "ificmpeq {}", offset),
            Instruction::IfIcmpNe(offset) => write!(f, "ificmpne {}", offset),
            Instruction::IfIcmpLt(offset) => write!(f, "ificmplt {}", offset),
            Instruction::IfIcmpGe(offset) => write!(f, "ificmpge {}", offset),
            Instruction::IfIcmpGt(offset) => write!(f, "ificmpgt {}", offset),
            Instruction::IfIcmpLe(offset) => write!(f, "ificmple {}", offset),
            Instruction::IfAcmpEq(offset) => write!(f, "ifacmpeq {}", offset),
            Instruction::IfAcmpNe(offset) => write!(f, "ifacmpne {}", offset),
            Instruction::Goto(offset) => write!(f, "goto {}", offset),
            Instruction::Jsr(offset) => write!(f, "jsr {}", offset),
            Instruction::Ret(idx) => write!(f, "ret {}", idx),
            Instruction::TableSwitch(default, low, high, entries) => write!(
                f,
                "tableswitch {} {}..{}, {:?}",
                default, low, high, entries
            ),
            Instruction::LookupSwitch(default, entries) => {
                write!(f, "lookupswitch {} {:?}", default, entries)
            }
            Instruction::IReturn => write!(f, "ireturn"),
            Instruction::LReturn => write!(f, "lreturn"),
            Instruction::FReturn => write!(f, "freturn"),
            Instruction::DReturn => write!(f, "dreturn"),
            Instruction::AReturn => write!(f, "areturn"),
            Instruction::Return => write!(f, "return"),
            Instruction::GetStatic(idx) => write!(
                f,
                "getstatic {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::PutStatic(idx) => write!(
                f,
                "putstatic {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::GetField(idx) => write!(
                f,
                "getfield {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::PutField(idx) => write!(
                f,
                "putfield {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::InvokeVirtual(idx) => write!(
                f,
                "invokevirtual {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::InvokeSpecial(idx) => write!(
                f,
                "invokespecial {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::InvokeStatic(idx) => write!(
                f,
                "invokestatic {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::InvokeInterface(idx, count) => write!(
                f,
                "invokeinterface {} (const {}), {}",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner(),
                count
            ),
            Instruction::InvokeDynamic(idx) => write!(
                f,
                "invokedynamic {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::New(idx) => write!(
                f,
                "new {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::NewArray(idx) => write!(
                f,
                "newarray (const {})",
                idx,
            ),
            Instruction::ANewArray => write!(f, "anewarray"),
            Instruction::ArrayLength => write!(f, "arraylength"),
            Instruction::AThrow => write!(f, "athrow"),
            Instruction::CheckCast(idx) => write!(
                f,
                "checkcast {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::InstanceOf(idx) => write!(
                f,
                "instanceof {} (const {})",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner()
            ),
            Instruction::MonitorEnter => write!(f, "monitorenter"),
            Instruction::MonitorExit => write!(f, "monitorexit"),
            Instruction::MultiANewArray(idx, dimensions) => write!(
                f,
                "multianewarray {} (const {}), {}",
                self.class_file
                    .get_const(*idx)
                    .unwrap()
                    .display(self.class_file),
                idx.inner(),
                dimensions
            ),
            Instruction::IfNull(offset) => write!(f, "ifnull {}", offset),
            Instruction::IfNonNull(offset) => write!(f, "ifnonnull {}", offset),
            Instruction::GotoW(offset) => write!(f, "gotow {}", offset),
            Instruction::JsrW(offset) => write!(f, "jsrw {}", offset),
        }
    }
}

impl Instruction {
    pub fn display<'a, 'b>(&'a self, class_file: &'b ClassFile) -> InstructionDisplay<'a, 'b> {
        InstructionDisplay {
            instruction: self,
            class_file,
        }
    }
}

#[derive(Debug)]
pub enum Instruction {
    Nop,
    AConstNull,
    IConstM1,
    IConst0,
    IConst1,
    IConst2,
    IConst3,
    IConst4,
    IConst5,
    LConst0,
    LConst1,
    FConst0,
    FConst1,
    FConst2,
    DConst0,
    DConst1,
    BIPush,
    SIPush,
    Ldc(ConstantIdx),
    LdcW(ConstantIdx),
    Ldc2W(ConstantIdx),
    ILoad(u16),
    LLoad(u16),
    FLoad(u16),
    DLoad(u16),
    ALoad,
    ILoad0,
    ILoad1,
    ILoad2,
    ILoad3,
    LLoad0,
    LLoad1,
    LLoad2,
    LLoad3,
    FLoad0,
    FLoad1,
    FLoad2,
    FLoad3,
    DLoad0,
    DLoad1,
    DLoad2,
    DLoad3,
    ALoad0,
    ALoad1,
    ALoad2,
    ALoad3,
    IAStore,
    IALoad,
    FALoad,
    DALoad,
    AALoad,
    BALoad,
    CALoad,
    SALoad,
    IStore(u16),
    LStore(u16),
    FStore(u16),
    DStore(u16),
    AStore,
    IStore0,
    IStore1,
    IStore2,
    IStore3,
    LStore0,
    LStore1,
    LStore2,
    LStore3,
    FStore0,
    FStore1,
    FStore2,
    FStore3,
    DStore0,
    DStore1,
    DStore2,
    DStore3,
    AStore0,
    AStore1,
    AStore2,
    AStore3,
    LAStore,
    FAStore,
    DAStore,
    AAStore,
    BAStore,
    CAStore,
    SAStore,
    Pop,
    Pop2,
    Dup,
    DupX1,
    DupX2,
    Dup2,
    Dup2X1,
    Dup2X2,
    Swap,
    IAdd,
    FAdd,
    DAdd,
    ISub,
    LSub,
    FSub,
    DSub,
    IMul,
    LMul,
    FMul,
    DMul,
    IDiv,
    LDiv,
    FDiv,
    DDiv,
    IRem,
    LRem,
    FRem,
    DRem,
    INeg,
    LNeg,
    FNeg,
    DNeg,
    IShl,
    LShl,
    IShr,
    LShr,
    IUshr,
    LUshr,
    IAnd,
    LAnd,
    IOr,
    LOr,
    IXor,
    LXor,
    IInc(u16, i16),
    I2L,
    I2F,
    I2D,
    L2I,
    L2F,
    L2D,
    F2I,
    F2L,
    F2D,
    D2I,
    D2L,
    D2F,
    I2B,
    I2C,
    I2S,
    ICmp,
    FCmpL,
    FCmpG,
    DCmpL,
    DCmpG,
    IfEq(i16),
    IfNe(i16),
    IfLt(i16),
    IfGe(i16),
    IfGt(i16),
    IfLe(i16),
    IfIcmpEq(i16),
    IfIcmpNe(i16),
    IfIcmpLt(i16),
    IfIcmpGe(i16),
    IfIcmpGt(i16),
    IfIcmpLe(i16),
    IfAcmpEq(i16),
    IfAcmpNe(i16),
    Goto(u16),
    Jsr(i16),
    Ret(u16),
    TableSwitch(i32, i32, i32, Vec<i32>),
    LookupSwitch(i32, Vec<(u32, i32)>),
    IReturn,
    LReturn,
    FReturn,
    DReturn,
    AReturn,
    Return,
    GetStatic(ConstantIdx),
    PutStatic(ConstantIdx),
    GetField(ConstantIdx),
    PutField(ConstantIdx),
    InvokeVirtual(ConstantIdx),
    InvokeSpecial(ConstantIdx),
    InvokeStatic(ConstantIdx),
    InvokeInterface(ConstantIdx, u8),
    InvokeDynamic(ConstantIdx),
    New(ConstantIdx),
    NewArray(u8),
    ANewArray,
    ArrayLength,
    AThrow,
    CheckCast(ConstantIdx),
    InstanceOf(ConstantIdx),
    MonitorEnter,
    MonitorExit,
    MultiANewArray(ConstantIdx, u8),
    IfNull(i16),
    IfNonNull(i16),
    GotoW(i32),
    JsrW(i32),
}

impl<R: Read + Seek> FromReader<R> for Instruction {
    fn read_from(data: &mut R) -> Result<Self, Error> {
        fn read_instruction<R: Read + Seek>(
            data: &mut R,
            opc: u8,
            wide: bool,
        ) -> Result<Instruction, Error> {
            fn read_idx<R: Read>(data: &mut R, wide: bool) -> Result<u16, Error> {
                if wide {
                    u16::read_from(data)
                } else {
                    u8::read_from(data).map(|v| v.into())
                }
            }
            let opc = match opc {
                0x00 => Instruction::Nop,
                0x01 => Instruction::AConstNull,
                0x02 => Instruction::IConstM1,
                0x03 => Instruction::IConst0,
                0x04 => Instruction::IConst1,
                0x05 => Instruction::IConst2,
                0x06 => Instruction::IConst3,
                0x07 => Instruction::IConst4,
                0x08 => Instruction::IConst5,
                0x09 => Instruction::LConst0,
                0x0a => Instruction::LConst1,
                0x0b => Instruction::FConst0,
                0x0c => Instruction::FConst1,
                0x0d => Instruction::FConst2,
                0x0e => Instruction::DConst0,
                0x0f => Instruction::DConst1,
                0x10 => Instruction::BIPush,
                0x11 => Instruction::SIPush,
                0x12 => Instruction::Ldc(ConstantIdx::new(u8::read_from(data)? as u16).unwrap()),
                0x13 => Instruction::LdcW(ConstantIdx::read_from(data)?),
                0x14 => Instruction::Ldc2W(ConstantIdx::read_from(data)?),
                0x15 => Instruction::ILoad(read_idx(data, wide)?),
                0x16 => Instruction::LLoad(read_idx(data, wide)?),
                0x17 => Instruction::FLoad(read_idx(data, wide)?),
                0x18 => Instruction::DLoad(read_idx(data, wide)?),
                0x19 => Instruction::ALoad,
                0x1a => Instruction::ILoad0,
                0x1b => Instruction::ILoad1,
                0x1c => Instruction::ILoad2,
                0x1d => Instruction::ILoad3,
                0x1e => Instruction::LLoad0,
                0x1f => Instruction::LLoad1,
                0x20 => Instruction::LLoad2,
                0x21 => Instruction::LLoad3,
                0x22 => Instruction::FLoad0,
                0x23 => Instruction::FLoad1,
                0x24 => Instruction::FLoad2,
                0x25 => Instruction::FLoad3,
                0x26 => Instruction::DLoad0,
                0x27 => Instruction::DLoad1,
                0x28 => Instruction::DLoad2,
                0x29 => Instruction::DLoad3,
                0x2a => Instruction::ALoad0,
                0x2b => Instruction::ALoad1,
                0x2c => Instruction::ALoad2,
                0x2d => Instruction::ALoad3,
                0x2e => Instruction::IAStore,
                0x2f => Instruction::IALoad,
                0x30 => Instruction::FALoad,
                0x31 => Instruction::DALoad,
                0x32 => Instruction::AALoad,
                0x33 => Instruction::BALoad,
                0x34 => Instruction::CALoad,
                0x35 => Instruction::SALoad,
                0x36 => Instruction::IStore(read_idx(data, wide)?),
                0x37 => Instruction::LStore(read_idx(data, wide)?),
                0x38 => Instruction::FStore(read_idx(data, wide)?),
                0x39 => Instruction::DStore(read_idx(data, wide)?),
                0x3a => Instruction::AStore,
                0x3b => Instruction::IStore0,
                0x3c => Instruction::IStore1,
                0x3d => Instruction::IStore2,
                0x3e => Instruction::IStore3,
                0x3f => Instruction::LStore0,
                0x40 => Instruction::LStore1,
                0x41 => Instruction::LStore2,
                0x42 => Instruction::LStore3,
                0x43 => Instruction::FStore0,
                0x44 => Instruction::FStore1,
                0x45 => Instruction::FStore2,
                0x46 => Instruction::FStore3,
                0x47 => Instruction::DStore0,
                0x48 => Instruction::DStore1,
                0x49 => Instruction::DStore2,
                0x4a => Instruction::DStore3,
                0x4b => Instruction::AStore0,
                0x4c => Instruction::AStore1,
                0x4d => Instruction::AStore2,
                0x4e => Instruction::AStore3,
                0x50 => Instruction::LAStore,
                0x51 => Instruction::FAStore,
                0x52 => Instruction::DAStore,
                0x53 => Instruction::AAStore,
                0x54 => Instruction::BAStore,
                0x55 => Instruction::CAStore,
                0x56 => Instruction::SAStore,
                0x57 => Instruction::Pop,
                0x58 => Instruction::Pop2,
                0x59 => Instruction::Dup,
                0x5a => Instruction::DupX1,
                0x5b => Instruction::DupX2,
                0x5c => Instruction::Dup2,
                0x5d => Instruction::Dup2X1,
                0x5e => Instruction::Dup2X2,
                0x5f => Instruction::Swap,
                0x60 => Instruction::IAdd,
                0x61 => Instruction::IAdd,
                0x62 => Instruction::FAdd,
                0x63 => Instruction::DAdd,
                0x64 => Instruction::ISub,
                0x65 => Instruction::LSub,
                0x66 => Instruction::FSub,
                0x67 => Instruction::DSub,
                0x68 => Instruction::IMul,
                0x69 => Instruction::LMul,
                0x6a => Instruction::FMul,
                0x6b => Instruction::DMul,
                0x6c => Instruction::IDiv,
                0x6d => Instruction::LDiv,
                0x6e => Instruction::FDiv,
                0x6f => Instruction::DDiv,
                0x70 => Instruction::IRem,
                0x71 => Instruction::LRem,
                0x72 => Instruction::FRem,
                0x73 => Instruction::DRem,
                0x74 => Instruction::INeg,
                0x75 => Instruction::LNeg,
                0x76 => Instruction::FNeg,
                0x77 => Instruction::DNeg,
                0x78 => Instruction::IShl,
                0x79 => Instruction::LShl,
                0x7a => Instruction::IShr,
                0x7b => Instruction::LShr,
                0x7c => Instruction::IUshr,
                0x7d => Instruction::LUshr,
                0x7e => Instruction::IAnd,
                0x7f => Instruction::LAnd,
                0x80 => Instruction::IOr,
                0x81 => Instruction::LOr,
                0x82 => Instruction::IXor,
                0x83 => Instruction::LXor,
                0x84 => Instruction::IInc(read_idx(data, wide)?, read_idx(data, wide)? as i16),
                0x85 => Instruction::I2L,
                0x86 => Instruction::I2F,
                0x87 => Instruction::I2D,
                0x88 => Instruction::L2I,
                0x89 => Instruction::L2F,
                0x8a => Instruction::L2D,
                0x8b => Instruction::F2I,
                0x8c => Instruction::F2L,
                0x8d => Instruction::F2D,
                0x8e => Instruction::D2I,
                0x8f => Instruction::D2L,
                0x90 => Instruction::D2F,
                0x91 => Instruction::I2B,
                0x92 => Instruction::I2C,
                0x93 => Instruction::I2S,
                0x94 => Instruction::ICmp,
                0x95 => Instruction::FCmpL,
                0x96 => Instruction::FCmpG,
                0x97 => Instruction::DCmpL,
                0x98 => Instruction::DCmpG,
                0x99 => Instruction::IfEq(i16::read_from(data)?),
                0x9a => Instruction::IfNe(i16::read_from(data)?),
                0x9b => Instruction::IfLt(i16::read_from(data)?),
                0x9c => Instruction::IfGe(i16::read_from(data)?),
                0x9d => Instruction::IfGt(i16::read_from(data)?),
                0x9e => Instruction::IfLe(i16::read_from(data)?),
                0x9f => Instruction::IfIcmpEq(i16::read_from(data)?),
                0xa0 => Instruction::IfIcmpNe(i16::read_from(data)?),
                0xa1 => Instruction::IfIcmpLt(i16::read_from(data)?),
                0xa2 => Instruction::IfIcmpGe(i16::read_from(data)?),
                0xa3 => Instruction::IfIcmpGt(i16::read_from(data)?),
                0xa4 => Instruction::IfIcmpLe(i16::read_from(data)?),
                0xa5 => Instruction::IfAcmpEq(i16::read_from(data)?),
                0xa6 => Instruction::IfAcmpNe(i16::read_from(data)?),
                0xa7 => Instruction::Goto(u16::read_from(data)?),
                0xa8 => Instruction::Jsr(i16::read_from(data)?),
                0xa9 => Instruction::Ret(read_idx(data, wide)?),
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
                0xac => Instruction::IReturn,
                0xad => Instruction::LReturn,
                0xae => Instruction::FReturn,
                0xaf => Instruction::DReturn,
                0xb0 => Instruction::AReturn,
                0xb1 => Instruction::Return,
                0xb2 => Instruction::GetStatic(ConstantIdx::read_from(data)?),
                0xb3 => Instruction::PutStatic(ConstantIdx::read_from(data)?),
                0xb4 => Instruction::GetField(ConstantIdx::read_from(data)?),
                0xb5 => Instruction::PutField(ConstantIdx::read_from(data)?),
                0xb6 => Instruction::InvokeVirtual(ConstantIdx::read_from(data)?),
                0xb7 => Instruction::InvokeSpecial(ConstantIdx::read_from(data)?),
                0xb8 => Instruction::InvokeStatic(ConstantIdx::read_from(data)?),
                0xb9 => Instruction::InvokeInterface(
                    ConstantIdx::read_from(data)?,
                    u8::read_from(data)?,
                ),
                0xba => Instruction::InvokeDynamic(ConstantIdx::read_from(data)?),
                0xbb => Instruction::New(ConstantIdx::read_from(data)?),
                0xbc => Instruction::NewArray(u8::read_from(data)?),
                0xbd => Instruction::ANewArray,
                0xbe => Instruction::ArrayLength,
                0xbf => Instruction::AThrow,
                0xc0 => Instruction::CheckCast(ConstantIdx::read_from(data)?),
                0xc1 => Instruction::InstanceOf(ConstantIdx::read_from(data)?),
                0xc2 => Instruction::MonitorEnter,
                0xc3 => Instruction::MonitorExit,
                0xc4 => {
                    return Err(Error::BadInstruction(0xc4, wide));
                }
                0xc5 => {
                    Instruction::MultiANewArray(ConstantIdx::read_from(data)?, u8::read_from(data)?)
                }
                0xc6 => Instruction::IfNull(i16::read_from(data)?),
                0xc7 => Instruction::IfNonNull(i16::read_from(data)?),
                0xc8 => Instruction::GotoW(i32::read_from(data)?),
                0xc9 => Instruction::JsrW(i32::read_from(data)?),
                other => {
                    return Err(Error::BadInstruction(other, wide));
                }
            };
            Ok(opc)
        };

        let first = u8::read_from(data)?;

        if first == 0xc4 {
            let next = u8::read_from(data)?;
            read_instruction(data, next, true)
        } else {
            read_instruction(data, first, false)
        }
    }
}
