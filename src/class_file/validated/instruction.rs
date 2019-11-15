use std::fmt;

use crate::class_file::unvalidated::ConstantIdx;
use crate::class_file::validated::Constant;
use crate::class_file::validated::FieldRef;
use crate::class_file::validated::MethodRef;

impl<'method, 'cls> fmt::Display for Instruction<'method, 'cls> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
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
            Instruction::BIPush(b) => write!(f, "bipush {}", b),
            Instruction::SIPush(s) => write!(f, "sipush {}", s),
            Instruction::Ldc(c) => write!(
                f,
                "ldc {}", c
            ),
            Instruction::LdcW(c) => write!(
                f,
                "ldcw {}", c
            ),
            Instruction::Ldc2W(c) => write!(
                f,
                "ldc2w {}", c
            ),
            Instruction::ILoad(idx) => write!(f, "iload {}", idx),
            Instruction::LLoad(idx) => write!(f, "lload {}", idx),
            Instruction::FLoad(idx) => write!(f, "fload {}", idx),
            Instruction::DLoad(idx) => write!(f, "dload {}", idx),
            Instruction::ALoad(idx) => write!(f, "aload {}", idx),
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
            Instruction::LALoad => write!(f, "laload"),
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
            Instruction::AStore(idx) => write!(f, "astore {}", idx),
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
            Instruction::LAdd => write!(f, "ladd"),
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
            Instruction::GetStatic(fieldref) => write!(
                f,
                "getstatic {}.{}: {}",
                fieldref.class_name,
                fieldref.name,
                fieldref.desc,
            ),
            Instruction::PutStatic(fieldref) => write!(
                f,
                "putstatic {}.{}: {}",
                fieldref.class_name,
                fieldref.name,
                fieldref.desc,
            ),
            Instruction::GetField(fieldref) => write!(
                f,
                "getfield {}.{}: {}",
                fieldref.class_name,
                fieldref.name,
                fieldref.desc,
            ),
            Instruction::PutField(fieldref) => write!(
                f,
                "putfield {}.{}: {}",
                fieldref.class_name,
                fieldref.name,
                fieldref.desc,
            ),
            Instruction::InvokeVirtual(methodref) => write!(
                f,
                "invokevirtual {}.{}: {}",
                methodref.class_name,
                methodref.name,
                methodref.desc,
            ),
            Instruction::InvokeSpecial(methodref) => write!(
                f,
                "invokespecial {}.{}: {}",
                methodref.class_name,
                methodref.name,
                methodref.desc,
            ),
            Instruction::InvokeStatic(methodref) => write!(
                f,
                "invokestatic {}.{}: {}",
                methodref.class_name,
                methodref.name,
                methodref.desc,
            ),
            Instruction::InvokeInterface(idx, count) => write!(
                f,
                "invokeinterface {}, {}",
                idx.inner(),
                count
            ),
            Instruction::InvokeDynamic(idx) => write!(
                f,
                "invokedynamic {}",
                idx.inner()
            ),
            Instruction::New(tpe) => write!(
                f,
                "new {}",
                tpe
            ),
            Instruction::NewArray(tpe) => write!(f, "newarray (const {})", tpe),
            Instruction::ANewArray => write!(f, "anewarray"),
            Instruction::ArrayLength => write!(f, "arraylength"),
            Instruction::AThrow => write!(f, "athrow"),
            Instruction::CheckCast(tpe) => write!(
                f,
                "checkcast {}",
                tpe
            ),
            Instruction::InstanceOf(tpe) => write!(
                f,
                "instanceof {}",
                tpe
            ),
            Instruction::MonitorEnter => write!(f, "monitorenter"),
            Instruction::MonitorExit => write!(f, "monitorexit"),
            Instruction::MultiANewArray(tpe, dimensions) => write!(
                f,
                "multianewarray {}, {}",
                tpe,
                dimensions
            ),
            Instruction::IfNull(offset) => write!(f, "ifnull {}", offset),
            Instruction::IfNonNull(offset) => write!(f, "ifnonnull {}", offset),
            Instruction::GotoW(offset) => write!(f, "gotow {}", offset),
            Instruction::JsrW(offset) => write!(f, "jsrw {}", offset),
        }
    }
}

pub enum Instruction<'method, 'cls> {
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
    BIPush(i8),
    SIPush(i16),
    Ldc(&'method Constant<'cls>),
    LdcW(&'method Constant<'cls>),
    Ldc2W(&'method Constant<'cls>),
    ILoad(u16),
    LLoad(u16),
    FLoad(u16),
    DLoad(u16),
    ALoad(u16),
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
    LALoad,
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
    AStore(u16),
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
    LAdd,
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
    Goto(i16),
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
    GetStatic(&'method FieldRef<'cls>),
    PutStatic(&'method FieldRef<'cls>),
    GetField(&'method FieldRef<'cls>),
    PutField(&'method FieldRef<'cls>),
    InvokeVirtual(&'method MethodRef<'cls>),
    InvokeSpecial(&'method MethodRef<'cls>),
    InvokeStatic(&'method MethodRef<'cls>),
    InvokeInterface(ConstantIdx, u8),
    InvokeDynamic(ConstantIdx),
    New(&'cls str),
    NewArray(u8),
    ANewArray,
    ArrayLength,
    AThrow,
    CheckCast(&'cls str),
    InstanceOf(&'cls str),
    MonitorEnter,
    MonitorExit,
    MultiANewArray(&'cls str, u8),
    IfNull(i16),
    IfNonNull(i16),
    GotoW(i32),
    JsrW(i32),
}