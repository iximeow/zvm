use std::collections::HashMap;
use std::rc::Rc;

use crate::virtual_machine::Arg;
use crate::class_file::validated::ClassFile;

pub mod ir {
    use super::StructLayout;
    use std::fmt;

    #[derive(Debug)]
    pub enum Value {
        Argument(Argument),
        Local(Local),
        Const(Const),
    }

    #[derive(Debug)]
    pub struct Argument {
        pub ty: ValueType,
    }

    #[derive(Debug)]
    pub struct Local {
        pub ty: ValueType,
    }

    #[derive(Debug)]
    pub enum Const {
        Byte(i8),
        Short(i16),
        Int(i32),
        Long(i64),
        Float(f32),
        Double(f64),
        Null,
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub enum ValueType {
        Byte,
        Short,
        Int,
        Long,
        Float,
        Double,
        Ref,
    }

    impl ValueType {
        fn size(&self) -> usize {
            use ValueType::*;

            match self {
                Byte => 1,
                Short => 2,
                Int => 4,
                Long => 8,
                Float => 4,
                Double => 8,
                Ref => std::mem::size_of::<usize>(),
            }
        }
    }

    impl fmt::Display for ValueType {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                ValueType::Byte => f.write_str("byte"),
                ValueType::Short => f.write_str("short"),
                ValueType::Int => f.write_str("int"),
                ValueType::Long => f.write_str("long"),
                ValueType::Float => f.write_str("float"),
                ValueType::Double => f.write_str("double"),
                ValueType::Ref => f.write_str("ref"),
            }
        }
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub enum ValuePool {
        Argument,
        Local,
        Const,
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub struct ValueRef {
        pub(crate) id: usize,
        pub(crate) pool: ValuePool,
    }

    impl ValueRef {
        pub fn argument(id: usize) -> Self {
            Self {
                id,
                pool: ValuePool::Argument,
            }
        }

        pub fn local(id: usize) -> Self {
            Self {
                id,
                pool: ValuePool::Local,
            }
        }

        pub fn constant(id: usize) -> Self {
            Self {
                id,
                pool: ValuePool::Const,
            }
        }
    }

    struct FunctionRef {
        name: String,
    }

    #[derive(Debug)]
    pub enum Instruction {
        LoadArg { result: ValueRef, source: ValueRef, id: usize },
        ConstInt { result: ValueRef, value: i32 },
        ConstLong { result: ValueRef, value: i64 },
        IntAdd { result: ValueRef, left: ValueRef, right: ValueRef },
        IntXor { result: ValueRef, left: ValueRef, right: ValueRef },
        AReturn { retval: ValueRef },
        IReturn { retval: ValueRef },
        TypeAdjust { value: ValueRef, current_ty: ValueType, result: ValueRef, new_ty: ValueType },
        GetField { result: ValueRef, object: ValueRef, field_desc: LayoutFieldRef },
        Alloc { result: ValueRef, layout_id: LayoutId },
        Dealloc { value: ValueRef },
        CallImport { result: Option<ValueRef>, name: String, sig: (Vec<ValueType>, Option<ValueType>), args: Vec<ValueRef> },
        Return,
    }

    #[derive(Debug)]
    pub struct Block {
        instructions: Vec<Instruction>,
    }

    impl Block {
        pub fn new() -> Self {
            Self {
                instructions: Vec::new(),
            }
        }

        pub fn append(&mut self, inst: Instruction) {
            self.instructions.push(inst);
        }

        pub fn instructions(&self) -> &[Instruction] {
            &self.instructions
        }
    }

    #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
    pub struct LayoutId(pub usize);

    impl LayoutId {
        pub fn as_usize(&self) -> usize {
            self.0
        }
    }

    /// a (sorted by offset) list of fields, their types, and their names. this is in contrast to
    /// `StructLayout`, which is lower-level and discusses only the size of the backing allocation
    /// as well as the id of this high-level layout intended to be stored in that allocation.
    #[derive(Debug)]
    pub struct Layout {
        members: Vec<LayoutFieldRef>,
    }

    impl Layout {
        pub fn new() -> Self {
            Layout {
                members: Vec::new()
            }
        }

        pub fn add_field(&mut self, field_layout: LayoutFieldRef) {
            self.members.push(field_layout);
        }

        pub fn size(&self) -> usize {
            self.members.iter().map(|x| x.offset as usize + x.ty.size()).max().unwrap_or(0)
        }

        pub fn align(&self) -> usize {
            // good enough for now
            8
        }

        pub fn as_tagged_zvm(&self, layout_id: u64) -> StructLayout {
            StructLayout {
                size: self.size(),
                align: self.align(),
                layout_id,
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct LayoutFieldRef {
        // TODO: this limits even arrays to 2gb in max size. cranelift field offsets are Offset32.
        // how troublesome is this..?
        pub offset: i32,
        // TODO: record the size of the referenced field. probably not going to be directly useful
        // (should match `ValueType`'s size) but will probably become interesting if zvm directly
        // supports inline structs at some point?
        // pub size: i32,
        pub ty: ValueType,
        pub name: String,
    }

    #[derive(Debug)]
    pub struct LayoutsInfo {
        layouts_by_name: std::collections::HashMap<String, LayoutId>,
        layouts: Vec<Layout>,
    }

    impl LayoutsInfo {
        pub fn new() -> Self {
            LayoutsInfo {
                layouts_by_name: std::collections::HashMap::new(),
                layouts: Vec::new(),
            }
        }

        pub fn declare(&mut self, layout: Layout, name: String) {
            let id = self.layouts.len();
            self.layouts.push(layout);
            self.layouts_by_name.insert(name, LayoutId(id));
        }

        pub fn get_layout(&self, layout_id: LayoutId) -> &Layout {
            self.layouts.get(layout_id.0)
                .expect("layout id implies the layout exists")
        }

        pub fn get_layout_id(&self, name: &str) -> Result<LayoutId, TranslationError> {
            println!("looking up {} in...", name);
            for (i, l) in self.layouts_by_name.iter().enumerate() {
                println!("  {}: {:?}", i, l);
            }
            self.layouts_by_name.get(name).cloned()
                .ok_or_else(|| {
                    TranslationError::UnknownType(name.to_owned())
                })
        }

        pub fn get_field_in_layout(&self, layout_id: LayoutId, name: &str) -> &LayoutFieldRef {
            self
                .get_layout(layout_id)
                .members
                .iter()
                .find(|member| member.name == name)
                .expect("TODO: compile error; expect field exists..")
        }
    }

    /// any of the handful of errors that can occur when compiling zvm::ir::Instruction to native
    /// code
    #[derive(Debug)]
    pub enum CompileError {
        InvalidLoad(&'static str),
        InvalidSignature(&'static str),
        InvalidTypeAdjust(ValueRef, ValueType, ValueRef, ValueType),
    }

    /// any of the errors tht can occur when translating jvm bytecode to `zvm::ir` structures. for
    /// example, reaching unsupported instructions or code patterns will result in a
    /// `TranslationError`.
    pub enum TranslationError {
        UnsupportedInstruction(crate::class_file::validated::Instruction),
        UnknownType(String),
    }

    impl fmt::Debug for TranslationError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                TranslationError::UnsupportedInstruction(inst) => {
                    write!(f, "TranslationError::UnsupportedInstruction({})", inst)
                }
                TranslationError::UnknownType(s) => {
                    write!(f, "TranslationError::UnknownType({})", s)
                }
            }
        }
    }
}

pub struct FunctionEmitter {
    bytes: *mut u8,
    len: usize,
}

impl FunctionEmitter {
    fn data_mut(&mut self) -> &mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut(self.bytes, self.len)
        }
    }
    fn data(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(self.bytes, self.len)
        }
    }
    fn link(&mut self, relocs: &[cranelift_codegen::MachReloc], rt_info: &dyn RuntimeInfo) -> Result<(), crate::compiler::ir::CompileError> {
        use cranelift_codegen::binemit::Reloc;

        let data = self.data_mut();
        for reloc in relocs.iter() {
            match reloc.kind {
                Reloc::Abs8 => {
                    let addr = rt_info.func_addr(&reloc.name).expect("name exists");
                    data[reloc.offset as usize..][..8].copy_from_slice(&addr.to_le_bytes());
                },
                other => {
                    panic!("unhandled reloc kind: {:?}", other);
                }
            }
        }

        Ok(())
    }
    fn finalize(self) -> (*mut u8, usize) {
        let page_addr = self.bytes as usize & !0xfff;
        let prot_len = (self.len + 0xfff) & 0xfff;
        let res = unsafe { libc::mprotect(page_addr as *mut std::ffi::c_void, prot_len, libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC) };
        eprintln!("mprotect res: {}", res);

        let prot_res = unsafe { libc::mprotect(self.bytes as *mut std::ffi::c_void, self.len, libc::PROT_READ | libc::PROT_EXEC) };
        if prot_res != 0 {
            panic!("mprotect({:p}+{:#x}): {:?}", self.bytes, self.len, prot_res);
        }
        (self.bytes, self.len)
    }
}

pub struct CompiledMethod {
    bytes: *mut u8,
    len: usize,
    relocs: Box<[cranelift_codegen::MachReloc]>,
    // going to want to record stack maps as well probably
    // maybe call sites too
}

impl CompiledMethod {
    fn data_mut(&mut self) -> &mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut(self.bytes, self.len)
        }
    }
    fn data(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(self.bytes, self.len)
        }
    }
}

#[derive(Debug)]
pub struct ZvmMethod {
    arguments: Vec<ir::Argument>,
    locals: Vec<ir::Local>,
    blocks: Vec<ir::Block>,
    returns: Option<ir::ValueRef>,
    // map from some ir::Block to an address in the method bytecode
//    block_map: HashMap<ir::BlockRef, usize>,
}

pub trait RuntimeInfo: Sync {
//    fn addr_of(&self, extname: cranelift_codegen::ir::ExternalName) ->
    fn layouts(&self) -> &ir::LayoutsInfo;
    /// alloc a `FunctionEmitter` for a function of size `size`.
    ///
    /// the region allocated here will be read-write, but at an address selected so that this
    /// entire region can be moved from `rw-` to `r-x` when emission is complete. implementations
    /// should take care to not have other live code overlapping in the last page referenced, as
    /// memory permissions require rounding up to the nearest page granularity, and so the last
    /// page will be made `rw-` regardless of contents.
    fn alloc_function(&self, size: usize) -> Result<FunctionEmitter, &'static str>;
    fn func_addr(&self, name: &cranelift_codegen::ir::ExternalName) -> Result<usize, &'static str>;
    fn declare_layout(&mut self, layout: ir::Layout, name: String) -> Result<(), &'static str>;

    /// allocate an entire ZVM object suitable for representation of the provided layout.
    fn obj_alloc(&mut self, layout: StructLayout) -> Option<*mut u8>;
    // deallocate a ZVM object representing the provided layout. this implementation is left as an
    // exercise to the reader (for now)
//    fn obj_dealloc(&mut self, layout: &ir::Layout) -> Option<*const u8>;
}

impl ZvmMethod {
    pub fn compile(&self, rt_info: &dyn RuntimeInfo) -> Result<CompiledMethod, ir::CompileError> {
        use cranelift_codegen::entity::EntityRef;
        use cranelift_codegen::ir::types::*;
        use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder, MemFlags, Signature};
        use cranelift_codegen::isa::CallConv;
        use cranelift_codegen::settings;
        use cranelift_codegen::verifier::verify_function;
        use cranelift_codegen::ir::ExtFuncData;
        use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

        let mut sig = Signature::new(CallConv::SystemV);
        let mut vars: HashMap<ir::ValueRef, Variable> = HashMap::new();
        if let Some(v) = self.returns {
            let retval = &self.locals[v.id];
            sig.returns.push(AbiParam::new(ty_zvm_to_cranelift(retval.ty)));
        }
        for arg in self.arguments.iter() {
            sig.params.push(AbiParam::new(ty_zvm_to_cranelift(arg.ty)));
        }
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);

        fn ty_zvm_to_cranelift(ty: ir::ValueType) -> cranelift_codegen::ir::types::Type {
            match ty {
                ir::ValueType::Byte => {
                    cranelift_codegen::ir::types::I32
                },
                ir::ValueType::Short => {
                    cranelift_codegen::ir::types::I32
                },
                ir::ValueType::Int => {
                    cranelift_codegen::ir::types::I32
                },
                ir::ValueType::Long => {
                    cranelift_codegen::ir::types::I64
                },
                ir::ValueType::Float => {
                    cranelift_codegen::ir::types::F32
                },
                ir::ValueType::Double => {
                    cranelift_codegen::ir::types::F64
                },
                ir::ValueType::Ref => {
                    // so ref types are going to be treated as.. pointers.. into a method-local
                    // table maybe? hmm...
                    cranelift_codegen::ir::types::I64
                },
            }
        }

        {
            let managed_alloc_name = ExternalName::User { namespace: 0, index: 0 };
            let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);
//            let user_name_ref = builder.declare_imported_user_function(managed_alloc_name);

            for (i, local) in self.locals.iter().enumerate() {
                let var = Variable::new(i << 2);
                builder.declare_var(var, ty_zvm_to_cranelift(local.ty));
                vars.insert(ir::ValueRef::local(i), var);
            }

            let block0 = builder.create_block();
            builder.append_block_params_for_function_params(block0);

            for (i, arg) in self.arguments.iter().enumerate() {
                let var = Variable::new((i << 2) | 3);
                builder.declare_var(var, ty_zvm_to_cranelift(arg.ty));
                vars.insert(ir::ValueRef::argument(i), var);
            }

            builder.switch_to_block(block0);
            builder.seal_block(block0);
            for inst in self.blocks[0].instructions().iter() {
                match inst {
                    ir::Instruction::LoadArg { result, source, id } => {
                        let result = vars.get(&result).expect("result var is defined");
                        let _source = vars.get(&source).expect("source var is defined");
                        let v = builder.block_params(block0)[*id];
                        builder.def_var(*result, v);
                    }
                    ir::Instruction::ConstLong { result, value } => {
                        let result = vars.get(&result).expect("result var is defined");
                        let v = builder.ins().iconst(I64, *value);
                        builder.def_var(*result, v);
                    }
                    ir::Instruction::ConstInt { result, value } => {
                        let result = vars.get(&result).expect("result var is defined");
                        let v = builder.ins().iconst(I32, *value as i64);
                        builder.def_var(*result, v);
                    }
                    ir::Instruction::IntAdd { result, left, right } => {
                        let left = vars.get(&left).expect("result var is defined");
                        let right = vars.get(&right).expect("result var is defined");
                        let left = builder.use_var(*left);
                        let right = builder.use_var(*right);

                        let v = builder.ins().iadd(left, right);
                        let result = vars.get(&result).expect("result var is defined");
                        builder.def_var(*result, v);
                    }
                    ir::Instruction::IntXor { result, left, right } => {
                        let left = vars.get(&left).expect("result var is defined");
                        let right = vars.get(&right).expect("result var is defined");
                        let left = builder.use_var(*left);
                        let right = builder.use_var(*right);

                        let v = builder.ins().bxor(left, right);
                        let result = vars.get(&result).expect("result var is defined");
                        builder.def_var(*result, v);
                    }
                    ir::Instruction::IReturn { retval } => {
                        let retval = vars.get(&retval).expect("result var is defined");
                        let retval = builder.use_var(*retval);
                        builder.ins().return_(&[retval]);
                    }
                    ir::Instruction::AReturn { retval } => {
                        let retval = vars.get(&retval).expect("result var is defined");
                        let retval = builder.use_var(*retval);
                        builder.ins().return_(&[retval]);
                    }
                    ir::Instruction::Alloc { result, layout_id } => {
                        let result = vars.get(&result).expect("result var is defined");
                        // TODO: this should be pointer-type. derived from the target triple, you
                        // know...
                        let managed_alloc_sig = builder.import_signature(Signature {
                            // TODO: takes a layoutid
                            params: vec![AbiParam::new(cranelift_codegen::ir::types::I64)],
                            // TODO: should return pointer-width
                            returns: vec![AbiParam::new(cranelift_codegen::ir::types::I64)],
                            // TODO: this should be the target-defined calling convention
                            call_conv: CallConv::SystemV,
                        });
                        let managed_alloc_funcref = builder.import_function(ExtFuncData {
                            name: ExternalName::User { namespace: 0, index: 0 },
                            signature: managed_alloc_sig,
                            // TODO: it'd be nice to say "true".. but jit addresses are probably
                            // >128mb from malloc on aarch64
                            colocated: false,
                        });
                        let managed_alloc = builder.ins().func_addr(cranelift_codegen::ir::types::I64, managed_alloc_funcref);

                        let layout_id = builder.ins().iconst(I64, layout_id.as_usize() as i64);

                        let call = builder.ins().call_indirect(
                            managed_alloc_sig,
                            managed_alloc,
                            &[layout_id]
                        );
                        let results = builder.inst_results(call);
                        let value = match results {
                            &[value] => value,
                            other => { return Err(ir::CompileError::InvalidSignature("managed alloc returned multiple values?")); }
                        };
                        builder.def_var(*result, value);
                    }
                    ir::Instruction::Dealloc { value } => {
                        panic!("dealloc is not yet supported");
                    }
                    ir::Instruction::CallImport { result, name, sig, args } => {
                        let params = vec![AbiParam::new(cranelift_codegen::ir::types::I64); sig.0.len()];
                        let returns = if let Some(ret) = sig.1 {
                            vec![AbiParam::new(cranelift_codegen::ir::types::I64); 1]
                        } else {
                            vec![]
                        };

                        let sigref = builder.import_signature(Signature {
                            params,
                            returns,
                            call_conv: CallConv::SystemV,
                        });

                        let func_ref = builder.import_function(ExtFuncData {
                            name: ExternalName::user(0, 1),
                            signature: sigref,
                            colocated: true
                        });
                        // TODO: ptr type
                        let func_addr = builder.ins().func_addr(cranelift_codegen::ir::types::I64, func_ref);

                        let mut cranelift_args: Vec<cranelift_codegen::ir::Value> = Vec::new();
                        for arg in args.iter() {
                            cranelift_args.push(builder.use_var(*vars.get(arg).expect("argument var is defined")));
                        }

                        let call = builder.ins().call_indirect(
                            sigref,
                            func_addr,
                            &cranelift_args,
                        );
                        let results = builder.inst_results(call);
                        match results {
                            &[] => {
                                /* nothing to do */
                            },
                            &[value] => {
                                let result = result.expect("result var exists if function is supposed to return a value");
                                let result = vars.get(&result).expect("result var is defined");
                                builder.def_var(*result, value);
                            }
                            other => { return Err(ir::CompileError::InvalidSignature("call returned multiple values?")); }
                        };
                    }
                    ir::Instruction::GetField { result, object, field_desc } => {
                        let object = vars.get(&object).expect("object var is defined");
                        let object_val = builder.use_var(*object);

                        let out = builder.ins().load(
                            ty_zvm_to_cranelift(field_desc.ty),
                            // zvm requires that layouts uphold alignment requirements and that
                            // `GetField` is used only for valid references of appropriate type -
                            // it's the responsibility of client code to ensure that types are not
                            // confused.
                            MemFlags::trusted(),
                            object_val,
                            field_desc.offset
                        );

                        let result = vars.get(&result).expect("result var is defined");
                        builder.def_var(*result, out);
                    }
                    ir::Instruction::TypeAdjust { value, current_ty, result, new_ty } => {
                        if current_ty == new_ty {
                            eprintln!("bogus typeadjust? converting {} to {}", current_ty, new_ty);
                            return Err(ir::CompileError::InvalidTypeAdjust(*value, *current_ty, *result, *new_ty));
                        }
                        match (current_ty, new_ty) {
                            (ir::ValueType::Int, ir::ValueType::Ref) => {
                            }
                            _ => {
                                panic!("");
                            }
                        }
                        eprintln!("haha not yet sorry");
                    }
                    ir::Instruction::Return => {
                        builder.ins().return_(&[]);
                    }
                }
            }

            builder.finalize();
        }

        use cranelift_codegen::settings::Configurable;
        let mut settings = settings::builder();
        settings.set("opt_level", "speed_and_size").expect("works");
        let flags = settings::Flags::new(settings);
        let res = verify_function(&func, &flags);
        println!("{}", func.display());
        if let Err(errors) = res {
            panic!("{}", errors);
        }

        let target = cranelift_codegen::isa::lookup_by_name("x86_64").unwrap().finish(flags).unwrap();
        let res = target.compile_function(&func, false).unwrap();
        let bytes = res.buffer.data();
        let relocs = res.buffer.relocs();

        let mut function_bytes = rt_info.alloc_function(bytes.len()).expect("alloc works");
        function_bytes.data_mut().copy_from_slice(&bytes[..]);
        function_bytes.link(relocs, rt_info)?;
        let (ptr, len) = function_bytes.finalize();

        Ok(CompiledMethod {
            bytes: ptr,
            len,
            relocs: relocs.to_vec().into_boxed_slice()
        })
    }
}

#[derive(Debug)]
struct TranslatorState<'layouts> {
    current_block: ir::Block,
    blocks: Vec<ir::Block>,
    arguments: Vec<ir::Argument>,
    locals: Vec<ir::Local>,
    jvm_to_native_map: HashMap<usize, ir::ValueRef>,
    returns: Option<ir::ValueRef>,
    operand_stack: Vec<ir::ValueRef>,
    layouts: &'layouts ir::LayoutsInfo,
}

#[allow(unused)]
impl<'layouts> TranslatorState<'layouts> {
    fn inst(&mut self, inst: ir::Instruction) {
        self.current_block.append(inst);
    }

    fn new(layouts: &'layouts ir::LayoutsInfo) -> Self {
        TranslatorState {
            current_block: ir::Block::new(),
            blocks: Vec::new(),
            arguments: Vec::new(),
            locals: Vec::new(),
            jvm_to_native_map: HashMap::new(),
            returns: None,
            operand_stack: Vec::new(),
            layouts,
        }
    }

    fn into_method(mut self) -> ZvmMethod {
        if !self.operand_stack.is_empty() {
            panic!("translation leaves operand stack non-empty: bytecode is malformed. items remaining: {:?}\nstate: {:?}", &self.operand_stack, self);
        };

        ZvmMethod {
            locals: self.locals,
            arguments: self.arguments,
            blocks: vec![self.current_block],
            returns: self.returns,
        }
    }

    fn get_argument(&mut self, arg_id: usize, ty: ir::ValueType) -> ir::ValueRef {
        if arg_id >= self.arguments.len() {
            panic!("more arguments than signature holds?");
        }
        let valref = &self.arguments[arg_id];
        assert_eq!(valref.ty, ty);
        ir::ValueRef::argument(arg_id)
    }

    fn get_jvm_local(&mut self, local_id: usize, ty: ir::ValueType) -> ir::ValueRef {
        if let Some(res) = self.jvm_to_native_map.get(&local_id) {
            let local = &self.locals[res.id];
            assert_eq!(local.ty, ty);
            return *res;
        }

        let res = self.new_local(ty);
        self.jvm_to_native_map.insert(local_id, res);
        res
    }

    fn load_argument(&mut self, id: usize, ty: ir::ValueType) {
        let source = if self.arguments.len() > id {
            self.get_argument(id, ty)
        } else {
            self.get_jvm_local(id - self.arguments.len(), ty)
        };
        let result = self.new_local(ty);
        self.inst(ir::Instruction::LoadArg { result, source, id });
        self.operand_stack.push(result);
    }

    fn new_argument(&mut self, ty: ir::ValueType) {
        self.arguments.push(ir::Argument { ty });
    }

    fn new_local(&mut self, ty: ir::ValueType) -> ir::ValueRef {
        self.locals.push(ir::Local { ty });
        ir::ValueRef::local(self.locals.len() - 1)
    }

    fn type_of(&self, ty: ir::ValueRef) -> ir::ValueType {
        match ty {
            ir::ValueRef { pool: ir::ValuePool::Local, id } => {
                self.locals[id].ty
            },
            ir::ValueRef { pool: ir::ValuePool::Argument, id } => {
                self.arguments[id].ty
            },
            ir::ValueRef { pool: ir::ValuePool::Const, id } => {
                panic!("no constants yet?");
            },
        }
    }

    fn pop_typed(&mut self, ty: ir::ValueType) -> ir::ValueRef {
        if let Some(v) = self.operand_stack.pop() {
            let v_ty = self.type_of(v);
            if v_ty != ty {
                panic!("type error in ir? popped {}, expected {}", v_ty, ty);
                /*
                eprintln!("inserting typecast from {} to {}", v_ty, ty);
                let result = self.new_local(ty);
                self.inst(ir::Instruction::TypeAdjust { value: v, current_ty: v_ty, result, new_ty: ty });
                result
                */
            } else {
                v
            }
        } else {
            panic!("underflowed operand stack? invalid bytecode?");
        }
    }

    fn pop_any(&mut self) -> ir::ValueRef {
        self.operand_stack.pop().expect("TODO: translation error - operand stack has a value to pop")
    }

    fn push(&mut self, v: ir::ValueRef) {
        self.operand_stack.push(v);
    }

    // create a new (or use an existing) ExternalName for fname, reserve a sig, etc...
    fn extdata(&mut self, fname: &str) -> cranelift_codegen::ir::ExtFuncData {
        panic!("aa");
        // let sig = Signature {
        //    params: vec![
        /*
        self.extfuncs.insert(ExternalName::User(0, self.extfuncs.len()), fname.to_string());
        cranelift_codegen::ir::ExtFuncData {
            name: ExternalName::User(0, self.extfuncs.len() - 1),
            signature: asdf,
            colocated: true
        }
        */
    }
}

use crate::class_file::validated::{MethodBody, Instruction};

fn desc_to_ir_valuety(desc: &str) -> ir::ValueType {
    if desc == "I" {
        ir::ValueType::Int
    } else {
        panic!("unhandled desc: {}", desc);
    }
}

pub fn bytecode2ir(rt_info: &mut dyn RuntimeInfo, method: &MethodBody, sig: (Vec<Arg>, Option<Arg>)) -> Result<ZvmMethod, ir::TranslationError> {
    let mut translator = TranslatorState::new(rt_info.layouts());

    for arg in sig.0.iter() {
        translator.new_argument(arg.ty);
    }

    let mut instructions = method.iter_from(0);
    while let Some(instruction) = instructions.next() {
        match instruction {
            Instruction::ALoad0 => {
                translator.load_argument(0, ir::ValueType::Ref);
            }
            Instruction::ALoad1 => {
                translator.load_argument(1, ir::ValueType::Ref);
            }
            Instruction::ALoad2 => {
                translator.load_argument(2, ir::ValueType::Ref);
            }
            Instruction::ALoad3 => {
                translator.load_argument(3, ir::ValueType::Ref);
            }
            Instruction::LLoad0 => {
                translator.load_argument(0, ir::ValueType::Long);
            }
            Instruction::LLoad1 => {
                translator.load_argument(1, ir::ValueType::Long);
            }
            Instruction::ILoad0 => {
                translator.load_argument(0, ir::ValueType::Int);
            }
            Instruction::ILoad1 => {
                translator.load_argument(1, ir::ValueType::Int);
            }
            Instruction::ILoad2 => {
                translator.load_argument(2, ir::ValueType::Int);
            }
            Instruction::ILoad3 => {
                translator.load_argument(3, ir::ValueType::Int);
            }
            Instruction::Nop => {},
            Instruction::LConst0 => {
                let result = translator.new_local(ir::ValueType::Long);
                translator.inst(ir::Instruction::ConstLong { result, value: 0 });
                translator.push(result);
            }
            Instruction::LConst1 => {
                let result = translator.new_local(ir::ValueType::Long);
                translator.inst(ir::Instruction::ConstLong { result, value: 1 });
                translator.push(result);
            }
            Instruction::IConst0 => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: 0 });
                translator.push(result);
            }
            Instruction::IConst1 => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: 1 });
                translator.push(result);
            }
            Instruction::IConst2 => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: 2 });
                translator.push(result);
            }
            Instruction::IConst3 => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: 3 });
                translator.push(result);
            }
            Instruction::IConst4 => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: 4 });
                translator.push(result);
            }
            Instruction::IConst5 => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: 5 });
                translator.push(result);
            }
            Instruction::IAdd => {
                let left = translator.pop_typed(ir::ValueType::Int);
                let right = translator.pop_typed(ir::ValueType::Int);
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::IntAdd { result, left, right });
                translator.push(result);
            }
            Instruction::IXor => {
                let left = translator.pop_typed(ir::ValueType::Int);
                let right = translator.pop_typed(ir::ValueType::Int);
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::IntXor { result, left, right });
                translator.push(result);
            }
            Instruction::GetField(fieldref) => {
                let layout_id = translator.layouts.get_layout_id(&fieldref.class_name)?;
                let field = translator.layouts.get_field_in_layout(layout_id, &fieldref.name);

                if desc_to_ir_valuety(&fieldref.desc) != field.ty {
                    panic!("TODO: inconsistency detected: field's referent is a different type than the declared layout");
                }

                let object = translator.pop_typed(ir::ValueType::Ref);
                let result = translator.new_local(field.ty);

                // TODO: would be nice to not clone the field name, here?
                translator.inst(ir::Instruction::GetField { result, object, field_desc: field.clone() });
                translator.push(result);
            }
            Instruction::Pop => {
                translator.pop_any();
            }
            Instruction::Return => {
                translator.inst(ir::Instruction::Return);
            }
            Instruction::IReturn => {
                let retval = translator.pop_typed(ir::ValueType::Int);
                // move this all into a function on `translator`
                translator.returns = Some(retval);
                translator.inst(ir::Instruction::IReturn { retval });
                // translator.apply_return();
            }
            Instruction::AReturn => {
                let retval = translator.pop_typed(ir::ValueType::Ref);
                // move this all into a function on `translator`
                translator.returns = Some(retval);
                translator.inst(ir::Instruction::AReturn { retval });
                // translator.apply_return();
            }
            Instruction::BIPush(b) => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: b as i32 });
                translator.push(result);
            }
            Instruction::SIPush(s) => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: s as i32 });
                translator.push(result);
            }
            Instruction::New(cls_name) => {
                let result = translator.new_local(ir::ValueType::Ref);
                let layout_id = translator.layouts.get_layout_id(&cls_name)?;
                translator.inst(ir::Instruction::Alloc { result, layout_id });
                translator.push(result);
            }
            Instruction::Dup => {
                let top = translator.pop_any();
                translator.push(top);
                translator.push(top);
            }
            Instruction::InvokeSpecial(method_ref) => {
                eprintln!("method ref desc: {}", method_ref.desc);
                let (arg_tys, ret_ty) = crate::virtual_machine::parse_signature_string(&method_ref.desc).expect("signature parses");

                // TODO: this leaks an Rc<MethodRef> into the function body? how to clean up
                let method_ref_rc = translator.new_local(ir::ValueType::Long);
                translator.inst(ir::Instruction::ConstLong { result: method_ref_rc, value: Rc::into_raw(method_ref) as usize as isize as i64 });

                let mut args = vec![
                    method_ref_rc,
                ];

                let mut managed_args = Vec::new();

                let mut zvm_arg_tys = vec![
                    ir::ValueType::Long,
                ];

                for arg in arg_tys.iter().rev() {
                    managed_args.push(translator.pop_typed(arg.ty));
                }

                // TODO: probably translate java/lang/Integer and friends to their primitive types?
                // maybe??
                let target = translator.pop_typed(ir::ValueType::Ref);
                zvm_arg_tys.push(ir::ValueType::Ref);
                managed_args.push(target);
                for arg in arg_tys.iter() {
                    zvm_arg_tys.push(arg.ty);
                }
                managed_args.reverse();
                args.append(&mut managed_args);
                let (result, result_ty) = if let Some(ret_ty) = ret_ty {
                    (Some(translator.new_local(ret_ty.ty)), Some(ret_ty.ty))
                } else {
                    (None, None)
                };
                // let extdata = translator.extdata("__vm_call_stub");

                translator.inst(ir::Instruction::CallImport {
                    result,
                    name: "__vm_call_stub".to_string(),
                    sig: (
                        zvm_arg_tys,
                        result_ty,
                    ),
                    args
                });
            }
            other => {
                eprintln!("wth {}", other);
                return Err(ir::TranslationError::UnsupportedInstruction(other));
            }
        }
    }

    Ok(translator.into_method())
}

use std::cell::RefCell;

static mut ZVM: Option<&'static mut dyn RuntimeInfo> = None;

/// a low-level description of an allocation suitable for storing a layout of id `layout_id`. this
/// is largely equivalent to a Rust `core::alloc::Layout`.
pub struct StructLayout {
    size: usize,
    align: usize,
    layout_id: u64,
}

impl StructLayout {
    const fn header_size() -> usize {
        // the zvm object header is.... currently.........
        //
        //    ------------------
        // 0  | layout id      |
        // 8  | misc           |
        // 16+| the actual obj |
        //    S ....           S
        //    ------------------
        core::mem::size_of::<usize>() +
            core::mem::size_of::<usize>()
    }

    const fn header_align() -> usize {
        // the object header is two usize, which i'm going to unilaterally declare as align=8 for
        // reasons of lazy
        8
    }
}

pub struct ZvmRuntime {
    layouts: ir::LayoutsInfo
}

use std::borrow::Borrow;
use crate::class_file::validated::MethodRef;
impl ZvmRuntime {
    pub fn init() -> Self {
        Self {
            layouts: ir::LayoutsInfo::new()
        }
    }

    // TODO: types
    fn runtime_alloc_func(layout_id: u64) -> usize {
        eprintln!("alloc requested for layout {}", layout_id);
        let vm = unsafe { ZVM.as_mut().unwrap() };
        let layout = vm.layouts().get_layout(crate::compiler::ir::LayoutId(layout_id as usize));
        eprintln!("layout: {:?}", layout);
        let struct_layout = layout.as_tagged_zvm(layout_id);
        let ptr = vm.obj_alloc(struct_layout);
        ptr.unwrap_or_else(|| {
            panic!("alloc failed for layout {}", layout_id);
        }) as usize
    }

    // TODO: types
    fn __vm_call_stub(method_ref: *const MethodRef, receiver: u64, arg: u64, arg2: u64) {
        let method_ref = unsafe { std::mem::transmute::<*const MethodRef, &MethodRef>(method_ref) };
        eprintln!("runtime witnessed call to {:?} on object {:#x} and args [{:#x}, {:#x}]", method_ref, receiver, arg, arg2);
    }
}

impl RuntimeInfo for ZvmRuntime {
    fn obj_alloc(&mut self, layout: StructLayout) -> Option<*mut u8> {
        let size = layout.size + StructLayout::header_size();
        let align = core::cmp::max(layout.align, StructLayout::header_align());

        let res = unsafe {
            std::alloc::alloc(core::alloc::Layout::from_size_align(size, align).unwrap())
        };

        if res.is_null() {
            None
        } else {
            Some(unsafe { res.offset(StructLayout::header_size() as isize) })
        }
    }

    fn layouts(&self) -> &ir::LayoutsInfo {
        &self.layouts
    }

    fn alloc_function(&self, size: usize) -> Result<FunctionEmitter, &'static str> {
        use libc::{PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS};

        let rounded_size = (size + 0xfff) & !0xfff;
        let ptr = unsafe {
            libc::mmap(std::ptr::null_mut(), rounded_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0) as *mut u8
        };
        if ptr == std::ptr::null_mut() {
            panic!("alloc fail ({:#x} bytes)", rounded_size);
        }
        Ok(FunctionEmitter {
            bytes: ptr,
            len: size,
        })
    }

    fn func_addr(&self, name: &cranelift_codegen::ir::ExternalName) -> Result<usize, &'static str> {
        match name {
            cranelift_codegen::ir::ExternalName::User { namespace: 0, index: 0 } => {
                Ok(Self::runtime_alloc_func as usize)
            },
            cranelift_codegen::ir::ExternalName::User { namespace: 0, index: 1 } => {
                Ok(Self::__vm_call_stub as usize)
            },
            other => {
                panic!("unknown extname: {:?}", other);
            }
        }
    }

    fn declare_layout(&mut self, layout: ir::Layout, name: String) -> Result<(), &'static str> {
        self.layouts.declare(layout, name);
        Ok(())
    }
}

#[test]
fn test_translator() {
    use crate::class_file::validated;

    let mut zvm_rt = ZvmRuntime::init();

    let instructions: Vec<validated::Instruction> = vec![
        Instruction::Nop,
        Instruction::IConst0,
        Instruction::IConst1,
        Instruction::IAdd,
        // Instruction::Pop,
        // Instruction::Return,
        Instruction::IReturn,
    ];

    let args = &[];
    jit(args.as_slice(), instructions, "()I", Vec::new(), &mut zvm_rt);
}

#[test]
fn test_add_args() {
    use crate::class_file::validated;

    let mut zvm_rt = ZvmRuntime::init();

    let instructions: Vec<validated::Instruction> = vec![
        Instruction::ILoad0,
        Instruction::ILoad1,
        Instruction::IAdd,
        Instruction::IReturn,
    ];

    let args = &[20, 4];
    jit(args.as_slice(), instructions, "(II)I", Vec::new(), &mut zvm_rt);
}

#[test]
fn test_field_access() {
    use crate::class_file::validated;

    let mut zvm_rt = ZvmRuntime::init();

    let instructions: Vec<validated::Instruction> = vec![
        Instruction::ALoad0,
        Instruction::GetField(Rc::new(crate::class_file::validated::FieldRef { class_name: "java/lang/Integer".to_owned(), name: "value".to_owned(), desc: "I".to_owned() })),
        Instruction::ILoad1,
        Instruction::IAdd,
        Instruction::IReturn,
    ];

    let definitely_an_integer = [0xdeadbeef_cafebabe_u64 as i64, 20i64];
    let integer_ref: *const [i64; 2] = &definitely_an_integer as *const [i64; 2];
    let args = &[integer_ref as i64, 4];
    jit(args.as_slice(), instructions, "(Ljava/lang/Integer;I)I", Vec::new(), &mut zvm_rt);
}

#[test]
fn test_object_return() {
    use crate::class_file::validated;

    let mut zvm_rt = ZvmRuntime::init();

    let mut classes: Vec<Rc<ClassFile>> = Vec::new();

    // pretend this method is, in fact, `test_method`.
    let custom_class: Rc<ClassFile> = Rc::new(
        crate::class_file::synthetic::SyntheticClassBuilder::<crate::SimpleJvmValue>::new("TestClass")
            .extends("java/lang/Object")
            .with_field("inner", "I")
            .with_method("<init>", "(LTestClass;J)V", None)
            .with_method("test_method", "()I", None)
            .validate().0);

    classes.push(Rc::clone(&custom_class));

    let instructions: Vec<validated::Instruction> = vec![
        Instruction::New(Rc::new(custom_class.this_class.clone())),
        Instruction::Dup,
        Instruction::ALoad0,
        Instruction::LLoad1,
        Instruction::InvokeSpecial(Rc::new(custom_class.method_ref("<init>", "(LTestClass;J)V").expect("method exists"))),
        Instruction::AReturn,
    ];

    let definitely_an_integer = [0xdeadbeef_cafebabe_u64 as i64, 20i64];
    let integer_ref: *const [i64; 2] = &definitely_an_integer as *const [i64; 2];
    let args = &[integer_ref as i64, 4];
    jit(args.as_slice(), instructions, "(Ljava/lang/String;J)LCustomClass;", classes, &mut zvm_rt);
}

use crate::class_file::validated;

fn jit(args: &[i64], instructions: Vec<validated::Instruction>, signature: &'static str, extra_classes: Vec<Rc<ClassFile>>, rt_info: &mut dyn RuntimeInfo) {
    println!("jvm instructions:");
    for inst in instructions.iter() {
        println!("  {}", inst);
    }
    println!("");
    let method = validated::assemble(instructions);
    println!("jvm bytecode: {:02x?}", &method.bytes);

    rt_info.declare_layout({
        let mut layout = ir::Layout::new();
        layout.add_field(ir::LayoutFieldRef {
            offset: 0,
            ty: ir::ValueType::Ref,
            name: "class".to_string()
        });
        layout.add_field(ir::LayoutFieldRef {
            offset: 8,
            ty: ir::ValueType::Int,
            name: "value".to_string()
        });
        layout
    }, "java/lang/Integer".to_string());
    for (i, cls) in extra_classes.iter().enumerate() {
        rt_info.declare_layout({
            let mut layout = ir::Layout::new();
            layout.add_field(ir::LayoutFieldRef {
                offset: 0,
                ty: ir::ValueType::Int,
                name: format!("STUB_layout_{}", i)
            });
            layout
        }, cls.this_class.to_string());
    }


    let result = bytecode2ir(rt_info, &method, crate::virtual_machine::parse_signature_string(signature).expect("parses")).expect("translates");
    println!("");
    println!("ir form: {:?}", result);
    println!("");
    let function = result.compile(rt_info).expect("compile succeeds");

    use yaxpeax_arch::LengthedInstruction;
    let decoder = yaxpeax_x86::amd64::InstDecoder::default();
    let mut addr = 0;
    let fn_bytes = function.data();
    while let Ok(inst) = decoder.decode_slice(&fn_bytes[addr..]) {
        let len = inst.len().to_const() as usize;
        print!("{:#08x}: ", addr);
        let sz = std::cmp::max(8, len);
        for i in 0..sz {
            // skip 8-len slots...
            if i < (sz - len) {
                print!("  ");
            } else {
                let byte = i - (sz - len);
                print!("{:02x}", fn_bytes[addr..][byte]);
            }
        }
        println!(": {}", inst);
        addr += len;
    }

    unsafe { ZVM = Some(std::mem::transmute::<&mut dyn RuntimeInfo, &'static mut dyn RuntimeInfo>(rt_info)); }
    let res = jitcall(args, unsafe { std::mem::transmute(function.data().as_ptr()) });
    unsafe { ZVM = None; }
    println!("result: {:x}", res);
}

fn jitcall(args: &[i64], code: fn(i64, i64, i64, i64, i64, i64) -> i64) -> i64 {
    // TODO: it is here that we would translate object arguments and returns to/from native impls
    let mut actual_args = [0i64; 6];
    for i in 0..args.len() {
        actual_args[i] = args[i];
    }
    code(actual_args[0], actual_args[1], actual_args[2], actual_args[3], actual_args[4], actual_args[0])
}
