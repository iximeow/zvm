use std::collections::HashMap;
use std::rc::Rc;

use crate::virtual_machine::Arg;
use crate::class_file::validated::FieldRef;

pub mod ir {
    use std::fmt;
    use std::rc::Rc;

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

    #[derive(Debug)]
    pub enum Instruction {
        LoadArg { result: ValueRef, source: ValueRef, id: usize },
        ConstInt { result: ValueRef, value: i32 },
        ConstLong { result: ValueRef, value: i64 },
        IntAdd { result: ValueRef, left: ValueRef, right: ValueRef },
        IntXor { result: ValueRef, left: ValueRef, right: ValueRef },
        IReturn { retval: ValueRef },
        TypeAdjust { value: ValueRef, current_ty: ValueType, result: ValueRef, new_ty: ValueType },
        GetField { result: ValueRef, object: ValueRef, field_desc: LayoutFieldRef },
// TODO: alloc is where we need to finally introduce some notion of translation state...
//        Alloc { result: ValueRef, layout_id: LayoutId },
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
    pub struct LayoutId(usize);

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

        pub fn get_layout_id(&self, name: &str) -> Option<LayoutId> {
            self.layouts_by_name.get(name).cloned()
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
    }

    impl fmt::Debug for TranslationError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                TranslationError::UnsupportedInstruction(inst) => {
                    write!(f, "TranslationError::UnsupportedInstruction({})", inst)
                }
            }
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

enum CompileError {
    InvalidLoad(&'static str)
}

impl ZvmMethod {
    pub fn compile(&self) -> Result<Box<[u8]>, ir::CompileError> {
        use cranelift_codegen::entity::EntityRef;
        use cranelift_codegen::ir::types::*;
        use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder, MemFlags, Signature};
        use cranelift_codegen::isa::CallConv;
        use cranelift_codegen::settings;
        use cranelift_codegen::verifier::verify_function;
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
            let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

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
                    /*
                    ir::Instruction::Alloc { result, layout_id } => {
                        let result = vars.get(&result).expect("result var is defined");
                        fn generate_native_alloc(&self, builder: &mut FunctionBuilder, layout_id: LayoutId) -> Result<Value, CompileError> {
                            let call = builder.ins().call_indirect(
                                self.managed_alloc_signature(),
                                self.get_managed_alloc(),
                                &[self.get_vmref(), layout_id]
                            );
                            let results = builder.inst_results(call);
                            match results {
                                &[value] => Ok(value),
                                other => { return Err(CompileError::InvalidSignature("managed alloc returned multiple values?")); }
                            }
                        }
                        let value = self.generate_native_alloc(layout_id).expect("no translation error");
                        builder.def_var(*result, value);
                    }
                    */
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

        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&func, &flags);
        println!("{}", func.display());
        if let Err(errors) = res {
            panic!("{}", errors);
        }

        let target = cranelift_codegen::isa::lookup_by_name("x86_64").unwrap().finish(flags).unwrap();
        let res = target.compile_function(&func, false).unwrap();
        let bytes = res.buffer.data();

        Ok(bytes.to_vec().into_boxed_slice())
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
}

use crate::class_file::validated::{MethodBody, Instruction};

pub fn bytecode2ir(layouts: &ir::LayoutsInfo, method: &MethodBody, sig: (Vec<Arg>, Option<Arg>)) -> Result<ZvmMethod, ir::TranslationError> {
    let mut translator = TranslatorState::new(layouts);

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
                fn desc_to_ir_valuety(desc: &str) -> ir::ValueType {
                    ir::ValueType::Int
                }

                let layout_id = translator.layouts.get_layout_id(&fieldref.class_name);
                if layout_id.is_none() {
                    panic!("layout for '{}' is not declared", &fieldref.class_name);
                }
                let layout_id = layout_id.expect("TODO: layout is declared ahead of time");
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
            Instruction::BIPush(b) => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: b as i32 })
            }
            Instruction::SIPush(s) => {
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::ConstInt { result, value: s as i32 })
            }
            other => {
                return Err(ir::TranslationError::UnsupportedInstruction(other));
            }
        }
    }

    Ok(translator.into_method())
}

#[test]
fn test_translator() {
    use crate::class_file::validated;

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
    jit(args.as_slice(), instructions, "()I");
}

#[test]
fn test_add_args() {
    use crate::class_file::validated;

    let instructions: Vec<validated::Instruction> = vec![
        Instruction::ILoad0,
        Instruction::ILoad1,
        Instruction::IAdd,
        Instruction::IReturn,
    ];

    let args = &[20, 4];
    jit(args.as_slice(), instructions, "(II)I");
}

#[test]
fn test_field_access() {
    use crate::class_file::validated;

    let instructions: Vec<validated::Instruction> = vec![
        Instruction::ALoad0,
        Instruction::GetField(Rc::new(FieldRef { class_name: "java/lang/Integer".to_owned(), name: "value".to_owned(), desc: "I".to_owned() })),
        Instruction::ILoad1,
        Instruction::IAdd,
        Instruction::IReturn,
    ];

    let definitely_an_integer = [0xdeadbeef_cafebabe_u64 as i64, 20i64];
    let integer_ref: *const [i64; 2] = &definitely_an_integer as *const [i64; 2];
    let args = &[integer_ref as i64, 4];
    jit(args.as_slice(), instructions, "(Ljava/lang/Integer;I)I");
}

use crate::class_file::validated;

fn jit(args: &[i64], instructions: Vec<validated::Instruction>, signature: &'static str) {
    println!("jvm instructions:");
    for inst in instructions.iter() {
        println!("  {}", inst);
    }
    println!("");
    let method = validated::assemble(instructions);
    println!("jvm bytecode: {:02x?}", &method.bytes);

    let mut layouts = ir::LayoutsInfo::new();
    layouts.declare({
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


    let result = bytecode2ir(&layouts, &method, crate::virtual_machine::parse_signature_string(signature).expect("parses")).expect("translates");
    println!("");
    println!("ir form: {:?}", result);
    println!("");
    let code = result.compile().expect("compile succeeds");

    use yaxpeax_arch::LengthedInstruction;
    let decoder = yaxpeax_x86::amd64::InstDecoder::default();
    let mut addr = 0;
    while let Ok(inst) = decoder.decode_slice(&code[addr..]) {
        let len = inst.len().to_const() as usize;
        print!("{:#08x}: ", addr);
        for i in 0..8 {
            // skip 8-len slots...
            if i < (8 - len) {
                print!("  ");
            } else {
                let byte = i - (8 - len);
                print!("{:02x}", code[addr..][byte]);
            }
        }
        println!(": {}", inst);
        addr += len;
    }

    {
        let page_addr = code.as_ptr() as usize & !0xfff;
        let prot_len = (code.len() + 0xfff) & 0xfff;
        let res = unsafe { libc::mprotect(page_addr as *mut std::ffi::c_void, prot_len, libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC) };
        eprintln!("mprotect res: {}", res);
    }
    let res = jitcall(args, unsafe { std::mem::transmute(code.as_ptr()) });
    println!("result: {}", res);
}

fn jitcall(args: &[i64], code: fn(i64, i64, i64, i64, i64, i64) -> i64) -> i64 {
    // TODO: it is here that we would translate object arguments and returns to/from native impls
    let mut actual_args = [0i64; 6];
    for i in 0..args.len() {
        actual_args[i] = args[i];
    }
    code(actual_args[0], actual_args[1], actual_args[2], actual_args[3], actual_args[4], actual_args[0])
}
