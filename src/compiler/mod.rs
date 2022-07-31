use std::collections::HashMap;

mod ir {
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

    #[derive(Copy, Clone, Debug)]
    pub enum ValueType {
        Byte,
        Short,
        Int,
        Long,
        Float,
        Double,
        Ref,
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
        pool: ValuePool,
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
        ConstInt { result: ValueRef, value: i32 },
        IntAdd { result: ValueRef, left: ValueRef, right: ValueRef },
        IReturn { retval: ValueRef },
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

impl ZvmMethod {
    pub fn compile(&self) -> Box<[u8]> {
        use cranelift_codegen::entity::EntityRef;
        use cranelift_codegen::ir::types::*;
        use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder, Signature};
        use cranelift_codegen::isa::CallConv;
        use cranelift_codegen::settings;
        use cranelift_codegen::verifier::verify_function;
        use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

        let mut sig = Signature::new(CallConv::SystemV);
        if let Some(v) = self.returns {
            let retval = &self.locals[v.id];
            sig.returns.push(AbiParam::new(ty_zvm_to_cranelift(retval.ty)));
        }
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
        let mut vars: HashMap<ir::ValueRef, Variable> = HashMap::new();

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
                let var = Variable::new(i);
                builder.declare_var(var, ty_zvm_to_cranelift(local.ty));
                vars.insert(ir::ValueRef::local(i), var);
            }

            let block0 = builder.create_block();
            builder.switch_to_block(block0);
            builder.seal_block(block0);
            for inst in self.blocks[0].instructions().iter() {
                match inst {
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
                    ir::Instruction::IReturn { retval } => {
                        let retval = vars.get(&retval).expect("result var is defined");
                        let retval = builder.use_var(*retval);
                        builder.ins().return_(&[retval]);
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

        bytes.to_vec().into_boxed_slice()
    }
}

struct TranslatorState {
    current_block: ir::Block,
    blocks: Vec<ir::Block>,
    arguments: Vec<ir::Argument>,
    locals: Vec<ir::Local>,
    returns: Option<ir::ValueRef>,
    operand_stack: Vec<ir::ValueRef>,
}

impl TranslatorState {
    fn inst(&mut self, inst: ir::Instruction) {
        self.current_block.append(inst);
    }

    fn new() -> Self {
        TranslatorState {
            current_block: ir::Block::new(),
            blocks: Vec::new(),
            arguments: Vec::new(),
            locals: Vec::new(),
            returns: None,
            operand_stack: Vec::new(),
        }
    }

    fn into_method(mut self) -> ZvmMethod {
        if !self.operand_stack.is_empty() {
            panic!("translation leaves operand stack non-empty: bytecode is malformed. items remaining: {:?}", &self.operand_stack);
        };

        ZvmMethod {
            locals: self.locals,
            arguments: self.arguments,
            blocks: vec![self.current_block],
            returns: self.returns,
        }
    }

    fn new_argument(&mut self, ty: ir::ValueType) -> ir::ValueRef {
        self.arguments.push(ir::Argument { ty });
        ir::ValueRef::argument(self.arguments.len() - 1)
    }

    fn new_local(&mut self, ty: ir::ValueType) -> ir::ValueRef {
        self.locals.push(ir::Local { ty });
        ir::ValueRef::local(self.locals.len() - 1)
    }

    fn pop_typed(&mut self, ty: ir::ValueType) -> ir::ValueRef {
        if let Some(v) = self.operand_stack.pop() {
            v
        } else {
            // if we'd underflow the operand stack, that implies that the stack should have had a
            // value, and values that should have been present before interpreting bytecode are
            // function arguments.
            self.new_argument(ty)
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

pub fn bytecode2ir(method: &MethodBody) -> ZvmMethod {
    let mut translator = TranslatorState::new();

    let mut instructions = method.iter_from(0);
    while let Some(instruction) = instructions.next() {
        match instruction {
            Instruction::Nop => {},
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
            Instruction::IAdd => {
                let left = translator.pop_typed(ir::ValueType::Int);
                let right = translator.pop_typed(ir::ValueType::Int);
                let result = translator.new_local(ir::ValueType::Int);
                translator.inst(ir::Instruction::IntAdd { result, left, right });
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
                translator.returns = Some(retval);
                translator.inst(ir::Instruction::IReturn { retval });
            }
            _ => { panic!() }
        }
    }

    translator.into_method()
}

#[test]
fn test_translator() {
    use std::collections::HashMap;
    use crate::class_file::validated;

    let mut instructions: Vec<validated::Instruction> = vec![
        Instruction::Nop,
        Instruction::IConst0,
        Instruction::IConst1,
        Instruction::IAdd,
        // Instruction::Pop,
        // Instruction::Return,
        Instruction::IReturn,
    ];
    println!("jvm instructions:");
    for inst in instructions.iter() {
        println!("  {}", inst);
    }
    println!("");
    let bytes = validated::assemble(instructions);
    println!("jvm bytecode: {:02x?}", bytes);

    let method = MethodBody {
        max_stack: 65535,
        max_locals: 65535,
        bytes,
        exception_info: Vec::new(),
        class_refs: HashMap::new(),
        field_refs: HashMap::new(),
        method_refs: HashMap::new(),
        const_refs: HashMap::new(),
    };

    let result = bytecode2ir(&method);
    println!("");
    println!("ir form: {:?}", result);
    println!("");
    let code = result.compile();

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
}
