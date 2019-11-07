use std::rc::Rc;
use std::str::FromStr;
use std::io::Cursor;
use std::collections::HashMap;

use crate::class_file::AccessFlags;
use crate::class_file::Constant;
use crate::class_file::ConstantIdx;
use crate::class_file::ClassFile;
use crate::class_file::ClassFileRef;
use crate::class_file::FieldInfo;
use crate::class_file::MethodAccessFlags;
use crate::class_file::MethodHandle;
use crate::class_file::MethodInfo;
use crate::class_file::read::FromReader;
use crate::class_file::attribute::Attribute;
use crate::class_file::instruction::Instruction;

struct CallFrame {
    offset: u32,
    arguments: Vec<Rc<Value>>,
    body: Rc<Attribute>,
    enclosing_class: Rc<ClassFile>,
    operand_stack: Vec<Rc<Value>>,
}

impl CallFrame {
    pub fn new(body: Rc<Attribute>, enclosing_class: Rc<ClassFile>, arguments:  Vec<Rc<Value>>) -> Self {
        CallFrame {
            offset:0 ,
            arguments,
            body,
            enclosing_class,
            operand_stack: Vec::new()
        }
    }
}

pub struct VMState {
    // Attribute is actually a Code (anything else is an error)
    call_stack: Vec<CallFrame>,
    static_instances: HashMap<ClassFileRef, HashMap<String, Rc<Value>>>,
}

impl VMState {
    pub fn new(code: Rc<Attribute>, method_class: Rc<ClassFile>, initial_args: Vec<Rc<Value>>) -> Self {
        let mut state = VMState {
            call_stack: Vec::new(),
            static_instances: HashMap::new(),
        };
        state.call_stack.push(CallFrame::new(code, method_class, initial_args));
        state
    }

    fn get_field(&mut self, class_ref: &Rc<ClassFile>, name: &str, ty: &str) -> Option<Rc<Value>> {
        let fields = self.static_instances.entry(ClassFileRef::of(class_ref)).or_insert_with(|| HashMap::new());
        if class_ref.has_static_field(name) {
            Some(Rc::clone(fields.entry(name.to_string()).or_insert_with(|| Rc::new(Value::Null(ty.to_owned())))))
        } else {
            None
        }
    }

    fn current_frame(&self) -> &CallFrame {
        self.call_stack.iter().rev().next().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack.iter_mut().rev().next().unwrap()
    }

    fn next_instruction(&mut self) -> Option<Instruction> {
        if self.call_stack.len() == 0 {
            return None;
        }

        let frame = self.current_frame_mut();
        let attr_ref: &Attribute = &*frame.body;
        if let Attribute::Code(_, _, code, _, _) = attr_ref {
            let mut instr_bytes = Cursor::new(code.as_slice());

            while (instr_bytes.position() as u32) < frame.offset {
                Instruction::read_from(&mut instr_bytes).unwrap();
            }

            let res = Instruction::read_from(&mut instr_bytes).ok();
            frame.offset = instr_bytes.position() as u32;
            res
        } else {
            panic!("call stack has a call record that does not have code");
        }
    }

    pub fn enter(&mut self, body: Rc<Attribute>, enclosing_class: Rc<ClassFile>, arguments: Vec<Rc<Value>>) {
        self.call_stack.push(CallFrame::new(body, enclosing_class, arguments));
    }

    pub fn leave(&mut self) {
        self.call_stack.pop().expect("stack is non-empty");
    }

    fn execute(&mut self, instruction: &Instruction, vm: &mut VirtualMachine) -> Result<(), VMError> {
        match instruction {
            Instruction::InvokeVirtual(idx) => {
                if let Some(Constant::Methodref(class_idx, name_and_type_idx)) = self.current_frame().enclosing_class.get_const(*idx) {
                    let method_class = self.current_frame().enclosing_class.get_const(*class_idx).unwrap();
                    let method_class_name = if let Constant::Class(class_name_idx) = method_class {
                        self.current_frame().enclosing_class.get_str(*class_name_idx).unwrap()
                    } else {
                        panic!("method's class is not a class?");
                    };
                    let target_class = vm.resolve_class(method_class_name).unwrap();
                    if let Some(Constant::NameAndType(name_idx, type_idx)) = self.current_frame().enclosing_class.get_const(*name_and_type_idx) {
                        let method_name = self.current_frame().enclosing_class.get_str(*name_idx).unwrap().to_string();
                        let method_type = self.current_frame().enclosing_class.get_str(*type_idx).unwrap().to_string();
                        let method = target_class.get_method(&method_name).expect("method exists");
                        // get method by name `method_name`
                        if method.access_flags.is_native() {
                            if let Some(native_method) = target_class.native_methods.get(&method_name) {
                                native_method(self, vm)
                            } else {
                                panic!("attempted to call native method with no implementation: {} - note, JNI resolution is not yet supported.", method_name);
                            }
                        } else {
                            interpreted_method_call(self, vm, method, target_class, &method_type)
                        }
                    } else {
                        Err(VMError::BadClass("fieldref name_and_type does not index a NameAndType"))
                    }
                } else {
                    Err(VMError::BadClass("getstatic constant pool idx does not index a Fieldref"))
                }
            }
            Instruction::InvokeStatic(idx) => {
                if let Some(Constant::Methodref(class_idx, name_and_type_idx)) = self.current_frame().enclosing_class.get_const(*idx) {
                    let method_class = self.current_frame().enclosing_class.get_const(*class_idx).unwrap();
                    let method_class_name = if let Constant::Class(class_name_idx) = method_class {
                        self.current_frame().enclosing_class.get_str(*class_name_idx).unwrap()
                    } else {
                        panic!("method's class is not a class?");
                    };
                    let target_class = vm.resolve_class(method_class_name).unwrap();
                    if let Some(Constant::NameAndType(name_idx, type_idx)) = self.current_frame().enclosing_class.get_const(*name_and_type_idx) {
                        let method_name = self.current_frame().enclosing_class.get_str(*name_idx).unwrap().to_string();
                        let method_type = self.current_frame().enclosing_class.get_str(*type_idx).unwrap().to_string();
                        let method = target_class.get_method(&method_name).expect("method exists");
                        // get method by name `method_name`
                        if method.access_flags.is_native() {
                            if let Some(native_method) = target_class.native_methods.get(&method_name) {
                                native_method(self, vm)
                            } else {
                                panic!("attempted to call native method with no implementation: {} - note, JNI resolution is not yet supported.", method_name);
                            }
                        } else {
                            interpreted_method_call(self, vm, method, target_class, &method_type)
                        }
                    } else {
                        Err(VMError::BadClass("fieldref name_and_type does not index a NameAndType"))
                    }
                } else {
                    Err(VMError::BadClass("getstatic constant pool idx does not index a Fieldref"))
                }

            }
            Instruction::GetStatic(idx) => {
                if let Some(Constant::Fieldref(class_idx, name_and_type_idx)) = self.current_frame().enclosing_class.get_const(*idx) {
                    let referent_class = self.current_frame().enclosing_class.get_const(*class_idx).unwrap();
                    let referent_class_name = if let Constant::Class(class_name_idx) = referent_class {
                        self.current_frame().enclosing_class.get_str(*class_name_idx).unwrap()
                    } else {
                        panic!("referent class is not a class?");
                    };
                    let target_class = vm.resolve_class(referent_class_name).unwrap();
                    if let Some(Constant::NameAndType(name_idx, type_idx)) = self.current_frame().enclosing_class.get_const(*name_and_type_idx) {
                        let referent_name = self.current_frame().enclosing_class.get_str(*name_idx).unwrap().to_string();
                        let referent_type = self.current_frame().enclosing_class.get_str(*type_idx).unwrap().to_string();
                        let value = self.get_field(&target_class, &referent_name, &referent_type).unwrap();
                        self.current_frame_mut().operand_stack.push(value);
                        Ok(())
                    } else {
                        Err(VMError::BadClass("fieldref name_and_type does not index a NameAndType"))
                    }
                } else {
                    Err(VMError::BadClass("getstatic constant pool idx does not index a Fieldref"))
                }
            }
            Instruction::Ldc(idx) => {
                let value = match self.current_frame().enclosing_class.get_const(*idx) {
                    Some(Constant::Integer(i)) => {
                        Rc::new(Value::Integer(*i as i32))
                    },
                    Some(Constant::Long(l)) => {
                        Rc::new(Value::Long(*l as i64))
                    }
                    Some(Constant::String(idx)) => {
                        if let Some(Constant::Utf8(data)) = self.current_frame().enclosing_class.get_const(*idx) {
                            Rc::new(Value::String(data.clone()))
                        } else {
                            return Err(VMError::BadClass("string ref is not utf8 data"));
                        }
                    }
                    _ => {
                        return Err(VMError::Unsupported("unsupported constant type for ldc"));
                    }
                };

                self.current_frame_mut().operand_stack.push(value);
                Ok(())
            }
            Instruction::Return => {
                self.leave();
                Ok(())
            }
            _ => {
                unimplemented!();
            }
        }
    }

    #[allow(dead_code)]
    fn return_value(&mut self) -> Option<Value> {
        // panic!("Hello there");
        None
    }
}

#[derive(Debug)]
pub enum Value {
    Integer(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(Vec<u8>),
    Null(String), // Null, of type `String`
}

impl Value {
    pub fn parse_from(s: &str) -> Option<Value> {
        if s == "null" {
            return Some(Value::Null("Object".to_string()));
        }

        if let Ok(v) = i64::from_str(s) {
            return Some(Value::Long(v));
        }

        if let Ok(v) = f64::from_str(s) {
            return Some(Value::Double(v));
        }

        if s.len() >= 2 && s.starts_with("\"") && s.ends_with("\"") {
            return Some(Value::String(s[1..][..s.len() - 2].bytes().collect()));
        }

        return None;
    }
}

pub struct VirtualMachine {
    classes: HashMap<String, Rc<ClassFile>>
}

#[derive(Debug)]
pub enum VMError {
    TypeError,
    NameResolutionError,
    ClassLoadError,
    AccessError(&'static str),
    BadClass(&'static str),
    Unsupported(&'static str),
}

impl VirtualMachine {
    pub fn new() -> Self {
        VirtualMachine {
            classes: HashMap::new()
        }
    }

    pub fn resolve_class(&mut self, referent: &str) -> Result<Rc<ClassFile>, VMError> {
        match referent {
            "java/lang/System" => {
                let constants = vec![
                    Constant::Utf8(b"java/lang/System".to_vec()),
                    Constant::Utf8(b"out".to_vec()),
                ];

                let synthetic_class = ClassFile {
                    major_version: 55,
                    minor_version: 0,
                    constant_pool: constants,
                    access_flags: AccessFlags { flags: 0x0001 },
                    this_class: ConstantIdx::new(1).unwrap(),
                    super_class: None,
                    interfaces: Vec::new(),
                    fields: vec![
                        FieldInfo {
                            is_static: true,
                            name_index: ConstantIdx::new(2).unwrap(),
                        },
                    ],
                    methods: vec![],
                    attributes: vec![],
                    native_methods: HashMap::new(),
                };

                Ok(Rc::new(synthetic_class))
            }
            "java/io/PrintStream" => {
                let constants = vec![
                    Constant::Utf8(b"java/io/PrintStream".to_vec()),
                    Constant::Utf8(b"println".to_vec()),
                    Constant::Utf8(b"Ljava/io/PrintStream;".to_vec()),
                ];

                let mut native_methods: HashMap<String, fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>> = HashMap::new();
                native_methods.insert("println".to_string(), system_out_println);

                let synthetic_class = ClassFile {
                    major_version: 55,
                    minor_version: 0,
                    constant_pool: constants,
                    access_flags: AccessFlags { flags: 0x0001 },
                    this_class: ConstantIdx::new(1).unwrap(),
                    super_class: None,
                    interfaces: Vec::new(),
                    fields: vec![],
                    methods: vec![
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(2).unwrap(),
                            descriptor_index: ConstantIdx::new(3).unwrap(),
                            attributes: Vec::new(),
                        },
                    ],
                    attributes: vec![],
                    native_methods,
                };

                Ok(Rc::new(synthetic_class))

            },
            class_name => {
                println!("Looking up class {}", class_name);
                match self.classes.get(class_name) {
                    Some(class_ref) => Ok(Rc::clone(class_ref)),
                    None => Err(VMError::BadClass("unknown class, cannot dynamically "))
                }
            }
        }
    }

    pub fn register(&mut self, class_name: String, class_file: ClassFile) -> Result<Rc<ClassFile>, VMError> {
        println!("Registering class {}", class_name);;
        let rc = Rc::new(class_file);
        self.classes.insert(class_name, Rc::clone(&rc));
        Ok(rc)
    }

    pub fn get_method(&self, class_ref: &Rc<ClassFile>, method: &str) -> Result<Rc<MethodHandle>, VMError> {
        class_ref.get_method(method).map_err(|_| VMError::NameResolutionError)
    }

    pub fn execute(&mut self, method: Rc<MethodHandle>, class_ref: &Rc<ClassFile>, args: Vec<Rc<Value>>) -> Result<Option<Value>, VMError> {
        if !method.access().is_static() {
            return Err(VMError::AccessError("attempted to call an instance method without an instance"));
        }

        if !method.access().is_public() {
            return Err(VMError::AccessError("attempted to initiate VM with non-public function"))
        }

        let code = method.body().ok_or(VMError::AccessError("attempted to initiate VM with function that has no body"))?;

        // TODO: verify arguments? verify that `method` does not take arguments??

        let mut state = VMState::new(code, Rc::clone(class_ref), args);
        self.interpret(&mut state)
    }

    fn interpret(&mut self, state: &mut VMState) -> Result<Option<Value>, VMError> {
        // magic incantation to awaken the machine
        println!("zoom zoom");

        while let Some(instruction) = state.next_instruction() {
//            println!("Executing {:?}", instruction);
            let enc = &*state.current_frame().enclosing_class;
            println!("Executing {}", instruction.display(enc));
            state.execute(&instruction, self)?;
//            println!("Complete!");
        }

        Ok(None)
//        Ok(state.return_value())
    }
}

fn interpreted_method_call(state: &mut VMState, _vm: &mut VirtualMachine, method: Rc<MethodHandle>, method_class: Rc<ClassFile>, _method_type: &str) -> Result<(), VMError> {
    // TODO: parse out arguments from method type, check against available operands, do the call
    //
    // today: [...], do the call

    state.enter(method.body().expect("method has a body"), method_class, vec![]);
    Ok(())
}

fn system_out_println(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state.current_frame_mut().operand_stack.pop().expect("argument available");
    let _receiver = state.current_frame_mut().operand_stack.pop().expect("argument available");
    if let Value::String(data) = &*argument {
        if let Ok(string) = std::str::from_utf8(data) {
            println!("{}", string);
        } else {
            panic!("executing System.out.println(\"{:?}\")", data);
        }
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
