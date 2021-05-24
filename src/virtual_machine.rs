use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

use crate::class_file::validated::Instruction;
use crate::class_file::unvalidated::AccessFlags;
use crate::class_file::unvalidated::{ClassFile as UnvalidatedClassFile};
use crate::class_file::validated::ClassFile;
use crate::class_file::validated::ClassFileRef;
use crate::class_file::unvalidated::{Constant as UnvalidatedConstant};
use crate::class_file::validated::Constant;
use crate::class_file::unvalidated::ConstantIdx;
use crate::class_file::unvalidated::FieldAccessFlags;
use crate::class_file::unvalidated::FieldInfo;
use crate::class_file::unvalidated::MethodAccessFlags;
use crate::class_file::validated::MethodBody;
use crate::class_file::validated::MethodHandle;
use crate::class_file::unvalidated::MethodInfo;

#[allow(dead_code)]
struct CallFrame {
    offset: u32,
    arguments: Vec<Value>,
    body: Rc<MethodBody>,
    enclosing_class: Rc<ClassFile>,
    operand_stack: Vec<Value>,
}

impl CallFrame {
    pub fn new(
        body: Rc<MethodBody>,
        enclosing_class: Rc<ClassFile>,
        mut arguments: Vec<Value>,
    ) -> Self {
        while arguments.len() < (body.max_locals as usize) {
            arguments.push(Value::Uninitialized);
        }

        CallFrame {
            offset: 0,
            arguments,
            body,
            enclosing_class,
            operand_stack: Vec::new(),
        }
    }
}

pub struct VMState {
    // Attribute is actually a Code (anything else is an error)
    call_stack: Vec<CallFrame>,
    throwing: bool,
}

#[allow(dead_code)]
impl VMState {
    pub fn new(
        code: Rc<MethodBody>,
        method_class: Rc<ClassFile>,
        initial_args: Vec<Value>,
    ) -> Self {
        let mut state = VMState {
            call_stack: Vec::new(),
            throwing: false,
        };
        state
            .call_stack
            .push(CallFrame::new(code, method_class, initial_args));
        state
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
        let mut inst_iter = frame.body.iter_from(frame.offset);
        let inst = inst_iter.next();
        frame.offset = inst_iter.offset as u32;

        inst
    }

    pub fn enter(
        &mut self,
        body: Rc<MethodBody>,
        enclosing_class: Rc<ClassFile>,
        arguments: Vec<Value>,
    ) {
        self.call_stack
            .push(CallFrame::new(body, enclosing_class, arguments));
    }

    pub fn leave(&mut self) {
        self.call_stack.pop().expect("stack is non-empty");
    }

    pub fn throw(&mut self) {
        self.throwing = true;
        // do *not* pop the top of the call stack yet - this is how we know what method to start
        // looking for exception handlers in.
        // self.call_stack.pop().expect("stack is non-empty");
    }

    fn interpret_iload(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => match argument {
                Value::Integer(v) => Value::Integer(*v),
                Value::Uninitialized => {
                    Value::default_of("I")
                }
                _ => {
                    return Err(VMError::BadClass("iload but not integer"));
                }
            },
            None => {
                return Err(VMError::BadClass("iload but insufficient arguments"));
            }
        };

        frame_mut.operand_stack.push(operand);
        Ok(None)
    }

    fn interpret_istore(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_lload(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => match argument {
                Value::Long(v) => Value::Long(*v),
                Value::Uninitialized => {
                    Value::default_of("J")
                }
                _ => {
                    return Err(VMError::BadClass("lload but not long"));
                }
            },
            None => {
                return Err(VMError::BadClass("lload but insufficient arguments"));
            }
        };

        frame_mut.operand_stack.push(operand);
        Ok(None)
    }

    fn interpret_lstore(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_fload(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => match argument {
                Value::Float(v) => Value::Float(*v),
                Value::Uninitialized => {
                    Value::default_of("F")
                }
                _ => {
                    return Err(VMError::BadClass("fload but not float"));
                }
            },
            None => {
                return Err(VMError::BadClass("fload but insufficient arguments"));
            }
        };

        frame_mut.operand_stack.push(operand);
        Ok(None)
    }

    fn interpret_fstore(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_dload(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => match argument {
                Value::Double(v) => Value::Double(*v),
                Value::Uninitialized => {
                    Value::default_of("D")
                }
                _ => {
                    return Err(VMError::BadClass("dload but not double"));
                }
            },
            None => {
                return Err(VMError::BadClass("dload but insufficient arguments"));
            }
        };

        frame_mut.operand_stack.push(operand);
        Ok(None)
    }

    fn interpret_dstore(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_aload(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            // TODO: type check argument as an object?
            Some(Value::Uninitialized) => {
                Value::Null(String::new())
            }
            Some(argument) => argument.clone(),
            None => {
                return Err(VMError::BadClass("dload but insufficient arguments"));
            }
        };

        frame_mut.operand_stack.push(operand);
        Ok(None)
    }

    fn interpret_astore(&mut self, idx: u16) -> Result<Option<Value>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn execute(
        &mut self,
        instruction: &Instruction,
        vm: &mut VirtualMachine,
    ) -> Result<Option<Value>, VMError> {
//        eprintln!("inst: {}", instruction);
        match instruction {
            Instruction::InvokeVirtual(method_ref) => {
                let target_class = vm.resolve_class(&method_ref.class_name).unwrap();
                // get method by name `method_name`
                if let Some(native_method) = target_class.native_methods.get(&format!("{}{}", &method_ref.name, &method_ref.desc)) {
                    native_method(self, vm)?;
                    Ok(None)
                } else {
                    let method = target_class
                        .get_method(&method_ref.name, &method_ref.desc)
                        .expect("method exists");
                    if method.access().is_native() {
                        panic!("attempted to call native method with no implementation: {} - note, JNI resolution is not yet supported.", &method_ref.name);
                    } else {
                        interpreted_method_call(self, vm, method, target_class, &method_ref.desc, true)?;
                    }
                    Ok(None)
                }
            }
            Instruction::InvokeStatic(method_ref) => {
                let target_class = vm.resolve_class(&method_ref.class_name).unwrap();
                // get method by name `method_name`
                if let Some(native_method) = target_class.native_methods.get(&format!("{}{}", &method_ref.name, &method_ref.desc)) {
                    native_method(self, vm)?;
                    Ok(None)
                } else {
                    let method = target_class
                        .get_method(&method_ref.name, &method_ref.desc)
                        .expect("method exists");
                    // get method by name `method_name`
                    if method.access().is_native() {
                        panic!("attempted to call native method with no implementation: {} - note, JNI resolution is not yet supported.", &method_ref.name);
                    } else {
                        interpreted_method_call(self, vm, method, target_class, &method_ref.desc, false)?;
                        Ok(None)
                    }
                }
            }
            Instruction::GetStatic(field_ref) => {
                let target_class = vm.resolve_class(&field_ref.class_name).unwrap();
                let value = vm
                    .get_static_field(&target_class, &field_ref.name, &field_ref.desc)
                    .unwrap();
                self.current_frame_mut().operand_stack.push(value);
                Ok(None)
            }
            Instruction::PutStatic(field_ref) => {
                let target_class = vm.resolve_class(&field_ref.class_name).unwrap();
                let value =
                    if let Some(value) = self.current_frame_mut().operand_stack.pop() {
                        value
                    } else {
                        return Err(VMError::BadClass("putstatic but insufficient arguments"));
                    };

                vm.put_static_field(&target_class, &field_ref.name, &field_ref.desc, value);
                Ok(None)
            }
            Instruction::InvokeSpecial(method_ref) => {
                let target_class = vm.resolve_class(&method_ref.class_name).unwrap();
                let method = target_class
                    .get_method(&method_ref.name, &method_ref.desc)
                    .expect("method exists");
                // get method by name `method_name`
                if let Some(native_method) = target_class.native_methods.get(&format!("{}{}", &method_ref.name, &method_ref.desc)) {
                    native_method(self, vm)?;
                    Ok(None)
                } else {
                    let method = target_class
                        .get_method(&method_ref.name, &method_ref.desc)
                        .expect("method exists");
                    if method.access().is_native() {
                        panic!("attempted to call native method with no implementation: {} - note, JNI resolution is not yet supported.", &method_ref.name);
                    } else {
                        interpreted_method_call(self, vm, method, target_class, &method_ref.desc, true)?;
                    }
                    Ok(None)
                }
            }
            Instruction::GetField(field_ref) => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                let value = match top {
                    Value::Object(fields, inst_class) => {
                        println!("getting the field, inst: {}", inst_class.this_class);
                        vm
                            .get_instance_field(Rc::clone(&inst_class), Rc::clone(&fields), &field_ref.name, &field_ref.desc)
                            .unwrap()
                    }
                    other => { panic!("should be an object, was {:?}, getting {:?}", other, field_ref); }
                };
                self.current_frame_mut().operand_stack.push(value);
                Ok(None)
            }
            Instruction::PutField(field_ref) => {
                let value = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                match top {
                    Value::Object(fields, inst_class) => {
                        println!("putting the field, inst: {}", inst_class.this_class);
                        vm
                            .put_instance_field(Rc::clone(&inst_class), Rc::clone(&fields), &field_ref.name, &field_ref.desc, value);
                    }
                    other => { panic!("should be an object, was {:?}, getting {:?}", other, field_ref); }
                };
                Ok(None)
            }
            Instruction::NewArray(_t) => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                let mut elems = Vec::new();
                if let Value::Integer(size) = top {
                    for _ in 0..size {
                        elems.push(Value::Integer(0));
                    }
                }

                self.current_frame_mut()
                    .operand_stack
                    .push(Value::Array(
                        Rc::new(RefCell::new(elems.into_boxed_slice())),
                    ));
                Ok(None)
            }
            Instruction::ANewArray(_tpe) => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                let mut elems = Vec::new();
                if let Value::Integer(size) = top {
                    for _ in 0..size {
                        elems.push(Value::Null(String::new()));
                    }
                }

                self.current_frame_mut()
                    .operand_stack
                    .push(Value::Array(
                        Rc::new(RefCell::new(elems.into_boxed_slice())),
                    ));
                Ok(None)
            }
            Instruction::New(tpe) => {
                self.current_frame_mut()
                    .operand_stack
                    .push(Value::new_inst(
                        vm.resolve_class(tpe)?,
                    ));
                Ok(None)
            }
            Instruction::BIPush(b) => {
                self.current_frame_mut()
                    .operand_stack
                    .push(Value::Integer(*b as i32));
                Ok(None)
            }
            Instruction::SIPush(s) => {
                self.current_frame_mut()
                    .operand_stack
                    .push(Value::Integer(*s as i32));
                Ok(None)
            }
            Instruction::Dup => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                self.current_frame_mut().operand_stack.push(top.clone());
                self.current_frame_mut().operand_stack.push(top);
                Ok(None)
            }
            Instruction::Dup2 => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let next = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                self.current_frame_mut().operand_stack.push(next.clone());
                self.current_frame_mut().operand_stack.push(top.clone());
                self.current_frame_mut().operand_stack.push(next);
                self.current_frame_mut().operand_stack.push(top);
                Ok(None)
            }
            Instruction::DupX1 => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let next = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                self.current_frame_mut().operand_stack.push(top.clone());
                self.current_frame_mut().operand_stack.push(next);
                self.current_frame_mut().operand_stack.push(top);
                Ok(None)
            }
            Instruction::DupX2 => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let next = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let third = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                self.current_frame_mut().operand_stack.push(top.clone());
                self.current_frame_mut().operand_stack.push(third);
                self.current_frame_mut().operand_stack.push(next);
                self.current_frame_mut().operand_stack.push(top);
                Ok(None)
            }
            Instruction::AALoad => {
                // ok this one is trickier
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    if let Some(value) = elements.borrow().get(index as usize) {
                        self.current_frame_mut()
                            .operand_stack
                            .push(value.clone());
                    } else {
                        panic!("array index out of bounds exceptions!!");
                    }
                } else {
                    panic!("getting element out of non-array");
                }
                Ok(None)
            }
            Instruction::AAStore => {
                // ok this one is trickier
                let value = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (&array, &index)
                {
                    // TODO: homogeneously typed arrays
                    elements.borrow_mut()[*index as usize] = value;
                } else {
                    panic!("storing element into non-array. array={:?}, index={:?}", array, index);
                }
                Ok(None)
            }
            Instruction::BALoad => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    if let Some(value) = elements.borrow().get(index as usize) {
                        self.current_frame_mut()
                            .operand_stack
                            .push(value.clone());
                    } else {
                        panic!("array index out of bounds exceptions!!");
                    }
                } else {
                    panic!("getting element out of non-array");
                }
                Ok(None)
            }
            Instruction::BAStore => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let value = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    elements.borrow_mut()[index as usize] = value;
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::CAStore => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let value = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    elements.borrow_mut()[index as usize] = value;
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::CALoad => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.borrow()[index as usize].clone());
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::DAStore => {
                // ok this one is trickier
                let value = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    elements.borrow_mut()[index as usize] = value;
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::DALoad => {
                // ok this one is trickier
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.borrow()[index as usize].clone());
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::FAStore => {
                // ok this one is trickier
                let value = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    elements.borrow_mut()[index as usize] = value;
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::FALoad => {
                // ok this one is trickier
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.borrow()[index as usize].clone());
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::SAStore => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let value = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    elements.borrow_mut()[index as usize] = value;
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::SALoad => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.borrow()[index as usize].clone());
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::IAStore => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let value = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    elements.borrow_mut()[index as usize] = value;
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::IALoad => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.borrow()[index as usize].clone());
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::LAStore => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let value = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (&array, &index)
                {
                    // TODO: homogeneously typed arrays
                    elements.borrow_mut()[*index as usize] = value;
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::LALoad => {
                // ok this one is trickier
                // TODO: handle longs/doubles properly
                let index = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let array = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let (Value::Array(elements), Value::Integer(index)) =
                    (array, index)
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.borrow()[index as usize].clone());
                } else {
                    panic!("storing element into non-array");
                }
                Ok(None)
            }
            Instruction::ArrayLength => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                if let Value::Array(elems) = top {
                    self.current_frame_mut()
                        .operand_stack
                        .push(Value::Integer(elems.borrow().len() as i32));
                    Ok(None)
                } else {
                    panic!("arraylength but value is not array");
                }
            }
            Instruction::AConstNull => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Null(String::new()));
                Ok(None)
            }
            Instruction::DConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Double(0.0f64));
                Ok(None)
            }
            Instruction::DConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Double(1.0f64));
                Ok(None)
            }
            Instruction::FConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Float(0.0f32));
                Ok(None)
            }
            Instruction::FConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Float(1.0f32));
                Ok(None)
            }
            Instruction::LConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Long(0));
                Ok(None)
            }
            Instruction::LConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Long(1));
                Ok(None)
            }
            Instruction::IConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Integer(0));
                Ok(None)
            }
            Instruction::IConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Integer(1));
                Ok(None)
            }
            Instruction::IConst2 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Integer(2));
                Ok(None)
            }
            Instruction::IConst3 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Integer(3));
                Ok(None)
            }
            Instruction::IConst4 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Integer(4));
                Ok(None)
            }
            Instruction::IConst5 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Integer(5));
                Ok(None)
            }
            Instruction::IConstM1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(Value::Integer(-1));
                Ok(None)
            }
            Instruction::Goto(offset) => {
                let frame_mut = self.current_frame_mut();
                frame_mut.offset = frame_mut.offset.wrapping_add(*offset as i32 as u32 - 3);
                Ok(None)
            }
            Instruction::IfGe(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match value {
                    Value::Integer(v) => {
                        if v >= 0 {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IfGt(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match value {
                    Value::Integer(v) => {
                        if v > 0 {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IfLe(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifle but insufficient arguments"));
                };

                match value {
                    Value::Integer(v) => {
                        if v <= 0 {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("ifle but invalid operand types")),
                }
            }
            Instruction::IfLt(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match value {
                    Value::Integer(v) => {
                        if v < 0 {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IfEq(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifeq but insufficient arguments"));
                };

                match value {
                    Value::Integer(v) => {
                        if v == 0 {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("ifeq but invalid operand types")),
                }
            }
            Instruction::IfNe(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifne but insufficient arguments"));
                };

                match value {
                    Value::Integer(v) => {
                        if v != 0 {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("ifne but invalid operand types")),
                }
            }
            Instruction::IfIcmpLe(offset) => {
                let frame_mut = self.current_frame_mut();
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        if l <= r {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IfIcmpLt(offset) => {
                let frame_mut = self.current_frame_mut();
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        if l < r {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IfIcmpGe(offset) => {
                let frame_mut = self.current_frame_mut();
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        if l >= r {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IfIcmpGt(offset) => {
                let frame_mut = self.current_frame_mut();
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        if l > r {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IfIcmpNe(offset) => {
                let frame_mut = self.current_frame_mut();
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        if l != r {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::LCmp => {
                let right = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");
                let left = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                match (left, right) {
                    (Value::Long(left), Value::Long(right)) => {
                        let left = left as i64;
                        let right = right as i64;
                        if left > right {
                            self.current_frame_mut()
                                .operand_stack
                                .push(Value::Integer(1));
                        } else if left == right {
                            self.current_frame_mut()
                                .operand_stack
                                .push(Value::Integer(0));
                        } else {
                            self.current_frame_mut()
                                .operand_stack
                                .push(Value::Integer(-1));
                        }
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("lcmp but invalid operand types")),
                }
            }
            Instruction::IfNonNull(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifnotnull but insufficient arguments"));
                };

                match value {
                    Value::Null(_v) => {
                        Ok(None)
                    }
                    Value::Object(_, _) => {
                        frame_mut.offset += *offset as i32 as u32 - 3;
                        Ok(None)
                    }
                    Value::String(_) => {
                        // TODO: really need to make this an internal String-with-value situation,
                        // but strings are not null....
                        frame_mut.offset += *offset as i32 as u32 - 3;
                        Ok(None)
                    }
                    _other => {
                        Err(VMError::BadClass("ifnotnull but invalid operand types"))
                    }
                }
            }
            Instruction::IInc(idx, constant) => {
                let frame_mut = self.current_frame_mut();
                let argument: Option<&mut Value> = frame_mut.arguments.get_mut(*idx as usize);
                match argument {
                    Some(argument) => match argument {
                        Value::Integer(v) => { *v = v.wrapping_add(*constant as i32); }
                        _ => {
                            return Err(VMError::BadClass("iload but not integer"));
                        }
                    },
                    None => {
                        return Err(VMError::BadClass("iload but insufficient arguments"));
                    }
                };
                Ok(None)
            }
            Instruction::ILoad0 => self.interpret_iload(0),
            Instruction::ILoad1 => self.interpret_iload(1),
            Instruction::ILoad2 => self.interpret_iload(2),
            Instruction::ILoad3 => self.interpret_iload(3),
            Instruction::ILoad(idx) => self.interpret_iload(*idx),
            Instruction::IStore0 => self.interpret_istore(0),
            Instruction::IStore1 => self.interpret_istore(1),
            Instruction::IStore2 => self.interpret_istore(2),
            Instruction::IStore3 => self.interpret_istore(3),
            Instruction::IStore(idx) => self.interpret_istore(*idx),
            Instruction::LLoad0 => self.interpret_lload(0),
            Instruction::LLoad1 => self.interpret_lload(1),
            Instruction::LLoad2 => self.interpret_lload(2),
            Instruction::LLoad3 => self.interpret_lload(3),
            Instruction::LLoad(idx) => self.interpret_lload(*idx),
            Instruction::LStore0 => self.interpret_lstore(0),
            Instruction::LStore1 => self.interpret_lstore(1),
            Instruction::LStore2 => self.interpret_lstore(2),
            Instruction::LStore3 => self.interpret_lstore(3),
            Instruction::LStore(idx) => self.interpret_lstore(*idx),
            Instruction::FLoad0 => self.interpret_fload(0),
            Instruction::FLoad1 => self.interpret_fload(1),
            Instruction::FLoad2 => self.interpret_fload(2),
            Instruction::FLoad3 => self.interpret_fload(3),
            Instruction::FLoad(idx) => self.interpret_fload(*idx),
            Instruction::FStore0 => self.interpret_fstore(0),
            Instruction::FStore1 => self.interpret_fstore(1),
            Instruction::FStore2 => self.interpret_fstore(2),
            Instruction::FStore3 => self.interpret_fstore(3),
            Instruction::FStore(idx) => self.interpret_fstore(*idx),
            Instruction::DLoad0 => self.interpret_dload(0),
            Instruction::DLoad1 => self.interpret_dload(1),
            Instruction::DLoad2 => self.interpret_dload(2),
            Instruction::DLoad3 => self.interpret_dload(3),
            Instruction::DLoad(idx) => self.interpret_dload(*idx),
            Instruction::DStore0 => self.interpret_dstore(0),
            Instruction::DStore1 => self.interpret_dstore(1),
            Instruction::DStore2 => self.interpret_dstore(2),
            Instruction::DStore3 => self.interpret_dstore(3),
            Instruction::DStore(idx) => self.interpret_dstore(*idx),
            Instruction::ALoad0 => self.interpret_aload(0),
            Instruction::ALoad1 => self.interpret_aload(1),
            Instruction::ALoad2 => self.interpret_aload(2),
            Instruction::ALoad3 => self.interpret_aload(3),
            Instruction::ALoad(idx) => self.interpret_aload(*idx),
            Instruction::AStore0 => self.interpret_astore(0),
            Instruction::AStore1 => self.interpret_astore(1),
            Instruction::AStore2 => self.interpret_astore(2),
            Instruction::AStore3 => self.interpret_astore(3),
            Instruction::AStore(idx) => self.interpret_astore(*idx),
            Instruction::LUshr => {
                let frame_mut = self.current_frame_mut();
                let amount = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match (value, amount) {
                    (Value::Long(value), Value::Integer(amount)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Long(((value as u64) >> (amount & 0x3f)) as i64));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::LNeg => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("lneg but insufficient arguments"));
                };

                match value {
                    Value::Long(l) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Long(-l));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("lneg but invalid operand types")),
                }
            }
            Instruction::LAnd => {
                let frame_mut = self.current_frame_mut();
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iand but insufficient arguments"));
                };
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iand but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Long(l), Value::Long(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Long(l & r));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iand but invalid operand types")),
                }
            }
            Instruction::LOr => {
                let frame_mut = self.current_frame_mut();
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ior but insufficient arguments"));
                };
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ior but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Long(l), Value::Long(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Long(l | r));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("ior but invalid operand types")),
                }
            }
            Instruction::LShl => {
                let frame_mut = self.current_frame_mut();
                let amount = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match (value, amount) {
                    (Value::Long(value), Value::Integer(amount)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Long(((value as u64) << (amount & 0x3f)) as i64));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IAdd => {
                let frame_mut = self.current_frame_mut();
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(l.wrapping_add(r)));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::LAdd => {
                let frame_mut = self.current_frame_mut();
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ladd but insufficient arguments"));
                };
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ladd but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Long(l), Value::Long(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Long(l.wrapping_add(r)));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("ladd but invalid operand types")),
                }
            }
            Instruction::ISub => {
                let frame_mut = self.current_frame_mut();
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(l.wrapping_sub(r)));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::INeg => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ineg but insufficient arguments"));
                };

                match value {
                    Value::Integer(l) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(-l));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("ineg but invalid operand types")),
                }
            }
            Instruction::IAnd => {
                let frame_mut = self.current_frame_mut();
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iand but insufficient arguments"));
                };
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iand but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(l & r));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iand but invalid operand types")),
                }
            }
            Instruction::IOr => {
                let frame_mut = self.current_frame_mut();
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ior but insufficient arguments"));
                };
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ior but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(l | r));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("ior but invalid operand types")),
                }
            }
            Instruction::IShr => {
                let frame_mut = self.current_frame_mut();
                let value2 = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ishr but insufficient arguments"));
                };
                let value1 = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ishr but insufficient arguments"));
                };

                match (value1, value2) {
                    (Value::Integer(v1), Value::Integer(v2)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(v1 >> (v2 & 0x1f)));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("ishr but invalid operand types")),
                }
            }
            Instruction::IShl => {
                let frame_mut = self.current_frame_mut();
                let value2 = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ishl but insufficient arguments"));
                };
                let value1 = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ishl but insufficient arguments"));
                };

                match (value1, value2) {
                    (Value::Integer(v1), Value::Integer(v2)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(v1 << (v2 & 0x1f)));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("ishl but invalid operand types")),
                }
            }
            Instruction::L2I => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match value {
                    Value::Long(l) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(l as i32));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::I2B => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match value {
                    Value::Integer(l) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(l as i8 as i32));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::I2C => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match value {
                    Value::Integer(l) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(l as i16 as u16 as i32));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::I2D => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("i2d but insufficient arguments"));
                };

                match value {
                    Value::Integer(l) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Double(l as f64));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("i2d but invalid operand types")),
                }
            }
            Instruction::I2F => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("i2f but insufficient arguments"));
                };

                match value {
                    Value::Integer(l) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Float(l as f32));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("i2f but invalid operand types")),
                }
            }
            Instruction::I2L => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match value {
                    Value::Integer(l) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Long(l as i64));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::DReturn => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("dreturn but insufficient arguments"));
                };

                match value {
                    Value::Double(_) => {
                        self.leave();
                        Ok(Some(value))
                    }
                    _ => Err(VMError::BadClass("dreturn but invalid operand types")),
                }
            }
            Instruction::IReturn => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ireturn but insufficient arguments"));
                };

                match value {
                    Value::Integer(_) => {
                        self.leave();
                        Ok(Some(value))
                    }
                    _ => Err(VMError::BadClass("ireturn but invalid operand types")),
                }
            }
            Instruction::AReturn => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("areturn but insufficient arguments"));
                };

                match value {
                    Value::Object(_, _) => {
                        self.leave();
                        Ok(Some(value))
                    }
                    _ => Err(VMError::BadClass("areturn but invalid operand types")),
                }
            }
            Instruction::AThrow => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("athrow but insufficient arguments"));
                };

                match value {
                    Value::Object(_, _) => {
                        self.throw();
                        Ok(Some(value))
                    }
                    _ => Err(VMError::BadClass("athrow but invalid operand types")),
                }
            }
            Instruction::Ldc(c) => {
                let value = match &**c {
                    Constant::Integer(i) => Value::Integer(*i as i32),
                    Constant::Float(f) => Value::Float(*f),
                    Constant::String(s) => {
                        Value::String(Rc::new(s.bytes().collect()))
                    }
                    Constant::Class(c) => {
                        class_object_new(vm, &c)
                    }
                    o => {
                        eprintln!("unsupported ldc: {:?}", o);
                        return Err(VMError::Unsupported("unsupported constant type for ldc"));
                    }
                };

                self.current_frame_mut().operand_stack.push(value);
                Ok(None)
            }
            Instruction::LdcW(c) => {
                let value = match &**c {
                    Constant::Integer(i) => Value::Integer(*i as i32),
                    Constant::Float(f) => Value::Float(*f),
                    Constant::String(s) => {
                        Value::String(Rc::new(s.bytes().collect()))
                    }
                    Constant::Class(c) => {
                        class_object_new(vm, &c)
                    }
                    _ => {
                        return Err(VMError::Unsupported("unsupported constant type for ldcw"));
                    }
                };

                self.current_frame_mut().operand_stack.push(value);
                Ok(None)
            }
            Instruction::Ldc2W(c) => {
                let value = match &**c {
                    Constant::Long(l) => Value::Long(*l as i64),
                    Constant::Double(d) => Value::Double(*d),
                    _ => {
                        return Err(VMError::Unsupported("unsupported constant type for ldc2w"));
                    }
                };

                self.current_frame_mut().operand_stack.push(value);
                Ok(None)
            }
            Instruction::Return => {
                self.leave();
                Ok(None)
            }
            Instruction::Nop => { Ok(None) }
            other => {
                todo!("implement {}", other);
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
    Array(Rc<RefCell<Box<[Value]>>>),
    String(Rc<Vec<u8>>),
    Object(Rc<RefCell<HashMap<String, Value>>>, Rc<ClassFile>),
    Null(String), // Null, of type `String`
    Uninitialized,
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Integer(v) => Value::Integer(*v),
            Value::Long(v) => Value::Long(*v),
            Value::Float(v) => Value::Float(*v),
            Value::Double(v) => Value::Double(*v),
            Value::Array(v) => Value::Array(Rc::clone(v)),
            Value::String(v) => Value::String(Rc::clone(v)),
            Value::Object(v1, v2) => Value::Object(Rc::clone(v1), Rc::clone(v2)),
            Value::Null(v) => Value::Null(v.clone()),
            Value::Uninitialized => Value::Uninitialized,
        }
    }
}

impl Value {
    pub fn new_inst(class_file: Rc<ClassFile>) -> Value {
        let mut fields = HashMap::new();
        // TODO: respect type and access flags of fields
        for field in class_file.fields.iter() {
            fields.insert(
                field.name.clone(),
                Value::default_of(
                    &field.desc,
                ),
            );
        }
        Value::Object(Rc::new(RefCell::new(fields)), class_file)
    }

    pub fn default_of(s: &str) -> Value {
        match s {
            "J" => Value::Long(0),
            "B" | "C" | "S" | "Z" | "I" => Value::Integer(0),
            "F" => Value::Float(0.0),
            "D" => Value::Double(0.0),
            // Lasdf;   reference type
            // [        array
            // [[Lasdf; asdf[][]
            other => Value::Null(other.to_string()),
        }
    }

    pub fn parse_from(s: &str) -> Option<Value> {
        if s == "null" {
            return Some(Value::Null("Object".to_string()));
        }

        if let Ok(v) = i64::from_str(s) {
            return Some(Value::Integer(v as i32));
        }

        if let Ok(v) = f64::from_str(s) {
            return Some(Value::Double(v));
        }

        if s.len() >= 2 && s.starts_with("\"") && s.ends_with("\"") {
            return Some(Value::String(Rc::new(s[1..][..s.len() - 2].bytes().collect())));
        }

        return None;
    }
}

pub(crate) struct ValueRef(Value);

impl ValueRef {
    pub fn of(reference: &Value) -> Self {
        ValueRef(reference.clone())
    }
}

impl Hash for ValueRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Value::Integer(v) => v.hash(state),
            Value::Long(v) => v.hash(state),
            Value::Float(v) => v.to_bits().hash(state),
            Value::Double(v) => v.to_bits().hash(state),
            Value::Array(v) => {
                unsafe {
                    let ptr = Rc::into_raw(Rc::clone(v));
                    ptr.hash(state);
                    Rc::from_raw(ptr);
                }
            },
            Value::String(v) => {
                unsafe {
                    let ptr = Rc::into_raw(Rc::clone(v));
                    ptr.hash(state);
                    Rc::from_raw(ptr);
                }
            },
            Value::Object(v1, v2) => {
                unsafe {
                    let ptr = Rc::into_raw(Rc::clone(v1));
                    ptr.hash(state);
                    Rc::from_raw(ptr);
                    let ptr = Rc::into_raw(Rc::clone(v2));
                    ptr.hash(state);
                    Rc::from_raw(ptr);
                }
            }
            Value::Null(v) => v.hash(state),
            Value::Uninitialized => 4.hash(state),
        }
    }
}

impl Eq for ValueRef {}

impl PartialEq for ValueRef {
    fn eq(&self, other: &ValueRef) -> bool {
        match (&self.0, &other.0) {
            (Value::Integer(v1), Value::Integer(v2)) => { v1 == v2 },
            (Value::Long(v1), Value::Long(v2)) => { v1 == v2 },
            (Value::Float(v1), Value::Float(v2)) => { v1 == v2 },
            (Value::Double(v1), Value::Double(v2)) => { v1 == v2 },
            (Value::Array(v1), Value::Array(v2)) => { Rc::ptr_eq(v1, v2) },
            (Value::Object(v1, _), Value::Object(v2, _)) => { Rc::ptr_eq(v1, v2) },
            (Value::String(v1), Value::String(v2)) => { Rc::ptr_eq(v1, v2) },
            (Value::Null(v1), Value::Null(v2)) => { v1 == v2 },
            (Value::Uninitialized, _) => false,
            (_, Value::Uninitialized) => false,
            _ => false,
        }
    }
}

#[allow(dead_code)]
enum NativeObject {
    StringBuilder(Vec<u16>),
    Unknown,
}

pub struct VirtualMachine {
    classes: HashMap<String, Rc<ClassFile>>,
    static_instances: HashMap<ClassFileRef, HashMap<String, Value>>,
    native_instances: HashMap<ValueRef, RefCell<NativeObject>>,
    classpath: Vec<PathBuf>,
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
    pub fn new(initial_classpath: Vec<PathBuf>) -> Self {
        VirtualMachine {
            classes: HashMap::new(),
            static_instances: HashMap::new(),
            native_instances: HashMap::new(),
            classpath: initial_classpath,
        }
    }

    fn has_instance_field(
        &mut self,
        instance_class: &Rc<ClassFile>,
        name: &str,
    ) -> bool {
//        eprintln!("checking class {} for {}", instance_class.this_class, name);
        if instance_class.has_instance_field(name) {
            return true;
        } else if let Some(sup) = instance_class.super_class.as_ref() {
//            eprintln!("checking superclass {} for {}", sup, name);
            let sup = self.resolve_class(sup).unwrap();
            return self.has_instance_field(&sup, name);
        } else {
            return false;
        }
    }

    fn is_exception(
        &mut self,
        cls: &Rc<ClassFile>,
        name: &str,
    ) -> bool {
        if cls.this_class == name {
            return true;
        } else if let Some(sup) = cls.super_class.as_ref() {
            let sup = self.resolve_class(sup).unwrap();
            return self.is_exception(&sup, name);
        } else {
            return false;
        }
    }

    fn get_instance_field(
        &mut self,
        instance_class: Rc<ClassFile>,
        fields: Rc<RefCell<HashMap<String, Value>>>,
        name: &str,
        ty: &str,
    ) -> Option<Value> {
        if self.has_instance_field(&instance_class, name) {
            Some(fields.borrow_mut().entry(name.to_string()).or_insert_with(
                || Value::default_of(ty)
            ).clone())
        } else {
            None
        }
    }

    fn put_instance_field(
        &mut self,
        instance_class: Rc<ClassFile>,
        fields: Rc<RefCell<HashMap<String, Value>>>,
        name: &str,
        ty: &str,
        value: Value,
    ) {
        if self.has_instance_field(&instance_class, name) {
            fields.borrow_mut().insert(name.to_owned(), value);
        } else {
            panic!("no field {} on {}", name, ty);
        }
    }

    fn get_static_field(
        &mut self,
        class_ref: &Rc<ClassFile>,
        name: &str,
        ty: &str,
    ) -> Option<Value> {
        let fields = self
            .static_instances
            .entry(ClassFileRef::of(class_ref))
            .or_insert_with(|| HashMap::new());
        if class_ref.has_static_field(name) {
            Some(fields.entry(name.to_string()).or_insert_with(
                || Value::default_of(ty)
            ).clone())
        } else {
            None
        }
    }

    fn put_static_field(
        &mut self,
        class_ref: &Rc<ClassFile>,
        name: &str,
        ty: &str,
        value: Value,
    ) {
        let fields = self
            .static_instances
            .entry(ClassFileRef::of(class_ref))
            .or_insert_with(|| HashMap::new());
        if class_ref.has_static_field(name) {
            fields.insert(name.to_owned(), value);
        } else {
            panic!("no field {} on {}", name, ty);
        }
    }

    pub fn resolve_class(&mut self, referent: &str) -> Result<Rc<ClassFile>, VMError> {
        if let Some(class_file) = self.classes.get(referent) {
            return Ok(Rc::clone(class_file));
        }

        let new_class = match referent {
            "java/lang/Class" => {
                let constants = vec![
                    UnvalidatedConstant::Utf8(b"java/lang/Class".to_vec()),
                    UnvalidatedConstant::Utf8(b"<init>".to_vec()),
                    UnvalidatedConstant::Utf8(b"()V".to_vec()),
                    UnvalidatedConstant::Class(ConstantIdx::new(1).unwrap()),
                    UnvalidatedConstant::Utf8(b"desiredAssertionStatus".to_vec()),
                    UnvalidatedConstant::Utf8(b"()Z".to_vec()),
                    UnvalidatedConstant::Utf8(b"getPrimitiveClass".to_vec()),
                    UnvalidatedConstant::Utf8(b"(Ljava/lang/String;)Ljava/lang/Class;".to_vec()),
                ];

                let mut native_methods: HashMap<
                    String,
                    fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>,
                > = HashMap::new();
                native_methods.insert("<init>()V".to_string(), object_init);
                native_methods.insert("desiredAssertionStatus()Z".to_string(), class_desired_assertion_status);
                native_methods.insert("getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;".to_string(), class_get_primitive_class);

                let synthetic_class = ClassFile::validate(&UnvalidatedClassFile {
                    major_version: 55,
                    minor_version: 0,
                    constant_pool: constants,
                    access_flags: AccessFlags { flags: 0x0001 },
                    this_class: ConstantIdx::new(4).unwrap(),
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
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(5).unwrap(),
                            descriptor_index: ConstantIdx::new(6).unwrap(),
                            attributes: Vec::new(),
                        },
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(7).unwrap(),
                            descriptor_index: ConstantIdx::new(8).unwrap(),
                            attributes: Vec::new(),
                        },
                    ],
                    attributes: vec![],
                    native_methods,
                }).unwrap();

                synthetic_class
            }
            "java/lang/Throwable" => {
                let constants = vec![
                    UnvalidatedConstant::Utf8(b"java/lang/Throwable".to_vec()),
                    UnvalidatedConstant::Utf8(b"<init>".to_vec()),
                    UnvalidatedConstant::Utf8(b"()V".to_vec()),
                    UnvalidatedConstant::Class(ConstantIdx::new(1).unwrap()),
                    UnvalidatedConstant::Utf8(b"(Ljava/lang/String;)V".to_vec()),
                ];

                let mut native_methods: HashMap<
                    String,
                    fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>,
                > = HashMap::new();
                native_methods.insert("<init>()V".to_string(), object_init);
                native_methods.insert("<init>(Ljava/lang/String;)V".to_string(), throwable_init_string);

                let synthetic_class = ClassFile::validate(&UnvalidatedClassFile {
                    major_version: 55,
                    minor_version: 0,
                    constant_pool: constants,
                    access_flags: AccessFlags { flags: 0x0001 },
                    this_class: ConstantIdx::new(4).unwrap(),
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
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(2).unwrap(),
                            descriptor_index: ConstantIdx::new(5).unwrap(),
                            attributes: Vec::new(),
                        },
                    ],
                    attributes: vec![],
                    native_methods,
                }).unwrap();

                synthetic_class
            }
            "java/lang/Object" => {
                let constants = vec![
                    UnvalidatedConstant::Utf8(b"java/lang/Object".to_vec()),
                    UnvalidatedConstant::Utf8(b"<init>".to_vec()),
                    UnvalidatedConstant::Utf8(b"hashCode".to_vec()),
                    UnvalidatedConstant::Utf8(b"()I".to_vec()),
                    UnvalidatedConstant::Utf8(b"()V".to_vec()),
                    UnvalidatedConstant::Utf8(b"([B)V".to_vec()),
                    UnvalidatedConstant::Utf8(b"[B".to_vec()),
                    UnvalidatedConstant::Utf8(b"value".to_vec()),
                    UnvalidatedConstant::Class(ConstantIdx::new(1).unwrap()),
                ];

                let mut native_methods: HashMap<
                    String,
                    fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>,
                > = HashMap::new();
                native_methods.insert("<init>()V".to_string(), object_init);
                native_methods.insert("hashCode()I".to_string(), object_hashcode);

                let synthetic_class = ClassFile::validate(&UnvalidatedClassFile {
                    major_version: 55,
                    minor_version: 0,
                    constant_pool: constants,
                    access_flags: AccessFlags { flags: 0x0001 },
                    this_class: ConstantIdx::new(9).unwrap(),
                    super_class: None,
                    interfaces: Vec::new(),
                    fields: vec![],
                    methods: vec![
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(2).unwrap(),
                            descriptor_index: ConstantIdx::new(5).unwrap(),
                            attributes: Vec::new(),
                        },
                    ],
                    attributes: vec![],
                    native_methods,
                }).unwrap();

                synthetic_class
            }
            "java/lang/String" => {
                let constants = vec![
                    UnvalidatedConstant::Utf8(b"java/lang/String".to_vec()),
                    UnvalidatedConstant::Utf8(b"<init>".to_vec()),
                    UnvalidatedConstant::Utf8(b"hashCode".to_vec()),
                    UnvalidatedConstant::Utf8(b"()I".to_vec()),
                    UnvalidatedConstant::Utf8(b"(Ljava/lang/String;)".to_vec()),
                    UnvalidatedConstant::Utf8(b"([B)V".to_vec()),
                    UnvalidatedConstant::Utf8(b"[B".to_vec()),
                    UnvalidatedConstant::Utf8(b"value".to_vec()),
                    UnvalidatedConstant::Utf8(b"(Ljava/lang/String;)Ljava/lang/String;".to_vec()),
                    UnvalidatedConstant::Utf8(b"concat".to_vec()),
                    UnvalidatedConstant::Class(ConstantIdx::new(1).unwrap()),
                ];

                let mut native_methods: HashMap<
                    String,
                    fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>,
                > = HashMap::new();
                native_methods.insert("<init>(Ljava/lang/String;)".to_string(), string_init_string);
                native_methods.insert("<init>([B)V".to_string(), string_init_bytearray);
                native_methods.insert("hashCode()I".to_string(), string_hashcode);
                native_methods.insert(
                    "concat(Ljava/lang/String;)Ljava/lang/String;".to_string(),
                    string_concat,
                );

                let synthetic_class = ClassFile::validate(&UnvalidatedClassFile {
                    major_version: 55,
                    minor_version: 0,
                    constant_pool: constants,
                    access_flags: AccessFlags { flags: 0x0001 },
                    this_class: ConstantIdx::new(11).unwrap(),
                    super_class: None,
                    interfaces: Vec::new(),
                    fields: vec![FieldInfo {
                        access_flags: FieldAccessFlags { flags: 0x0001 },
                        name_index: ConstantIdx::new(8).unwrap(),
                        descriptor_index: ConstantIdx::new(7).unwrap(),
                        attributes: Vec::new(),
                    }],
                    methods: vec![
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(2).unwrap(),
                            descriptor_index: ConstantIdx::new(5).unwrap(),
                            attributes: Vec::new(),
                        },
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(2).unwrap(),
                            descriptor_index: ConstantIdx::new(6).unwrap(),
                            attributes: Vec::new(),
                        },
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(3).unwrap(),
                            descriptor_index: ConstantIdx::new(4).unwrap(),
                            attributes: Vec::new(),
                        },
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(10).unwrap(),
                            descriptor_index: ConstantIdx::new(9).unwrap(),
                            attributes: Vec::new(),
                        },
                    ],
                    attributes: vec![],
                    native_methods,
                }).unwrap();

                synthetic_class
            }
            "java/lang/StringBuilder" => {
                let constants = vec![
                    UnvalidatedConstant::Utf8(b"java/lang/StringBuilder".to_vec()),
                    UnvalidatedConstant::Utf8(b"<init>".to_vec()),
                    UnvalidatedConstant::Utf8(b"()V".to_vec()),
                    UnvalidatedConstant::Utf8(b"(Ljava/lang/String;)V".to_vec()),
                    UnvalidatedConstant::Utf8(b"append".to_vec()),
//                    UnvalidatedConstant::Utf8(b"(B)Ljava/lang/String;".to_vec()),
//                    UnvalidatedConstant::Utf8(b"(C)Ljava/lang/String;".to_vec()),
//                    UnvalidatedConstant::Utf8(b"([C)Ljava/lang/String;".to_vec()),
                    UnvalidatedConstant::Utf8(b"(Ljava/lang/String;)Ljava/lang/StringBuilder;".to_vec()),
                    UnvalidatedConstant::Utf8(b"toString".to_vec()),
                    UnvalidatedConstant::Utf8(b"()Ljava/lang/String;".to_vec()),
                ];

                let mut native_methods: HashMap<
                    String,
                    fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>,
                > = HashMap::new();
                native_methods.insert("<init>()V".to_string(), stringbuilder_init);
                native_methods.insert("<init>(Ljava/lang/String;)V".to_string(), string_init_string);
                native_methods.insert("append(Ljava/lang/String;)Ljava/lang/StringBuilder;".to_string(), stringbuilder_append_string);
//                native_methods.insert("append([C)Ljava/lang/StringBuilder".to_string(), stringbuilder_append_chars);
                native_methods.insert("toString()Ljava/lang/String;".to_string(), stringbuilder_tostring);

                let synthetic_class = ClassFile::validate(&UnvalidatedClassFile {
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
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(2).unwrap(),
                            descriptor_index: ConstantIdx::new(4).unwrap(),
                            attributes: Vec::new(),
                        },
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(5).unwrap(),
                            descriptor_index: ConstantIdx::new(6).unwrap(),
                            attributes: Vec::new(),
                        },
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(7).unwrap(),
                            descriptor_index: ConstantIdx::new(8).unwrap(),
                            attributes: Vec::new(),
                        },
                    ],
                    attributes: vec![],
                    native_methods,
                }).unwrap();

                synthetic_class
            }
            "java/lang/System" => {
                let constants = vec![
                    UnvalidatedConstant::Utf8(b"java/lang/System".to_vec()),
                    UnvalidatedConstant::Utf8(b"out".to_vec()),
                    UnvalidatedConstant::Utf8(b"Ljava/io/PrintStream;".to_vec()),
                    UnvalidatedConstant::Utf8(b"exit".to_vec()),
                    UnvalidatedConstant::Utf8(b"(I)V".to_vec()),
                    UnvalidatedConstant::Class(ConstantIdx::new(1).unwrap()),
                ];

                let mut native_methods: HashMap<
                    String,
                    fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>,
                > = HashMap::new();
                native_methods.insert("exit(I)V".to_string(), system_exit);

                let synthetic_class = ClassFile::validate(&UnvalidatedClassFile {
                    major_version: 55,
                    minor_version: 0,
                    constant_pool: constants,
                    access_flags: AccessFlags { flags: 0x0001 },
                    this_class: ConstantIdx::new(6).unwrap(),
                    super_class: None,
                    interfaces: Vec::new(),
                    fields: vec![FieldInfo {
                        access_flags: FieldAccessFlags { flags: 0x0009 },
                        name_index: ConstantIdx::new(2).unwrap(),
                        descriptor_index: ConstantIdx::new(3).unwrap(),
                        attributes: Vec::new(),
                    }],
                    methods: vec![
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(4).unwrap(),
                            descriptor_index: ConstantIdx::new(5).unwrap(),
                            attributes: Vec::new(),
                        },
                    ],
                    attributes: vec![],
                    native_methods,
                }).unwrap();

                synthetic_class
            }
            "java/io/PrintStream" => {
                let constants = vec![
                    UnvalidatedConstant::Utf8(b"java/io/PrintStream".to_vec()),
                    UnvalidatedConstant::Utf8(b"println".to_vec()),
                    UnvalidatedConstant::Utf8(b"(Ljava/lang/String;)V".to_vec()),
                    UnvalidatedConstant::Utf8(b"(I)V".to_vec()),
                    UnvalidatedConstant::Utf8(b"(J)V".to_vec()),
                    UnvalidatedConstant::Utf8(b"(Ljava/lang/Object;)V".to_vec()),
                    UnvalidatedConstant::Class(ConstantIdx::new(1).unwrap()),
                ];

                let mut native_methods: HashMap<
                    String,
                    fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>,
                > = HashMap::new();
                native_methods.insert("println(Ljava/lang/String;)V".to_string(), system_out_println_string);
                native_methods.insert("println(Ljava/lang/Object;)V".to_string(), system_out_println_object);
                native_methods.insert("println(I)V".to_string(), system_out_println_int);
                native_methods.insert("println(J)V".to_string(), system_out_println_long);

                let synthetic_class = ClassFile::validate(&UnvalidatedClassFile {
                    major_version: 55,
                    minor_version: 0,
                    constant_pool: constants,
                    access_flags: AccessFlags { flags: 0x0001 },
                    this_class: ConstantIdx::new(7).unwrap(),
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
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(2).unwrap(),
                            descriptor_index: ConstantIdx::new(4).unwrap(),
                            attributes: Vec::new(),
                        },
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(2).unwrap(),
                            descriptor_index: ConstantIdx::new(5).unwrap(),
                            attributes: Vec::new(),
                        },
                        MethodInfo {
                            access_flags: MethodAccessFlags { flags: 0x0101 },
                            name_index: ConstantIdx::new(2).unwrap(),
                            descriptor_index: ConstantIdx::new(6).unwrap(),
                            attributes: Vec::new(),
                        },
                    ],
                    attributes: vec![],
                    native_methods,
                }).unwrap();

                synthetic_class
            }
            class_name => {
                use std::collections::hash_map::Entry;
                use std::fs::File;
//                println!("Looking up class {}", class_name);
                return match self.classes.entry(class_name.to_string()) {
                    Entry::Occupied(oe) => Ok(Rc::clone(oe.get())),
                    Entry::Vacant(_ve) => {
                        for path in self.classpath.iter() {
                            let possible_class = path.join(PathBuf::from(format!("{}.class", class_name)));
//                            println!("-- checking {}", possible_class.display());
                            if possible_class.exists() {
                                let class_file = ClassFile::validate(
                                    &crate::class_file::unvalidated::read::class_header(
                                        &mut File::open(possible_class).unwrap()
                                    ).unwrap()
                                ).unwrap();
                                let class_file = augment_classfile(class_file);
                                return self.register(referent.to_string(), class_file);
                            }
                        }
                        Err(VMError::BadClass("could not resolve class"))
                    }
                };
            }
        };

        self.register(referent.to_string(), new_class)
    }

    pub fn register(
        &mut self,
        class_name: String,
        class_file: ClassFile,
    ) -> Result<Rc<ClassFile>, VMError> {
        eprintln!("registering class {}", class_name);
        let rc = Rc::new(class_file);
        self.classes.insert(class_name, Rc::clone(&rc));

        if let Some(method) = rc.get_method("<clinit>", "()V") {
            if method.access().is_native() {
                let mut state = VMState::new(
                    Rc::new(MethodBody::native()),
                    Rc::clone(&rc),
                    vec![],
                );
                let native_method = rc.native_methods.get("<clinit>()V").expect("native clinit has native method impl");
                native_method(&mut state, self)?;
            } else {
                let mut state = VMState::new(
                    Rc::clone(method.body.as_ref().expect("clinit has a body")),
                    Rc::clone(&rc),
                    vec![],
                );
                let clinit_res = self
                    .interpret(&mut state)
                    .expect("clinit executes successfully");
                if clinit_res.is_some() {
                    panic!("clinit should not return values");
                }
            }
        } // else no static initializer

        Ok(rc)
    }

    pub fn execute(
        &mut self,
        method: Rc<MethodHandle>,
        class_ref: &Rc<ClassFile>,
        args: Vec<Value>,
    ) -> Result<Option<Value>, VMError> {
        if !method.access().is_static() {
            return Err(VMError::AccessError(
                "attempted to call an instance method without an instance",
            ));
        }

        if !method.access().is_public() {
            return Err(VMError::AccessError(
                "attempted to initiate VM with non-public function",
            ));
        }

        let code = method.body.as_ref().ok_or(VMError::AccessError(
            "attempted to initiate VM with function that has no body",
        ))?;

        // TODO: verify arguments? verify that `method` does not take arguments??

        let mut state = VMState::new(Rc::clone(code), Rc::clone(class_ref), args);
        self.interpret(&mut state)
    }

    fn interpret(&mut self, state: &mut VMState) -> Result<Option<Value>, VMError> {
        // magic incantation to awaken the machine
        println!("zoom zoom");

        while let Some(instruction) = state.next_instruction() {
            //            println!("Executing {:?}", instruction);
            //            let enc = &*state.current_frame().enclosing_class;
            //            println!("Executing {}", instruction);
            if let Some(value) = state.execute(&instruction, self)? {
                if state.throwing {
                    let mut handler_pc = None;
                    while state.throwing && state.call_stack.len() > 0 {
                        for exception_record in state.current_frame().body.exception_info.iter() {
                            if exception_record.contains(state.current_frame().offset) {
                                if let Value::Object(_, cls) = &value {
                                    if self.is_exception(cls, exception_record.catch_type.as_str()) {
                                        handler_pc = Some(exception_record.handler_pc as u32);
                                        break;
                                    }
                                } else {
                                    panic!("threw a non-Object: {:?}", value);
                                }
                            }
                        }

                        if handler_pc.is_some() {
                            state.throwing = false;
                        } else {
                            // no catch/finally covering this address in this function, pop the
                            // record and try again up the stack.
                            state.leave();
                        }
                    }

                    if let Some(pc) = handler_pc {
                        state.current_frame_mut().offset = pc;
                        state.current_frame_mut().operand_stack.push(value);
                    } else {
                        // if there is no handler and we stopped walking the call stack, we must
                        // have reached the end of the stack.
                        assert!(state.call_stack.len() == 0);
                        if let Value::Object(fields, cls) = &value {
                            if let Some(Value::String(msg)) = fields.borrow().get("message") {
                                eprintln!("unhandled {}: {}", cls.this_class.as_str(), String::from_utf8_lossy(msg));
                                std::process::exit(1);
                            }
                        }
                        eprintln!("unhandled exception: {:?}", value);
                        std::process::exit(1);
                    }
                } else {
                    // TODO: type check the return value
                    if state.call_stack.len() == 0 {
                        // this was a return from the first call frame, so it's the result of the
                        // entire interpreter execution
                        return Ok(Some(value));
                    } else {
                        // this was an inner return, so push it onto the caller's operand stack
                        state.current_frame_mut().operand_stack.push(value);
                    }
                }
            }
            //            println!("Complete!");
        }

        Ok(None)
        //        Ok(state.return_value())
    }
}

fn interpreted_method_call(
    state: &mut VMState,
    _vm: &mut VirtualMachine,
    method: Rc<MethodHandle>,
    method_class: Rc<ClassFile>,
    method_type: &str,
    is_virtual: bool,
) -> Result<(), VMError> {
    // TODO: parse out arguments from method type, check against available operands, do the call
    //
    // today: [...], do the call
    
    struct Arg {}

    #[allow(unused_assignments)]
    fn parse_signature_string(signature: &str) -> Option<(Vec<Arg>, Option<Arg>)> {
        let mut reading_type = false;
        let mut _reading_array = false;
        let mut reading_return = false;
        let mut args = Vec::new();
        for b in signature.bytes() {
            match b {
                b'(' => {
                    // one day assert that this is the first character
                }
                b')' => {
                    reading_return = true;
                }
                b'V' => {
                    if reading_type {
                        continue;
                    }
                    if !reading_return {
                        return None;
                    } else {
                        return Some((args, None));
                    }
                }
                b'Z' | // boolean
                b'B' | // byte
                b'C' | // char
                b'S' | // short
                b'I' | // int
                b'J' | // long
                b'F' | // float
                b'D' => { // double
                    if reading_type {
                        continue;
                    }
                    if reading_return {
                        return Some((args, Some(Arg {})));
                    } else {
                        args.push(Arg {});
                    }
                }
                // not valid inside a type name
                b'[' => {
                    _reading_array = true;
                }
                b'L' => {
                    if !reading_type {
                        reading_type = true;
                    }
                }
                b';' => {
                    if reading_type {
                        if reading_return {
                            return Some((args, Some(Arg {})));
                        } else {
                            args.push(Arg {});
                            reading_type = false;
                        }
                    }
                }
                _ => {
                    if reading_type {
                        /* do nothing */
                    } else {
                        panic!("invalid type string: {}", signature);
                    }
                }
            }
        }
        panic!("signature strings include return value type (even if it's just [V]oid)");
    }

    // TODO typecheck ret and the returned object type
    let (args, _ret) = parse_signature_string(method_type).expect("signature string is valid (not like there's much validation right now, come on!!");

    let frame = state.current_frame_mut();

    let mut real_args = std::collections::VecDeque::new();
    for _arg in args {
        real_args.push_front(frame.operand_stack.pop().expect("argument is present"));
    }
    if is_virtual {
        real_args.push_front(frame.operand_stack.pop().expect("argument is present"));
    }

    state.enter(
        Rc::clone(method.body.as_ref().expect("method has a body")),
        method_class,
        real_args.drain(..).collect(),
    );
    Ok(())
}

fn system_out_println_string(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::String(data) = argument {
        if let Ok(string) = std::str::from_utf8(data.as_slice()) {
            println!("{}", string);
        } else {
            panic!("executing System.out.println(\"{:?}\")", data);
        }
    } else if let Value::Object(fields, _) = argument {
        if let Value::Array(elements) = &fields.borrow()["value"] {
            for el in elements.borrow().iter() {
                if let Value::Integer(v) = el {
                    print!("{}", *v as u8 as char);
                } else {
                    panic!("string contains non-byte element")
                }
            }
            print!("\n");
        } else {
            panic!("string does not contain value");
        }
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

fn system_out_println_object(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::Object(_fields, _cls) = argument {
//        println!("{}: {:?}", cls.this_class, fields);
        println!("[object Object]");
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

fn system_out_println_int(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::Integer(v) = argument {
        println!("{}", v);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

fn system_out_println_long(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::Long(v) = argument {
        println!("{}", v);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

// "<init>()V"
fn stringbuilder_init(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let data = RefCell::new(NativeObject::StringBuilder(Vec::new()));

    vm.native_instances.insert(ValueRef::of(&receiver), data);
    Ok(())
}

// "append(Ljava/lang/String;);Ljava/lang/StringBuilder"
fn stringbuilder_append_string(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
    let appendee = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let data = vm.native_instances.get(&ValueRef::of(&receiver)).expect("stringbuilder receiver has associated native data");

    if let NativeObject::StringBuilder(data) = &mut *data.borrow_mut() {
        if let Value::String(str_data) = appendee {
            // do thing
            for el in str_data.iter() {
                data.push(*el as u16);
            }
        } else if let Value::Object(fields, _) = appendee {
            // do thing
            if let Value::Array(str_data) = &fields.borrow()["value"] {
                let str_data = str_data.borrow();
                for el in str_data.iter() {
                    if let Value::Integer(i) = el {
                        data.push(*i as u16);
                    }
                }
            } else {
                panic!("appendee of stringbuilder is a non-string object");
            }
        } else {
            panic!("appendee of stringbuilder append is not a string");
        }
    } else {
        panic!("native object corresponding to stringbuilder receiver is not stringbuilder data");
    }

    state.current_frame_mut().operand_stack.push(receiver);
    Ok(())
}

// "toString()Ljava/lang/String"
fn stringbuilder_tostring(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
    // really just consume the argument
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let data = vm.native_instances.get(&ValueRef::of(&receiver)).expect("stringbuilder receiver has associated native data");

    let mut str_data: Vec<Value> = Vec::new();

    if let NativeObject::StringBuilder(data) = &*data.borrow() {
        for el in data.iter() {
            str_data.push(Value::Integer(*el as i32));
        }
    } else {
        panic!("native object corresponding to stringbuilder receiver is not stringbuilder data");
    }

    // now make the string to return...
    let mut string_fields = HashMap::new();
    string_fields.insert("value".to_string(), Value::Array(Rc::new(RefCell::new(str_data.into_boxed_slice()))));
    let s = Value::Object(Rc::new(RefCell::new(string_fields)), vm.resolve_class("java/lang/String")?);

    state.current_frame_mut().operand_stack.push(s);
    Ok(())
}

/*
                native_methods.insert("<init>(Ljava/lang/String;)V".to_string(), string_init_string);
                native_methods.insert("append(Ljava/lang/String;)Ljava/lang/StringBuilder".to_string(), stringbuilder_append_string);
//                native_methods.insert("append([C)Ljava/lang/StringBuilder".to_string(), stringbuilder_append_chars);
                native_methods.insert("toString()Ljava/lang/String".to_string(), stringbuilder_tostring);
*/

// "<init>()V"
fn object_init(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    Ok(())
}
// "hashCode()I"
fn object_hashcode(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    state
        .current_frame_mut()
        .operand_stack
        .push(Value::Integer(0));
    Ok(())
}

// "<init>(Ljava/lang/String;)V"
fn string_init_string(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let (Value::Object(argument, _), Value::Object(receiver, _)) =
        (&argument, &receiver)
    {
        let new_value = argument.borrow()["value"].clone();
        receiver.borrow_mut().insert("value".to_string(), new_value);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
fn throwable_init_string(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let (Value::String(_), Value::Object(receiver, _)) =
        (&argument, &receiver)
    {
        receiver.borrow_mut().insert("message".to_string(), argument);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "<init>([B)"
fn string_init_bytearray(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let (Value::Array(new_elems), Value::Object(fields, _)) =
        (&argument, &receiver)
    {
        let mut str_elems = Vec::new();
        for el in new_elems.borrow().iter() {
            if let Value::Integer(i) = el {
                if (*i as u8) < 128 {
                    str_elems.push(Value::Integer(*i));
                } else {
                    str_elems.push(Value::Integer(0xfffd));
                }
            } else {
                panic!("bad string");
            }
        }
        fields.borrow_mut().insert(
            "value".to_string(),
            Value::Array(Rc::new(RefCell::new(str_elems.into_boxed_slice()))),
        );
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "hashCode()I"
fn string_hashcode(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::String(data) = receiver {
        let mut hashcode: i32 = 0;
        for c in data.iter().cloned() {
            // value is actually a char array
            hashcode = hashcode.wrapping_mul(31).wrapping_add(c as u16 as i32);
        }
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Integer(hashcode));
    } else if let Value::Object(fields, _) = receiver {
        let mut hashcode: i32 = 0;
        if let Value::Array(elems) = &fields.borrow()["value"] {
            for c in elems.borrow().iter() {
                if let Value::Integer(v) = c {
                    // value is actually a char array
                    hashcode = hashcode.wrapping_mul(31).wrapping_add(*v as u16 as i32);
                } else {
                    panic!("string contains non-byte element");
                }
            }
            state
                .current_frame_mut()
                .operand_stack
                .push(Value::Integer(hashcode));
        } else {
            panic!("string does not have a value?");
        }
    } else {
        panic!("type error, expected string, got {:?}", receiver);
    }
    Ok(())
}
// "concat(Ljava/lang/String;)Ljava/lang/String;"
fn string_concat(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let (Value::String(base), Value::String(ext)) = (&receiver, &argument) {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::String(
                Rc::new(base.iter().cloned().chain(ext.iter().cloned()).collect()),
            ));
    } else {
        panic!("type error, expected string, string, got {:?}", argument);
    }
    Ok(())
}

fn system_exit(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Integer(i) = argument {
        std::process::exit(i);
    } else {
        panic!("attempted to exit with non-int operand");
    }
}

fn class_desired_assertion_status(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    state
        .current_frame_mut()
        .operand_stack
        .push(Value::Integer(0));
    Ok(())
}

fn class_get_primitive_class(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    state
        .current_frame_mut()
        .operand_stack
        .push(Value::Null("some primitive class".to_string()));
    Ok(())
}

fn class_object_new(vm: &mut VirtualMachine, class_name: &str) -> Value {
    let class_class = vm.resolve_class("java/lang/Class").unwrap();
    let fields = Rc::new(RefCell::new(HashMap::new()));
    fields.borrow_mut().insert("class".to_string(), Value::String(Rc::new(class_name.bytes().collect())));
    Value::Object(fields, class_class)
}

fn float_to_raw_int_bits(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Float(f) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Integer(f.to_bits() as i32));
    } else {
        panic!("bad operand type for float_to_raw_int_bits");
    }
    Ok(())
}

fn long_bits_to_double(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Long(l) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Double(f64::from_bits(l as u64)));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn int_bits_to_float(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Integer(i) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Float(f32::from_bits(i as u32)));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn double_to_raw_long_bits(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Double(d) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Long(d.to_bits() as i64));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn double_log(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Double(d) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Double(d.ln()));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn augment_classfile(mut class_file: ClassFile) -> ClassFile {
    match class_file.this_class.as_str() {
        "java/lang/Float" => {
            class_file.native_methods.insert("floatToRawIntBits(F)I".to_string(), float_to_raw_int_bits);
            class_file.native_methods.insert("intBitsToFloat(I)F".to_string(), int_bits_to_float);
        }
        "java/lang/Double" => {
            class_file.native_methods.insert("doubleToRawLongBits(D)J".to_string(), double_to_raw_long_bits);
            class_file.native_methods.insert("longBitsToDouble(J)D".to_string(), long_bits_to_double);
        }
        "java/lang/StrictMath" => {
            class_file.native_methods.insert("log(D)D".to_string(), double_log);
        }
        _ => {}
    }
    class_file
}
