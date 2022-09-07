use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use crate::class_file::validated::Instruction;
use crate::class_file::unvalidated::{ClassFile as UnvalidatedClassFile};
use crate::class_file::validated::ClassFile;
use crate::class_file::validated::ClassFileRef;
use crate::class_file::validated::Constant;
use crate::class_file::validated::MethodBody;
use crate::class_file::validated::MethodHandle;
use crate::class_file::validated::MethodRef;

mod jvm;

static NULL_COUNT: AtomicUsize = AtomicUsize::new(0);
static NEW_COUNT: AtomicUsize = AtomicUsize::new(0);

#[allow(dead_code)]
struct CallFrame<ObjectImpl: JvmObject, ArrayImpl: JvmArray> {
    offset: u32,
    arguments: Vec<Value<ObjectImpl, ArrayImpl>>,
    body: Rc<MethodBody>,
    pub enclosing_class: Rc<ClassFile>,
    pub method_name: String,
    operand_stack: Vec<Value<ObjectImpl, ArrayImpl>>,
}

impl<ObjectImpl: JvmObject, ArrayImpl: JvmArray> CallFrame<ObjectImpl, ArrayImpl> {
    pub fn new(
        body: Rc<MethodBody>,
        enclosing_class: Rc<ClassFile>,
        method_name: String,
        mut arguments: Vec<Value<ObjectImpl, ArrayImpl>>,
    ) -> Self {
        while arguments.len() < (body.max_locals as usize) {
            arguments.push(Value::Uninitialized);
        }

        CallFrame {
            offset: 0,
            arguments,
            body,
            enclosing_class,
            method_name,
            operand_stack: Vec::new(),
        }
    }

    pub fn branch_rel(&mut self, rel: i16) {
        self.offset = self.offset.wrapping_add(rel as i32 as u32);
    }
}

#[derive(Debug, Copy, Clone)]
enum CallKind {
    Virtual,
    Special,
    Static,
}

impl CallKind {
    fn takes_self(&self) -> bool {
        if let CallKind::Static = self {
            false
        } else {
            true
        }
    }
}

pub struct VMState<ObjectImpl: JvmObject, ArrayImpl: JvmArray> {
    // Attribute is actually a Code (anything else is an error)
    call_stack: Vec<CallFrame<ObjectImpl, ArrayImpl>>,
    throwing: bool,
}

#[allow(dead_code)]
impl<ObjectImpl: JvmObject, ArrayImpl: JvmArray> VMState<ObjectImpl, ArrayImpl> {
    pub fn new(
        code: Rc<MethodBody>,
        method_class: Rc<ClassFile>,
        method_name: String,
        initial_args: Vec<Value<ObjectImpl, ArrayImpl>>,
    ) -> Self {
        let mut state = VMState {
            call_stack: Vec::new(),
            throwing: false,
        };
        state
            .call_stack
            .push(CallFrame::new(code, method_class, method_name, initial_args));
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
        method_name: &str,
        arguments: Vec<Value<ObjectImpl, ArrayImpl>>,
    ) {
        self.call_stack
            .push(CallFrame::new(body, enclosing_class, method_name.to_string(), arguments));
    }

    pub fn leave(&mut self) {
        self.call_stack.pop().expect("stack is non-empty , pls check it");
    }

    pub fn throw(&mut self) {
        self.throwing = true;
        // do *not* pop the top of the call stack yet - this is how we know what method to start
        // looking for exception handlers in.
        // self.call_stack.pop().expect("stack is non-empty");
    }

    fn interpret_iload(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value<ObjectImpl, ArrayImpl>> = frame_mut.arguments.get(idx as usize);
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

    fn interpret_istore(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_lload(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value<ObjectImpl, ArrayImpl>> = frame_mut.arguments.get(idx as usize);
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

    fn interpret_lstore(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_fload(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value<ObjectImpl, ArrayImpl>> = frame_mut.arguments.get(idx as usize);
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

    fn interpret_fstore(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_dload(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value<ObjectImpl, ArrayImpl>> = frame_mut.arguments.get(idx as usize);
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

    fn interpret_dstore(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_aload(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Value<ObjectImpl, ArrayImpl>> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            // TODO: type check argument as an object?
            Some(Value::Uninitialized) => {
                NULL_COUNT.fetch_add(1, Ordering::SeqCst);
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

    fn interpret_astore(&mut self, idx: u16) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn call_method(&mut self, vm: &mut VirtualMachine<ObjectImpl, ArrayImpl>, method: &MethodRef, kind: CallKind) {
        let mut current_class = match kind {
            CallKind::Virtual => {
                // virtual calls can be abstract and resolution has to start at the ref being called on
                let (args, _ret) = parse_signature_string(&method.desc).expect("signature is valid");
                let frame = self.current_frame();
                let stack = &frame.operand_stack;
                let receiver = &stack[stack.len() - args.len() - 1];
                match receiver {
                    Value::Object(obj) => {
                        Rc::clone(obj.cls())
                    }
                    Value::Array(_) => {
                        vm.resolve_class("java/lang/Object").unwrap()
                    }
                    Value::String(_) => {
                        vm.resolve_class("java/lang/String").unwrap()
                    }
                    other => {
                        panic!("unexpected receiver {:?}", other);
                    }
                }
            },
            CallKind::Static | CallKind::Special => {
                vm.resolve_class(&method.class_name).unwrap()
            }
        };
        let init_class = Rc::clone(&current_class);
        loop {
            let target_class = Rc::clone(&current_class);
            if let Some(native_method) = target_class.native_methods.get(&format!("{}{}", &method.name, &method.desc)) {
                native_method(self, vm).expect("native method call works");
                break;
            } else if let Some(handle) = target_class.get_method(&method.name, &method.desc) {
                if handle.access().is_native() {
                    panic!("attempted to call native method with no implementation: {} - note, JNI resolution is not yet supported.", &method.name);
                } else if handle.access().is_abstract() {
                    // continue up on the chanin...
                    if target_class.super_class.is_none() {
                        eprintln!("started at {:?}", init_class);
                        panic!("cannot find method {:?}", method);
                    }
                    let super_class = target_class.super_class.as_ref().expect("superclass exists");
                    current_class = vm.resolve_class(super_class).expect("can resolve superclass");
                } else {
                    interpreted_method_call(self, vm, handle, target_class, &method.desc, kind).unwrap();
                    break;
                }
            } else {
                // well if it's not on this class, maybe somewhere up the inheritance chain?
                if target_class.super_class.is_none() {
                    eprintln!("started at {:?}", init_class);
                    panic!("cannot find method {:?}", method);
                }
                let super_class = target_class.super_class.as_ref().expect("superclass exists");
                current_class = vm.resolve_class(super_class).expect("can resolve superclass");
            }
        }
    }

    fn execute(
        &mut self,
        instruction: &Instruction,
        vm: &mut VirtualMachine<ObjectImpl, ArrayImpl>,
    ) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let frame = self.current_frame();
        eprintln!("{}.{}: inst {}", &frame.enclosing_class.this_class, &frame.method_name, instruction);
        {
            let stack = &frame.operand_stack;
            for i in 0..5 {
                if i < stack.len() {
                    eprintln!("  stack[-{}]: {:?}", i, &stack[stack.len() - i - 1]);
                }
            }
        }
        match instruction {
            Instruction::InvokeVirtual(method_ref) => {
                self.call_method(vm, &**method_ref, CallKind::Virtual);
                Ok(None)
            }
            Instruction::InvokeSpecial(method_ref) => {
                self.call_method(vm, &**method_ref, CallKind::Special);
                Ok(None)
            }
            Instruction::InvokeStatic(method_ref) => {
                self.call_method(vm, &**method_ref, CallKind::Static);
                Ok(None)
            }
            Instruction::InvokeInterface(method_ref, count) => {
                // resolve method, *then* call it...
                let frame = self.current_frame();
                let stack = &frame.operand_stack;
                let this = &stack[stack.len() - (*count) as usize];
                if let Value::Object(obj) = &this {
                    let mut current_class = Rc::clone(obj.cls());
                    let new_ref: MethodRef = loop {
                        if let Some(_handle) = current_class.get_method(&method_ref.name, &method_ref.desc) {
                            break MethodRef {
                                class_name: current_class.this_class.to_string(),
                                name: method_ref.name.to_string(),
                                desc: method_ref.desc.to_string()
                            };
                        } else {
                            let super_class = current_class.super_class.as_ref().expect("superclass exists");
                            current_class = vm.resolve_class(super_class).expect("can resolve superclass");
                        }
                    };

                    self.call_method(vm, &new_ref, CallKind::Virtual);
                } else {
                    panic!("invokeinterface on non-object: {:?}", this);
                }
                Ok(None)
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
            Instruction::GetField(field_ref) => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                let value = match top {
                    Value::Object(obj) => {
                        obj.get_field(&field_ref.name)
//                        println!("getting the field, inst: {}", inst_class.this_class);
                        /*
                        vm
                            .get_instance_field(Rc::clone(&inst_class), Rc::clone(&fields), &field_ref.name, &field_ref.desc)
                            .unwrap()
                        */
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
                    Value::Object(obj) => {
                        obj.set_field(&field_ref.name, value);
                        /*
                        vm
                            .put_instance_field(Rc::clone(&inst_class), Rc::clone(&fields), &field_ref.name, &field_ref.desc, value);
                        */
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
                        NULL_COUNT.fetch_add(1, Ordering::SeqCst);
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
                NEW_COUNT.fetch_add(1, Ordering::SeqCst);
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
                NULL_COUNT.fetch_add(1, Ordering::SeqCst);
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
                frame_mut.branch_rel(*offset - 3);
                Ok(None)
            }
            Instruction::IfAcmpEq(offset) => {
                let frame_mut = self.current_frame_mut();
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifacmpne but insufficient arguments"));
                };
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifacmpne but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Array(l), Value::Array(r)) => {
                        if Rc::ptr_eq(&l, &r) {
                            frame_mut.branch_rel(*offset - 3);
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    (Value::Object(obj1), Value::Object(obj2)) => {
                        if obj1 == obj2 {
                            frame_mut.branch_rel(*offset - 3);
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    (Value::Null(_), Value::Null(_)) => {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    }
                    (Value::Null(_), _) |
                    (_, Value::Null(_)) => {
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("ifacmpne but invalid operand types")),
                }
            }
            Instruction::IfAcmpNe(offset) => {
                let frame_mut = self.current_frame_mut();
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifacmpne but insufficient arguments"));
                };
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifacmpne but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Array(l), Value::Array(r)) => {
                        if !Rc::ptr_eq(&l, &r) {
                            frame_mut.branch_rel(*offset - 3);
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    (Value::Object(obj1), Value::Object(obj2)) => {
                        if obj1 != obj2 {
                            frame_mut.branch_rel(*offset - 3);
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    (Value::Null(_), Value::Null(_)) => {
                        Ok(None)
                    }
                    (Value::Null(_), _) |
                    (_, Value::Null(_)) => {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("ifacmpne but invalid operand types")),
                }
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
                            frame_mut.branch_rel(*offset - 3);
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
                            frame_mut.branch_rel(*offset - 3);
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
                            frame_mut.branch_rel(*offset - 3);
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
                            frame_mut.branch_rel(*offset - 3);
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
                            frame_mut.branch_rel(*offset - 3);
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
                            frame_mut.branch_rel(*offset - 3);
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("ifne but invalid operand types")),
                }
            }
            Instruction::IfIcmpEq(offset) => {
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
                        if l == r {
                            frame_mut.branch_rel(*offset - 3);
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
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
                            frame_mut.branch_rel(*offset - 3);
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
                            frame_mut.branch_rel(*offset - 3);
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
                            frame_mut.branch_rel(*offset - 3);
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
                            frame_mut.branch_rel(*offset - 3);
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
                            frame_mut.branch_rel(*offset - 3);
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
            Instruction::FCmpG => {
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
                    (Value::Float(left), Value::Float(right)) => {
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
            Instruction::FCmpL => {
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
                    (Value::Float(left), Value::Float(right)) => {
                        if left < right {
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
            Instruction::FMul => {
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
                    // TODO: ensure that division works "correctly" around NaN and zeroes (doesn't
                    // panic pls)
                    (Value::Float(left), Value::Float(right)) => {
                        self.current_frame_mut()
                            .operand_stack
                            .push(Value::Float(left * right));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("lcmp but invalid operand types")),
                }
            }
            Instruction::FDiv => {
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
                    // TODO: ensure that division works "correctly" around NaN and zeroes (doesn't
                    // panic pls)
                    (Value::Float(left), Value::Float(right)) => {
                        self.current_frame_mut()
                            .operand_stack
                            .push(Value::Float(left / right));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("lcmp but invalid operand types")),
                }
            }
            Instruction::IfNull(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifnull but insufficient arguments"));
                };

                match value {
                    Value::Null(_v) => {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    }
                    Value::Array(_) => {
                        Ok(None)
                    }
                    Value::Object(_) => {
                        Ok(None)
                    }
                    Value::String(_) => {
                        // TODO: really need to make this an internal String-with-value situation,
                        // but strings are not null....
                        Ok(None)
                    }
                    _other => {
                        Err(VMError::BadClass("ifnull but invalid operand types"))
                    }
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
                    Value::Array(_) => {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    }
                    Value::Object(_) => {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    }
                    Value::String(_) => {
                        // TODO: really need to make this an internal String-with-value situation,
                        // but strings are not null....
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    }
                    _other => {
                        Err(VMError::BadClass("ifnotnull but invalid operand types"))
                    }
                }
            }
            Instruction::IInc(idx, constant) => {
                let frame_mut = self.current_frame_mut();
                let argument: Option<&mut Value<ObjectImpl, ArrayImpl>> = frame_mut.arguments.get_mut(*idx as usize);
                match argument {
                    Some(argument) => match argument {
                        Value::Integer(v) => { *v = v.wrapping_add(*constant as i32); }
                        _ => {
                            return Err(VMError::BadClass("iinc but not integer"));
                        }
                    },
                    None => {
                        return Err(VMError::BadClass("iinc but insufficient arguments"));
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
                            .push(Value::Long(l.wrapping_neg()));
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
            Instruction::DAdd => {
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
                    (Value::Double(l), Value::Double(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Double(l + r));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::FAdd => {
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
                    (Value::Float(l), Value::Float(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Float(l + r));
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
            Instruction::IMul => {
                let frame_mut = self.current_frame_mut();
                let left = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("imul but insufficient arguments"));
                };
                let right = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("imul but insufficient arguments"));
                };

                match (left, right) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(l.wrapping_mul(r)));
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
                            .push(Value::Integer(l.wrapping_neg()));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("ineg but invalid operand types")),
                }
            }
            Instruction::IXor => {
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
                            .push(Value::Integer(l ^ r));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iand but invalid operand types")),
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
            Instruction::IUshr => {
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
                        let res = (v1 as u32) >> ((v2 as u32) & 0x1f);
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(res as i32));
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
            Instruction::F2D => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("i2d but insufficient arguments"));
                };

                match value {
                    Value::Float(f) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Double(f as f64));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("i2d but invalid operand types")),
                }
            }
            Instruction::F2I => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("i2d but insufficient arguments"));
                };

                match value {
                    Value::Float(f) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Integer(f as i32));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("i2d but invalid operand types")),
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
            Instruction::L2F => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("l2f but insufficient arguments"));
                };

                match value {
                    Value::Long(l) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Float(l as f32));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("l2f but invalid operand types")),
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
            Instruction::D2L => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match value {
                    Value::Double(d) => {
                        frame_mut
                            .operand_stack
                            .push(Value::Long(d as i64));
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
            Instruction::LReturn => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("lreturn but insufficient arguments"));
                };

                match value {
                    Value::Long(_) => {
                        self.leave();
                        Ok(Some(value))
                    }
                    _ => Err(VMError::BadClass("lreturn but invalid operand types")),
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
                    Value::String(_) => {
                        self.leave();
                        Ok(Some(value))
                    }
                    Value::Array(_) => {
                        self.leave();
                        Ok(Some(value))
                    }
                    Value::Object(_) => {
                        self.leave();
                        Ok(Some(value))
                    }
                    Value::Null(_) => {
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
                    Value::Object(_) => {
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
                        jvm::synthetic::class_object_new(vm, &c)
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
                        jvm::synthetic::class_object_new(vm, &c)
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
            Instruction::InstanceOf(name) => {
                let frame_mut = self.current_frame_mut();
                let stack = &mut frame_mut.operand_stack;

                let item = stack.pop().expect("operand available");

                let mut item_class = {
                    if item.is_null() {
                        stack.push(Value::Integer(0));
                        return Ok(None);
                    }

                    match item {
                        Value::Object(obj) => {
                            Rc::clone(obj.cls())
                        },
                        Value::Array(_) => {
                            vm.resolve_class("java/lang/Object").expect("object exists")
                        }
                        Value::String(_) => {
                            vm.resolve_class("java/lang/String").expect("string exists")
                        }
                        other => {
                            panic!("cant instanceof a non-reference type: {:?}", other);
                        }
                    }
                };
                loop {
                    let cls = Rc::clone(&item_class);
                    if cls.this_class.as_str() == name.as_str() {
                        stack.push(Value::Integer(1));
                        return Ok(None);
                    } else if cls.interfaces.contains(&name.to_string()) {
                        stack.push(Value::Integer(1));
                        return Ok(None);
                    } else if let Some(super_class) = cls.super_class.as_ref() {
                        // not this class, not an interface, maybe superclass?
                        item_class = vm.resolve_class(super_class).expect("superclass exists");
                    } else {
                        stack.push(Value::Integer(0));
                        return Ok(None);
                    }
                }
            }
            Instruction::CheckCast(name) => {
                let frame_mut = self.current_frame_mut();
                let stack = &frame_mut.operand_stack;
                let mut array = false;
                let mut check_class = match &stack[stack.len() - 1] {
                    Value::Object(obj) => {
                        Rc::clone(obj.cls())
                    }
                    Value::Array(_) => {
                        array = true;
                        vm.resolve_class("java/lang/Object").expect("arrays are objects")
                    },
                    Value::String(_) => {
                        vm.resolve_class("java/lang/String").expect("strings exist")
                    },
                    Value::Null(_) => {
                        // TODO: think this should raise an exception...
                        return Ok(None);
                    }
                    _ => {
                        return Err(VMError::BadClass("invalid operand for checkcast"))
                    }
                };
                eprintln!("checking if {:?} can be cast to {}", check_class.this_class, name);

                let name = {
                    if array {
                        if !name.starts_with("[") {
                            panic!("checkcast failed (was array, did not want array)");
                        }
                        &name[2..name.len()-1]
                    } else {
                        &name[..]
                    }
                };

                loop {
                    let cls = Rc::clone(&check_class);
                    eprintln!("checking {}, interfaces {:?}, super={:?}", cls.this_class, cls.interfaces, cls.super_class);
                    if cls.this_class.as_str() == name {
                        return Ok(None);
                    } else if cls.interfaces.contains(&name.to_string()) {
                        return Ok(None);
                    } else if let Some(super_class) = cls.super_class.as_ref() {
                        // not this class, not an interface, maybe superclass?
                        check_class = vm.resolve_class(super_class).expect("superclass exists");
                    } else {
                        // should be ClassCastException but not right now
                        return Err(VMError::BadClass("checkclass failed"));
                    }
                }
            }
            Instruction::Return => {
                self.leave();
                Ok(None)
            }
            Instruction::Nop => { Ok(None) }
            Instruction::Pop => {
                self.current_frame_mut().operand_stack.pop();
                Ok(None)
            }
            Instruction::Pop2 => {
                self.current_frame_mut().operand_stack.pop();
                self.current_frame_mut().operand_stack.pop();
                Ok(None)
            }
            // TODO: zvm is not (yet) concurrent so all operations are already thread-safe.
            Instruction::MonitorEnter => { Ok(None) }
            Instruction::MonitorExit => { Ok(None) }
            other => {
                todo!("implement {}", other);
            }
        }
    }
    #[allow(dead_code)]
    fn return_value(&mut self) -> Option<Value<ObjectImpl, ArrayImpl>> {
        // panic!("Hello there");
        None
    }
}

pub struct JvmObject {
    class: Rc<ClassFile>,
    // TODO: declare classes to zvm and get layout ids to use here..
    // zvm_layout_id: LayoutId,
    fields: Rc<RefCell<HashMap<String, Value<ObjectImpl, ArrayImpl>>>>,
}

impl PartialEq for JvmObject {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.fields, &other.fields)
    }
}

impl Clone for JvmObject {
    fn clone(&self) -> Self {
        JvmObject {
            class: Rc::clone(&self.class),
            fields: Rc::clone(&self.fields),
        }
    }
}

impl JvmObject {
    pub fn new_with_data(class: Rc<ClassFile>, data: HashMap<String, Value<ObjectImpl, ArrayImpl>>) -> Self {
        JvmObject {
            fields: Rc::new(RefCell::new(data)),
            class,
        }
    }
    pub fn create(class: Rc<ClassFile>) -> Self {
        JvmObject {
            fields: Rc::new(RefCell::new(HashMap::new())),
            class,
        }
    }

    pub fn with_field(self, name: &str, value: Value) -> Self {
        self.fields.borrow_mut().insert(name.to_string(), value);
        self
    }

    pub fn cls(&self) -> &Rc<ClassFile> {
        &self.class
    }

    pub fn fields_ptr(&self) -> *const u8 {
        let ptr: *const HashMap<String, Value<ObjectImpl, ArrayImpl>> = (&*self.fields.borrow()) as *const HashMap<String, Value<ObjectImpl, ArrayImpl>>;
        ptr as *const u8
    }

    pub fn get_field(&self, name: &str) -> Value {
        self.fields.borrow().get(name).expect("field is defined").clone()
    }

    pub fn set_field(&self, name: &str, value: Value) {
        self.fields.borrow_mut().insert(name.to_string(), value);
    }

    pub fn new_inst(class_file: Rc<ClassFile>) -> JvmObject {
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
        JvmObject { fields: Rc::new(RefCell::new(fields)), class: class_file }
    }
}

pub enum Value<ObjectImpl: JvmObject, ArrayImpl: JvmArray> {
    Integer(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    // Array(Rc<RefCell<Box<[Value]>>>),
    Array(ArrayImpl),
    String(Rc<Vec<u8>>),
    Object(ObjectImpl),
    Null(String), // Null, of type `String`
    Uninitialized,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Integer(i) => { write!(f, "{}", i) },
            Value::Long(l) => { write!(f, "{}", l) },
            Value::Float(value) => { write!(f, "{}", value) },
            Value::Double(d) => { write!(f, "{}", d) },
            Value::Array(array) => {
                write!(f, "Array({:?})", array)
            },
            Value::String(bytes) => {
                if let Ok(s) = std::str::from_utf8(bytes) {
                    write!(f, "String({:?})", s)
                } else {
                    write!(f, "String({:?})", bytes)
                }
            }
            Value::Object(obj) => {
                write!(f, "Object({})", obj.cls())
            }
            Value::Null(cls) => {
                write!(f, "Null({})", cls)
            }
            Value::Uninitialized => { f.write_str("uninitialized") }
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Integer(v) => Value::Integer(*v),
            Value::Long(v) => Value::Long(*v),
            Value::Float(v) => Value::Float(*v),
            Value::Double(v) => Value::Double(*v),
            Value::Array(v) => Value::Array(v.clone()),
            Value::String(v) => Value::String(Rc::clone(v)),
            Value::Object(obj) => Value::Object(obj.clone()),
            Value::Null(v) => Value::Null(v.clone()),
            Value::Uninitialized => Value::Uninitialized,
        }
    }
}

impl Value<ObjectImpl: JvmObject, ArrayImpl: JvmArray> {
    pub fn new_inst(class_file: Rc<ClassFile>) -> Value {
        Value::Object(ObjectImpl::new_inst(class_file))
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

    pub fn parse_from(s: &str) -> Option<Value<ObjectImpl, ArrayImpl>> {
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

    pub fn is_null(&self) -> bool {
        if let Value::Null(_) = self {
            true
        } else {
            false
        }
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
            Value::Object(obj) => {
                panic!();
                /*
                unsafe {
                    let ptr = Rc::into_raw(Rc::clone(v1));
                    ptr.hash(state);
                    Rc::from_raw(ptr);
                    let ptr = Rc::into_raw(Rc::clone(v2));
                    ptr.hash(state);
                    Rc::from_raw(ptr);
                }
                */
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
            (Value::Object(obj1), Value::Object(obj2)) => { obj1 == obj2 },
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

pub struct VirtualMachine<ObjectImpl: JvmObject, ArrayImpl: JvmArray> {
    classes: HashMap<String, Rc<ClassFile>>,
    class_instances: HashMap<String, ObjectImpl>,
    static_instances: HashMap<ClassFileRef, HashMap<String, Value<ObjectImpl, ArrayImpl>>>,
    native_instances: HashMap<ValueRef, RefCell<NativeObject>>, // TODO: should probably be an ObjectImpl?
    classpath: Vec<PathBuf>,
    // TODO: actually reuse the VirtualMachine for <clinit> calls - mutually recursive classes
    // would loop and crash right now, among other issues..
    first_run: bool,
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

impl<ObjectImpl: JvmObject, ArrayImpl: JvmArray> VirtualMachine<ObjectImpl, ArrayImpl> {
    pub fn new(initial_classpath: Vec<PathBuf>) -> Self {
        VirtualMachine {
            classes: HashMap::new(),
            class_instances: HashMap::new(),
            static_instances: HashMap::new(),
            native_instances: HashMap::new(),
            classpath: initial_classpath,
            first_run: true,
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
        fields: Rc<RefCell<HashMap<String, Value<ObjectImpl, ArrayImpl>>>>,
        name: &str,
        ty: &str,
    ) -> Option<Value<ObjectImpl, ArrayImpl>> {
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
        fields: Rc<RefCell<HashMap<String, Value<ObjectImpl, ArrayImpl>>>>,
        name: &str,
        ty: &str,
        value: Value<ObjectImpl, ArrayImpl>,
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
    ) -> Option<Value<ObjectImpl, ArrayImpl>> {
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
        value: Value<ObjectImpl, ArrayImpl>,
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

    pub fn resolve_class(&mut self, class_name: &str) -> Result<Rc<ClassFile>, VMError> {
//        eprintln!("resolve class: {}", referent);
        if let Some(cls) = self.classes.get(class_name) {
            return Ok(Rc::clone(cls));
        }

        let new_class = if let Some(new_class) = jvm::synthetic::build_synthetic_class(class_name) {
            new_class
        } else {
            use std::collections::hash_map::Entry;
            use std::fs::File;
            let mut res: Option<ClassFile> = None;
            for path in self.classpath.iter() {
                let possible_class = path.join(PathBuf::from(format!("{}.class", class_name)));
//                            println!("-- checking {}", possible_class.display());
                if possible_class.exists() {
                    let class_file = ClassFile::validate(
                        &crate::class_file::unvalidated::read::class_header(
                            &mut File::open(possible_class).unwrap()
                        ).unwrap()
                    ).unwrap();
                    res = Some(jvm::synthetic::augment_classfile(class_file));
                    break;
                }
            }
            if let Some(res) = res {
                res
            } else {
                return Err(VMError::BadClass("could not resolve class"));
            }
        };

        self.register(class_name.to_string(), new_class)
    }

    pub fn register(
        &mut self,
        class_name: String,
        class_file: ClassFile,
    ) -> Result<Rc<ClassFile>, VMError> {
        eprintln!("registering class {}", class_name);
        let rc = Rc::new(class_file);
        self.classes.insert(class_name, Rc::clone(&rc));

        if let Some(native_method) = rc.native_methods.get("<clinit>()V") {
            let mut state = VMState::new(
                Rc::new(MethodBody::native()),
                Rc::clone(&rc),
                "<clinit>".to_string(),
                vec![],
            );
            native_method(&mut state, self)?;
        } else if let Some(method) = rc.get_method("<clinit>", "()V") {
            if method.access().is_native() {
                let mut state = VMState::new(
                    Rc::new(MethodBody::native()),
                    Rc::clone(&rc),
                    "<clinit>".to_string(),
                    vec![],
                );
                let native_method = rc.native_methods.get("<clinit>()V").expect("native clinit has native method impl");
                native_method(&mut state, self)?;
            } else {
                let mut state = VMState::new(
                    Rc::clone(method.body.as_ref().expect("clinit has a body")),
                    Rc::clone(&rc),
                    "<clinit>".to_string(),
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
        args: Vec<Value<ObjectImpl, ArrayImpl>>,
    ) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
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

        let mut state = VMState::new(Rc::clone(code), Rc::clone(class_ref), method.name.to_string(), args);
        self.interpret(&mut state)
    }

    fn interpret(&mut self, state: &mut VMState<ObjectImpl, ArrayImpl>) -> Result<Option<Value<ObjectImpl, ArrayImpl>>, VMError> {
        let first_run = self.first_run;

        // magic incantation to awaken the machine
        if self.first_run {
            println!("zoom zoom");
            self.first_run = false;
        }

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
                                if let Value::Object(obj) = &value {
                                    if self.is_exception(obj.cls(), exception_record.catch_type.as_str()) {
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
                        if let Value::Object(obj) = &value {
                            if let Value::String(msg) = obj.get_field("message") {
                                eprintln!("unhandled {}: {}", obj.cls().this_class.as_str(), String::from_utf8_lossy(msg.as_ref()));
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

        if first_run {
            eprintln!("------");
            eprintln!("STATS:");
            eprintln!("  loaded {} classes", self.classes.len());
            eprintln!("  loaded {} scala classes", self.classes.keys().filter(|x| x.starts_with("scala/")).count());
            eprintln!("  {} static instances across {} classes",
                self.static_instances.values().map(|values| values.len()).sum::<usize>(),
                self.static_instances.len()
            );
            eprintln!("  {} nulls created", NULL_COUNT.load(Ordering::SeqCst));
            eprintln!("  {} `new`s", NEW_COUNT.load(Ordering::SeqCst));
        }

        Ok(None)
        //        Ok(state.return_value())
    }
}

use crate::compiler::ir;

pub struct Arg { pub ty: ir::ValueType }

#[allow(unused_assignments)]
pub fn parse_signature_string(signature: &str) -> Option<(Vec<Arg>, Option<Arg>)> {
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
            b'F' => { // float
                if reading_type {
                    continue;
                }
                if reading_return {
                    return Some((args, Some(Arg { ty: ir::ValueType::Float })));
                } else {
                    args.push(Arg { ty: ir::ValueType::Float });
                }
            }
            b'D' => { // double
                if reading_type {
                    continue;
                }
                if reading_return {
                    return Some((args, Some(Arg { ty: ir::ValueType::Double })));
                } else {
                    args.push(Arg { ty: ir::ValueType::Double });
                }
            }
            b'Z' | // boolean
            b'B' | // byte
            b'C' | // char
            b'S' | // short
            b'I' => { // int
                if reading_type {
                    continue;
                }
                if reading_return {
                    return Some((args, Some(Arg { ty: ir::ValueType::Int })));
                } else {
                    args.push(Arg { ty: ir::ValueType::Int });
                }

            }
            b'J' => { // long
                if reading_type {
                    continue;
                }
                if reading_return {
                    return Some((args, Some(Arg { ty: ir::ValueType::Long })));
                } else {
                    args.push(Arg { ty: ir::ValueType::Long });
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
                        return Some((args, Some(Arg { ty: ir::ValueType::Ref })));
                    } else {
                        args.push(Arg { ty: ir::ValueType::Ref });
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

fn interpreted_method_call<ObjectImpl: JvmObject, ArrayImpl: JvmArray>(
    state: &mut VMState<ObjectImpl, ArrayImpl>,
    _vm: &mut VirtualMachine<ObjectImpl, ArrayImpl>,
    method: Rc<MethodHandle>,
    method_class: Rc<ClassFile>,
    method_type: &str,
    kind: CallKind,
) -> Result<(), VMError> {
    // TODO: parse out arguments from method type, check against available operands, do the call
    //
    // today: [...], do the call

    // TODO typecheck ret and the returned object type
    let (args, _ret) = parse_signature_string(method_type).expect("signature string is valid (not like there's much validation right now, come on!!");

    let frame = state.current_frame_mut();

    let mut real_args = std::collections::VecDeque::new();
    for _arg in args {
        real_args.push_front(frame.operand_stack.pop().expect("argument is present"));
    }
    if kind.takes_self() {
        real_args.push_front(frame.operand_stack.pop().expect("argument is present"));
    }

    state.enter(
        Rc::clone(method.body.as_ref().expect("method has a body")),
        method_class,
        &method.name,
        real_args.drain(..).collect(),
    );
    Ok(())
}
