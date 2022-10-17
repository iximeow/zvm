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

pub mod jvm;

static NULL_COUNT: AtomicUsize = AtomicUsize::new(0);
static NEW_COUNT: AtomicUsize = AtomicUsize::new(0);

#[allow(dead_code)]
struct CallFrame<ValueImpl: JvmValue> {
    offset: u32,
    arguments: Vec<ValueImpl>,
    body: Rc<MethodBody>,
    pub enclosing_class: Rc<ClassFile>,
    pub method_name: String,
    operand_stack: Vec<ValueImpl>,
}

impl<ValueImpl: JvmValue> CallFrame<ValueImpl> {
    pub fn new(
        body: Rc<MethodBody>,
        enclosing_class: Rc<ClassFile>,
        method_name: String,
        mut arguments: Vec<ValueImpl>,
    ) -> Self {
        while arguments.len() < (body.max_locals as usize) {
            arguments.push(ValueImpl::uninitialized());
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

pub struct VMState<ValueImpl: JvmValue> {
    // Attribute is actually a Code (anything else is an error)
    call_stack: Vec<CallFrame<ValueImpl>>,
    throwing: bool,
}

#[allow(dead_code)]
impl<ValueImpl: JvmValue> VMState<ValueImpl> {
    pub fn new(
        code: Rc<MethodBody>,
        method_class: Rc<ClassFile>,
        method_name: String,
        initial_args: Vec<ValueImpl>,
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

    fn current_frame(&self) -> &CallFrame<ValueImpl> {
        self.call_stack.iter().rev().next().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame<ValueImpl> {
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
        arguments: Vec<ValueImpl>,
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

    fn interpret_iload(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&ValueImpl> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => {
                if let Some(v) = argument.as_integer() {
                    ValueImpl::integer(*v)
                } else if argument.is_uninitialized() {
                    ValueImpl::default_of("I")
                } else {
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

    fn interpret_istore(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_lload(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&ValueImpl> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => {
                if let Some(v) = argument.as_long() {
                    ValueImpl::long(*v)
                } else if argument.is_uninitialized() {
                    ValueImpl::default_of("J")
                } else {
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

    fn interpret_lstore(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_fload(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&ValueImpl> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => {
                if let Some(v) = argument.as_float() {
                    ValueImpl::float(*v)
                } else if argument.is_uninitialized() {
                    ValueImpl::default_of("F")
                } else {
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

    fn interpret_fstore(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_dload(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&ValueImpl> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => {
                if let Some(v) = argument.as_double() {
                    ValueImpl::double(*v)
                } else if argument.is_uninitialized() {
                    ValueImpl::default_of("D")
                } else {
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

    fn interpret_dstore(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn interpret_aload(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&ValueImpl> = frame_mut.arguments.get(idx as usize);
        let operand = if let Some(argument) = argument {
            if argument.is_uninitialized() {
                // TODO: type check argument as an object?
                NULL_COUNT.fetch_add(1, Ordering::SeqCst);
                ValueImpl::null(String::new())
            } else {
                argument.clone()
            }
        } else {
            return Err(VMError::BadClass("dload but insufficient arguments"));
        };

        frame_mut.operand_stack.push(operand);
        Ok(None)
    }

    fn interpret_astore(&mut self, idx: u16) -> Result<Option<ValueImpl>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut
            .operand_stack
            .pop()
            .expect("operand stack has value");
        frame_mut.arguments[idx as usize] = value.clone();

        Ok(None)
    }

    fn call_method(&mut self, vm: &mut VirtualMachine<ValueImpl>, method: &MethodRef, kind: CallKind) {
        let mut current_class = match kind {
            CallKind::Virtual => {
                // virtual calls can be abstract and resolution has to start at the ref being called on
                let (args, _ret) = parse_signature_string(&method.desc).expect("signature is valid");
                let frame = self.current_frame();
                let stack = &frame.operand_stack;
                let receiver = &stack[stack.len() - args.len() - 1];
                if let Some(o) = receiver.as_object() {
                    o.cls()
                } else if let Some(_) = receiver.as_array() {
                    vm.resolve_class("java/lang/Object").unwrap()
                } else {
                    panic!("unexpected receiver {:?}", receiver);
                }
            },
            CallKind::Static | CallKind::Special => {
                vm.resolve_class(&method.class_name).unwrap()
            }
        };
        let init_class = Rc::clone(&current_class);
        loop {
            let target_class = Rc::clone(&current_class);
            if let Some(native_method) = vm.get_native_method(Rc::clone(&target_class), format!("{}{}", &method.name, &method.desc)) {
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
        vm: &mut VirtualMachine<ValueImpl>,
    ) -> Result<Option<ValueImpl>, VMError> {
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
                if let Some(obj) = this.as_object() {
                    let mut current_class = obj.cls();
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

                let value = if let Some(obj) = top.as_object() {
                    obj.get_field(&field_ref.name)
//                    println!("getting the field, inst: {}", inst_class.this_class);
                    /*
                    vm
                        .get_instance_field(Rc::clone(&inst_class), Rc::clone(&fields), &field_ref.name, &field_ref.desc)
                        .unwrap()
                    */
                } else {
                    panic!("should be an object, was {:?}, getting {:?}", top, field_ref);
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

                if let Some(obj) = top.as_object() {
                    obj.set_field(&field_ref.name, value);
                    /*
                    vm
                        .put_instance_field(Rc::clone(&inst_class), Rc::clone(&fields), &field_ref.name, &field_ref.desc, value);
                    */
                } else {
                    panic!("should be an object, was {:?}, getting {:?}", top, field_ref);
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
                if let Some(size) = top.as_integer() {
                    for _ in 0..*size {
                        elems.push(ValueImpl::integer(0));
                    }
                } else {
                    panic!("bad argument to newarray");
                }

                self.current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::array_with_data(
                        // TODO: use correct class (tpe?)
                        vm.resolve_class("java/lang/Object").expect("class is defined"),
                        elems.into_boxed_slice(),
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
                if let Some(size) = top.as_integer() {
                    for _ in 0..*size {
                        NULL_COUNT.fetch_add(1, Ordering::SeqCst);
                        elems.push(ValueImpl::null(String::new()));
                    }
                } else {
                    panic!("bad argument to newarray");
                }

                self.current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::array_with_data(
                        // TODO: use correct class (tpe?)
                        vm.resolve_class("java/lang/Object").expect("class is defined"),
                        elems.into_boxed_slice()
                    ));
                Ok(None)
            }
            Instruction::New(tpe) => {
                NEW_COUNT.fetch_add(1, Ordering::SeqCst);
                self.current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::object(ValueImpl::ObjectTy::new_inst(
                        vm.resolve_class(tpe)?,
                    )));
                Ok(None)
            }
            Instruction::BIPush(b) => {
                self.current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::integer(*b as i32));
                Ok(None)
            }
            Instruction::SIPush(s) => {
                self.current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::integer(*s as i32));
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    if let Some(value) = elements.get_elem(*index as usize) {
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    *elements.get_elem_mut(*index as usize).expect("TODO: handle oob") = value;
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    if let Some(value) = elements.get_elem(*index as usize) {
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    *elements.get_elem_mut(*index as usize).expect("TODO: handle oob") = value;
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    *elements.get_elem_mut(*index as usize).expect("TODO: handle oob") = value;
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.get_elem(*index as usize).expect("TODO: handle oob").clone());
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    *elements.get_elem_mut(*index as usize).expect("TODO: handle oob") = value;
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.get_elem(*index as usize).expect("TODO: handle oob").clone());
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    *elements.get_elem_mut(*index as usize).expect("TODO: handle oob") = value;
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.get_elem(*index as usize).expect("TODO: handle oob").clone());
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    *elements.get_elem_mut(*index as usize).expect("TODO: handle oob") = value;
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.get_elem(*index as usize).expect("TODO: handle oob").clone());
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    *elements.get_elem_mut(*index as usize).expect("TODO: handle oob") = value;
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.get_elem(*index as usize).expect("TODO: handle oob").clone());
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    *elements.get_elem_mut(*index as usize).expect("TODO: handle oob") = value;
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

                if let (Some(elements), Some(index)) =
                    (array.as_array(), index.as_integer())
                {
                    // TODO: homogeneously typed arrays
                    self
                        .current_frame_mut()
                        .operand_stack
                        .push(elements.get_elem(*index as usize).expect("TODO: handle oob").clone());
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

                if let Some(elems) = top.as_array() {
                    self.current_frame_mut()
                        .operand_stack
                        .push(ValueImpl::integer(elems.len() as i32));
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
                    .push(ValueImpl::null(String::new()));
                Ok(None)
            }
            Instruction::DConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::double(0.0f64));
                Ok(None)
            }
            Instruction::DConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::double(1.0f64));
                Ok(None)
            }
            Instruction::FConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::float(0.0f32));
                Ok(None)
            }
            Instruction::FConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::float(1.0f32));
                Ok(None)
            }
            Instruction::LConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::long(0));
                Ok(None)
            }
            Instruction::LConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::long(1));
                Ok(None)
            }
            Instruction::IConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::integer(0));
                Ok(None)
            }
            Instruction::IConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::integer(1));
                Ok(None)
            }
            Instruction::IConst2 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::integer(2));
                Ok(None)
            }
            Instruction::IConst3 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::integer(3));
                Ok(None)
            }
            Instruction::IConst4 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::integer(4));
                Ok(None)
            }
            Instruction::IConst5 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::integer(5));
                Ok(None)
            }
            Instruction::IConstM1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut
                    .operand_stack
                    .push(ValueImpl::integer(-1));
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

                if left.is_reference() && right.is_reference() {
                    if left == right {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("ifacmpeq but invalid operand types"))
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

                if left.is_reference() && right.is_reference() {
                    if left != right {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("ifacmpne but invalid operand types"))
                }
            }
            Instruction::IfGe(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                if let Some(v) = value.as_integer() {
                    if *v >= 0 {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
                }
            }
            Instruction::IfGt(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                if let Some(v) = value.as_integer() {
                    if *v > 0 {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
                }
            }
            Instruction::IfLe(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifle but insufficient arguments"));
                };

                if let Some(v) = value.as_integer() {
                    if *v <= 0 {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
                }
            }
            Instruction::IfLt(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                if let Some(v) = value.as_integer() {
                    if *v < 0 {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
                }
            }
            Instruction::IfEq(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifeq but insufficient arguments"));
                };

                if let Some(v) = value.as_integer() {
                    if *v == 0 {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
                }
            }
            Instruction::IfNe(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifne but insufficient arguments"));
                };

                if let Some(v) = value.as_integer() {
                    if *v != 0 {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
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

                if let (Some(l), Some(r)) = (left.as_integer(), right.as_integer()) {
                    if l == r {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
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

                if let (Some(l), Some(r)) = (left.as_integer(), right.as_integer()) {
                    if l <= r {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
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

                if let (Some(l), Some(r)) = (left.as_integer(), right.as_integer()) {
                    if l < r {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
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

                if let (Some(l), Some(r)) = (left.as_integer(), right.as_integer()) {
                    if l >= r {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
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

                if let (Some(l), Some(r)) = (left.as_integer(), right.as_integer()) {
                    if l > r {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
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

                if let (Some(l), Some(r)) = (left.as_integer(), right.as_integer()) {
                    if l != r {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("iadd but invalid operand types"))
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

                match (left.as_long(), right.as_long()) {
                    (Some(left), Some(right)) => {
                        let left = *left as i64;
                        let right = *right as i64;
                        if left > right {
                            self.current_frame_mut()
                                .operand_stack
                                .push(ValueImpl::integer(1));
                        } else if left == right {
                            self.current_frame_mut()
                                .operand_stack
                                .push(ValueImpl::integer(0));
                        } else {
                            self.current_frame_mut()
                                .operand_stack
                                .push(ValueImpl::integer(-1));
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

                match (left.as_float(), right.as_float()) {
                    (Some(left), Some(right)) => {
                        if left > right {
                            self.current_frame_mut()
                                .operand_stack
                                .push(ValueImpl::integer(1));
                        } else if left == right {
                            self.current_frame_mut()
                                .operand_stack
                                .push(ValueImpl::integer(0));
                        } else {
                            self.current_frame_mut()
                                .operand_stack
                                .push(ValueImpl::integer(-1));
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

                match (left.as_float(), right.as_float()) {
                    (Some(left), Some(right)) => {
                        if left < right {
                            self.current_frame_mut()
                                .operand_stack
                                .push(ValueImpl::integer(1));
                        } else if left == right {
                            self.current_frame_mut()
                                .operand_stack
                                .push(ValueImpl::integer(0));
                        } else {
                            self.current_frame_mut()
                                .operand_stack
                                .push(ValueImpl::integer(-1));
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

                match (left.as_float(), right.as_float()) {
                    // TODO: ensure that division works "correctly" around NaN and zeroes (doesn't
                    // panic pls)
                    (Some(left), Some(right)) => {
                        self.current_frame_mut()
                            .operand_stack
                            .push(ValueImpl::float(left * right));
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

                match (left.as_float(), right.as_float()) {
                    // TODO: ensure that division works "correctly" around NaN and zeroes (doesn't
                    // panic pls)
                    (Some(left), Some(right)) => {
                        self.current_frame_mut()
                            .operand_stack
                            .push(ValueImpl::float(left / right));
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

                if value.is_reference() {
                    if value.as_null().is_some() {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("ifnull but invalid operand types"))
                }
            }
            Instruction::IfNonNull(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ifnotnull but insufficient arguments"));
                };

                if value.is_reference() {
                    if value.as_null().is_some() {
                        Ok(None)
                    } else {
                        frame_mut.branch_rel(*offset - 3);
                        Ok(None)
                    }
                } else {
                    Err(VMError::BadClass("ifnotnull but invalid operand types"))
                }
            }
            Instruction::IInc(idx, constant) => {
                let frame_mut = self.current_frame_mut();
                let argument: Option<&mut ValueImpl> = frame_mut.arguments.get_mut(*idx as usize);
                match argument {
                    Some(argument) => if let Some(v) = argument.as_integer() {
                        *argument = ValueImpl::integer(v.wrapping_add(*constant as i32));
                    } else {
                        return Err(VMError::BadClass("iinc but not integer"));
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

                match (value.as_long(), amount.as_integer()) {
                    (Some(value), Some(amount)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::long(((*value as u64) >> (*amount & 0x3f)) as i64));
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

                match value.as_long() {
                    Some(l) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::long(l.wrapping_neg()));
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

                match (left.as_long(), right.as_long()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::long(l & r));
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

                match (left.as_long(), right.as_long()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::long(l | r));
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

                match (value.as_long(), amount.as_integer()) {
                    (Some(value), Some(amount)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::long(((*value as u64) << (*amount & 0x3f)) as i64));
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

                match (left.as_double(), right.as_double()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::double(l + r));
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

                match (left.as_float(), right.as_float()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::float(l + r));
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

                match (left.as_integer(), right.as_integer()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(l.wrapping_add(*r)));
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

                match (left.as_integer(), right.as_integer()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(l.wrapping_mul(*r)));
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

                match (left.as_long(), right.as_long()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::long(l.wrapping_add(*r)));
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

                match (left.as_integer(), right.as_integer()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(l.wrapping_sub(*r)));
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

                match value.as_integer() {
                    Some(l) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(l.wrapping_neg()));
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

                match (left.as_integer(), right.as_integer()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(l ^ r));
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

                match (left.as_integer(), right.as_integer()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(l & r));
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

                match (left.as_integer(), right.as_integer()) {
                    (Some(l), Some(r)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(l | r));
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

                match (value1.as_integer(), value2.as_integer()) {
                    (Some(v1), Some(v2)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(v1 >> (v2 & 0x1f)));
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

                match (value1.as_integer(), value2.as_integer()) {
                    (Some(v1), Some(v2)) => {
                        let res = (*v1 as u32) >> ((*v2 as u32) & 0x1f);
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(res as i32));
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

                match (value1.as_integer(), value2.as_integer()) {
                    (Some(v1), Some(v2)) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(v1 << (v2 & 0x1f)));
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

                match value.as_float() {
                    Some(f) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::double(*f as f64));
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

                match value.as_float() {
                    Some(f) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(*f as i32));
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

                match value.as_long() {
                    Some(l) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(*l as i32));
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

                match value.as_long() {
                    Some(l) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::float(*l as f32));
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

                match value.as_integer() {
                    Some(l) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(*l as i8 as i32));
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

                match value.as_integer() {
                    Some(l) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::integer(*l as i16 as u16 as i32));
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

                match value.as_integer() {
                    Some(l) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::double(*l as f64));
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

                match value.as_integer() {
                    Some(l) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::float(*l as f32));
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

                match value.as_integer() {
                    Some(l) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::long(*l as i64));
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

                match value.as_double() {
                    Some(d) => {
                        frame_mut
                            .operand_stack
                            .push(ValueImpl::long(*d as i64));
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

                match value.as_double() {
                    Some(_) => {
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

                match value.as_integer() {
                    Some(_) => {
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

                match value.as_long() {
                    Some(_) => {
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

                if value.is_reference() {
                    self.leave();
                    Ok(Some(value))
                } else {
                    Err(VMError::BadClass("areturn but invalid operand types"))
                }
            }
            Instruction::AThrow => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("athrow but insufficient arguments"));
                };

                if value.as_object().is_some() {
                    self.throw();
                    Ok(Some(value))
                } else {
                    Err(VMError::BadClass("athrow but invalid operand types"))
                }
            }
            Instruction::Ldc(c) => {
                let value = match &**c {
                    Constant::Integer(i) => ValueImpl::integer(*i as i32),
                    Constant::Float(f) => ValueImpl::float(*f),
                    Constant::String(s) => {
                        ValueImpl::string(vm, s)
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
                    Constant::Integer(i) => ValueImpl::integer(*i as i32),
                    Constant::Float(f) => ValueImpl::float(*f),
                    Constant::String(s) => {
                        ValueImpl::string(vm, s)
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
                    Constant::Long(l) => ValueImpl::long(*l as i64),
                    Constant::Double(d) => ValueImpl::double(*d),
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
                    if item.as_null().is_some() {
                        stack.push(ValueImpl::integer(0));
                        return Ok(None);
                    }

                    if let Some(obj) = item.as_object() {
                        obj.cls()
                    } else if let Some(_) = item.as_array() {
                        vm.resolve_class("java/lang/Object").expect("object exists")
                    } else {
                        // TODO: handle null?
                        panic!("cant instanceof a non-reference type: {:?}", item);
                    }
                };
                loop {
                    let cls = Rc::clone(&item_class);
                    if cls.this_class.as_str() == name.as_str() {
                        stack.push(ValueImpl::integer(1));
                        return Ok(None);
                    } else if cls.interfaces.contains(&name.to_string()) {
                        stack.push(ValueImpl::integer(1));
                        return Ok(None);
                    } else if let Some(super_class) = cls.super_class.as_ref() {
                        // not this class, not an interface, maybe superclass?
                        item_class = vm.resolve_class(super_class).expect("superclass exists");
                    } else {
                        stack.push(ValueImpl::integer(0));
                        return Ok(None);
                    }
                }
            }
            Instruction::CheckCast(name) => {
                let frame_mut = self.current_frame_mut();
                let stack = &frame_mut.operand_stack;
                let mut array = false;
                let top_of_stack = &stack[stack.len() - 1];
                let mut check_class = if let Some(obj) = top_of_stack.as_object() {
                    obj.cls()
                } else if let Some(_) = top_of_stack.as_array() {
                    array = true;
                    vm.resolve_class("java/lang/Object").expect("arrays are objects")
                } else if let Some(_) = top_of_stack.as_null() {
                    // TODO: think this should raise an exception...
                    return Ok(None);
                } else {
                    return Err(VMError::BadClass("invalid operand for checkcast"))
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
    fn return_value(&mut self) -> Option<ValueImpl> {
        // panic!("Hello there");
        None
    }
}

pub enum SimpleJvmValue {
    Integer(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    // Array(Rc<RefCell<Box<[Value]>>>),
    Array(SimpleJvmArray),
    String(Rc<Vec<u8>>),
    Object(SimpleJvmObject),
    Null(String), // Null, of type `String`
    Uninitialized,
}

impl fmt::Debug for SimpleJvmValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Integer(i) => { write!(f, "{}", i) },
            Self::Long(l) => { write!(f, "{}", l) },
            Self::Float(value) => { write!(f, "{}", value) },
            Self::Double(d) => { write!(f, "{}", d) },
            Self::Array(array) => {
                write!(f, "Array({:?})", array)
            },
            Self::String(bytes) => {
                if let Ok(s) = std::str::from_utf8(bytes) {
                    write!(f, "String({:?})", s)
                } else {
                    write!(f, "String({:?})", bytes)
                }
            }
            Self::Object(obj) => {
                write!(f, "Object({})", &obj.cls().this_class)
            }
            Self::Null(cls) => {
                write!(f, "Null({})", cls)
            }
            Self::Uninitialized => { f.write_str("uninitialized") }
        }
    }
}

impl Clone for SimpleJvmValue {
    fn clone(&self) -> Self {
        match self {
            Self::Integer(v) => Self::Integer(*v),
            Self::Long(v) => Self::Long(*v),
            Self::Float(v) => Self::Float(*v),
            Self::Double(v) => Self::Double(*v),
            Self::Array(v) => Self::Array(v.new_ref()),
            Self::String(v) => Self::String(Rc::clone(v)),
            Self::Object(obj) => Self::Object(obj.new_ref()),
            Self::Null(v) => Self::Null(v.clone()),
            Self::Uninitialized => Self::Uninitialized,
        }
    }
}

impl Hash for SimpleJvmValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Integer(v) => v.hash(state),
            Self::Long(v) => v.hash(state),
            Self::Float(v) => v.to_bits().hash(state),
            Self::Double(v) => v.to_bits().hash(state),
            Self::Array(v) => {
                v.hash(state);
            },
            Self::String(v) => {
                unsafe {
                    let ptr = Rc::into_raw(Rc::clone(v));
                    ptr.hash(state);
                    Rc::from_raw(ptr);
                }
            },
            Self::Object(obj) => {
                obj.hash(state);
            }
            Self::Null(v) => v.hash(state),
            Self::Uninitialized => 4.hash(state),
        }
    }
}

impl Eq for SimpleJvmValue {}

impl PartialEq for SimpleJvmValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(v1), Self::Integer(v2)) => { v1 == v2 },
            (Self::Long(v1), Self::Long(v2)) => { v1 == v2 },
            (Self::Float(v1), Self::Float(v2)) => { v1 == v2 },
            (Self::Double(v1), Self::Double(v2)) => { v1 == v2 },
            (Self::Array(v1), Self::Array(v2)) => { v1 == v2 },
            (Self::Object(obj1), Self::Object(obj2)) => { obj1 == obj2 },
            (Self::String(v1), Self::String(v2)) => { v1 == v2 },
            (Self::Null(v1), Self::Null(v2)) => { v1 == v2 },
            (Self::Uninitialized, _) => false,
            (_, Self::Uninitialized) => false,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct SimpleJvmArray {
    cls: Rc<ClassFile>,
    data: Rc<RefCell<Box<[SimpleJvmValue]>>>
}

impl Hash for SimpleJvmArray {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Rc::as_ptr(&self.cls) as u64).hash(state);
        (Rc::as_ptr(&self.data) as u64).hash(state);
    }
}

impl PartialEq for SimpleJvmArray {
    fn eq(&self, other: &Self) -> bool {
        &*self.cls.this_class == &*other.cls.this_class &&
        &*self.data.borrow() == &*other.data.borrow()
    }
}
impl Eq for SimpleJvmArray {}

impl JvmArray<SimpleJvmValue> for SimpleJvmArray {
    fn len(&self) -> usize {
        self.data.borrow().len()
    }
    fn cls(&self) -> Rc<ClassFile> {
        Rc::clone(&self.cls)
    }
    fn get_elem(&self, idx: usize) -> Option<&SimpleJvmValue> {
        panic!("ref of refcell.. uh oh...");
    }
    fn get_elem_mut(&self, idx: usize) -> Option<&mut SimpleJvmValue> {
        panic!("ref of refcell.. uh oh...");
    }
    unsafe fn as_slice<'data, T>(&'data self) -> Option<&'data [T]> {
        panic!("ref of refcell.. uh oh...");
    }
    fn internal_obj_id(&self) -> u64 {
        self.data.as_ptr() as u64
    }
    fn new_ref(&self) -> Self {
        Self {
            cls: Rc::clone(&self.cls),
            data: Rc::clone(&self.data),
        }
    }
    fn new_array(class_file: Rc<ClassFile>, data: Box<[SimpleJvmValue]>) -> Self {
        Self {
            cls: class_file,
            data: panic!("todo: do something with argument")
        }
    }
}

#[derive(Debug)]
pub struct SimpleJvmObject {
    fields: Rc<RefCell<HashMap<String, SimpleJvmValue>>>,
    cls: Rc<ClassFile>,
}

impl Hash for SimpleJvmObject {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Rc::as_ptr(&self.fields) as u64).hash(state);
        (Rc::as_ptr(&self.cls) as u64).hash(state);
    }
}

impl PartialEq for SimpleJvmObject {
    fn eq(&self, other: &Self) -> bool {
        &self.cls.this_class == &other.cls.this_class &&
        &*self.fields.borrow() == &*other.fields.borrow()
    }
}
impl Eq for SimpleJvmObject {}

impl JvmObject<SimpleJvmValue> for SimpleJvmObject {
    fn new_inst(class_file: Rc<ClassFile>) -> Self {
        panic!("new inst")
    }
    fn cls(&self) -> Rc<ClassFile> {
        panic!("array class");
    }
    fn get_field(&self, field: &str) -> SimpleJvmValue {
        self.fields.borrow().get(field).expect("field exists").to_owned()
    }
    fn set_field(&self, field: &str, v: SimpleJvmValue) {
        self.fields.borrow_mut().insert(field.to_owned(), v);
    }
    fn internal_obj_id(&self) -> u64 {
        self.fields.as_ptr() as u64
    }
    fn new_ref(&self) -> Self {
        Self {
            fields: Rc::clone(&self.fields),
            cls: Rc::clone(&self.cls),
        }
    }
}

impl SimpleJvmObject {
    pub fn new_with_data(class: Rc<ClassFile>, data: HashMap<String, SimpleJvmValue>) -> Self {
        SimpleJvmObject {
            fields: Rc::new(RefCell::new(data)),
            cls: class,
        }
    }
    pub fn create(class: Rc<ClassFile>) -> Self {
        SimpleJvmObject {
            fields: Rc::new(RefCell::new(HashMap::new())),
            cls: class,
        }
    }

    pub fn new_inst(class_file: Rc<ClassFile>) -> SimpleJvmObject {
        let mut fields = HashMap::new();
        // TODO: respect type and access flags of fields
        for field in class_file.fields.iter() {
            fields.insert(
                field.name.clone(),
                SimpleJvmValue::default_of(
                    &field.desc,
                ),
            );
        }
        SimpleJvmObject { fields: Rc::new(RefCell::new(fields)), cls: class_file }
    }
}

impl JvmValue for SimpleJvmValue {
    type ArrayTy = SimpleJvmArray;
    type ObjectTy = SimpleJvmObject;

    fn as_object(&self) -> Option<&Self::ObjectTy> {
        if let Self::Object(obj) = self {
            Some(obj)
        } else {
            None
        }
    }
    fn as_array(&self) -> Option<&Self::ArrayTy> {
        if let Self::Array(elems) = self {
            Some(elems)
        } else {
            None
        }
    }
    fn as_integer(&self) -> Option<&i32> {
        if let Self::Integer(i) = self {
            Some(i)
        } else {
            None
        }
    }
    fn as_long(&self) -> Option<&i64> {
        if let Self::Long(i) = self {
            Some(i)
        } else {
            None
        }
    }
    fn as_float(&self) -> Option<&f32> {
        if let Self::Float(i) = self {
            Some(i)
        } else {
            None
        }
    }
    fn as_double(&self) -> Option<&f64> {
        if let Self::Double(i) = self {
            Some(i)
        } else {
            None
        }
    }
    // `str` is the name of the class this null is an instance of. does that even make sense for
    // jvm semantics? i dunno
    fn as_null(&self) -> Option<&str> {
        if let Self::Null(cls) = self {
            Some(cls)
        } else {
            None
        }
    }
    fn is_uninitialized(&self) -> bool {
        if let Self::Uninitialized = self {
            true
        } else {
            false
        }
    }

    fn object(v: Self::ObjectTy) -> Self {
        Self::Object(v)
    }

    fn array(v: Self::ArrayTy) -> Self {
        Self::Array(v)
    }

    fn integer(v: i32) -> Self {
        Self::Integer(v)
    }

    fn long(v: i64) -> Self {
        Self::Long(v)
    }

    fn float(v: f32) -> Self {
        Self::Float(v)
    }

    fn double(v: f64) -> Self {
        Self::Double(v)
    }

    fn null(s: String) -> Self {
        Self::Null(s)
    }

    fn uninitialized() -> Self {
        Self::Uninitialized
    }
}

pub trait JvmValue: fmt::Debug + Sized + PartialEq + Eq + Hash + Clone {
    type ArrayTy: JvmArray<Self>;
    type ObjectTy: JvmObject<Self>;

    // as_object + a type check of the object
    fn as_type(&self, ty: &str) -> Option<&Self::ObjectTy> {
        if let Some(obj) = self.as_object() {
            if obj.cls().this_class == ty {
                return Some(obj);
            }
        }

        None
    }
    fn as_object(&self) -> Option<&Self::ObjectTy>;
    fn as_array(&self) -> Option<&Self::ArrayTy>;
    fn as_integer(&self) -> Option<&i32>;
    fn as_long(&self) -> Option<&i64>;
    fn as_float(&self) -> Option<&f32>;
    fn as_double(&self) -> Option<&f64>;
    fn as_null(&self) -> Option<&str>;
    fn is_uninitialized(&self) -> bool;
    fn is_reference(&self) -> bool {
        self.as_object().is_some() || self.as_array().is_some() || self.as_null().is_some()
    }

    fn integer(v: i32) -> Self;
    fn long(v: i64) -> Self;
    fn float(v: f32) -> Self;
    fn double(v: f64) -> Self;
    fn object(o: Self::ObjectTy) -> Self;
    fn array(a: Self::ArrayTy) -> Self;
    fn null(cls: String) -> Self;
    fn uninitialized() -> Self;

    // constructs a java.lang.String describing `data`
    fn string(vm: &mut VirtualMachine<Self>, data: &str) -> Self {
        let string_class = vm.resolve_class("java/lang/String").expect("strings exist");
        let mut jvm_data = HashMap::new();
        let mut jvm_chars = Vec::new();
        // TODO: strings are utf, not bytes.
        for c in data.as_bytes().iter() {
            jvm_chars.push(Self::integer(*c as i32));
        }
        jvm_data.insert("value".to_owned(), Self::array_with_data(
            vm.resolve_class("java/lang/Character").expect("chars exist"),
            jvm_chars.into_boxed_slice()
        ));
        Self::object_with_data(string_class, jvm_data)
    }


    fn object_with_data(class_file: Rc<ClassFile>, fields: HashMap<String, Self>) -> Self {
        let mut obj = Self::ObjectTy::new_inst(class_file);
        for (k, v) in fields.into_iter() {
            obj.set_field(&k, v)
        }
        Self::object(obj)
    }

    fn array_with_data(class_file: Rc<ClassFile>, elems: Box<[Self]>) -> Self {
        Self::array(Self::ArrayTy::new_array(class_file, elems))
    }

    fn default_of(s: &str) -> Self {
        match s {
            "J" => Self::long(0),
            "B" | "C" | "S" | "Z" | "I" => Self::integer(0),
            "F" => Self::float(0.0),
            "D" => Self::double(0.0),
            // Lasdf;   reference type
            // [        array
            // [[Lasdf; asdf[][]
            other => Self::null(other.to_string()),
        }
    }

    fn parse_from(s: &str) -> Option<Self> {
        if s == "null" {
            return Some(Self::null("Object".to_string()));
        }

        if let Ok(v) = i64::from_str(s) {
            return Some(Self::integer(v as i32));
        }

        if let Ok(v) = f64::from_str(s) {
            return Some(Self::double(v));
        }

        if s.len() >= 2 && s.starts_with("\"") && s.ends_with("\"") {
            panic!("string");
//            return Some(Self::String(Rc::new(s[1..][..s.len() - 2].bytes().collect())));
        }

        return None;
    }
}

pub trait JvmObject<ValueTy: JvmValue>: fmt::Debug + Sized + PartialEq + Eq + Hash {
    fn new_inst(class_file: Rc<ClassFile>) -> Self;
    fn cls(&self) -> Rc<ClassFile>;
    fn get_field(&self, field: &str) -> ValueTy;
    fn set_field(&self, field: &str, v: ValueTy);
    fn internal_obj_id(&self) -> u64;
    fn new_ref(&self) -> Self;
}

pub trait JvmArray<ValueTy: JvmValue>: fmt::Debug + Sized + PartialEq + Eq + Hash {
    fn new_array(class_file: Rc<ClassFile>, data: Box<[ValueTy]>) -> Self;
    fn cls(&self) -> Rc<ClassFile>;
    fn get_elem(&self, idx: usize) -> Option<&ValueTy>;
    fn get_elem_mut(&self, idx: usize) -> Option<&mut ValueTy>;
    fn len(&self) -> usize;
    fn internal_obj_id(&self) -> u64;
    fn new_ref(&self) -> Self;
    unsafe fn as_slice<'data, T>(&'data self) -> Option<&'data [T]>;
}

pub(crate) struct ValueRef<ValueImpl: JvmValue>(ValueImpl);

impl<ValueImpl: JvmValue> ValueRef<ValueImpl> {
    pub fn of(reference: &ValueImpl) -> Self {
        ValueRef(reference.clone())
    }
}

impl<ValueImpl: JvmValue> Hash for ValueRef<ValueImpl> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<ValueImpl: JvmValue> Eq for ValueRef<ValueImpl> {}

impl<ValueImpl: JvmValue> PartialEq for ValueRef<ValueImpl> {
    fn eq(&self, other: &Self) -> bool {
        &self.0 == &other.0
    }
}

#[allow(dead_code)]
enum NativeObject {
    StringBuilder(Vec<u16>),
    Unknown,
}

#[derive(Debug)]
enum NativeImplKey {
    // name is not included: this is only to describe `<clinit>()V` functions.
    Initializer { cls: String },
    Method { cls: Rc<ClassFile>, declaration: String },
}

impl Eq for NativeImplKey {}
impl PartialEq for NativeImplKey {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NativeImplKey::Initializer { cls }, NativeImplKey::Initializer { cls: other_cls }) => {
                cls == other_cls
            },
            (NativeImplKey::Method { cls, declaration }, NativeImplKey::Method { cls: other_cls, declaration: other_declaration }) => {
                &cls.this_class == &other_cls.this_class &&
                declaration == other_declaration
            }
            _ => {
                false
            }
        }
    }
}

impl Hash for NativeImplKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            NativeImplKey::Initializer { cls } => {
                1.hash(state);
                cls.hash(state);
            }
            NativeImplKey::Method { cls, declaration } => {
                2.hash(state);
                Rc::as_ptr(cls).hash(state);
                declaration.hash(state);
            }
        }
    }
}

impl NativeImplKey {
    fn initializer(s: &str) -> Self {
        NativeImplKey::Initializer { cls: s.to_owned() }
    }

    fn method(cls: &Rc<ClassFile>, decl: String) -> Self {
        NativeImplKey::Method {
            cls: Rc::clone(cls),
            declaration: decl
        }
    }
}

pub struct VirtualMachine<ValueImpl: JvmValue> {
    classes: HashMap<String, Rc<ClassFile>>,
    native_impls: HashMap<NativeImplKey, fn(&mut VMState<ValueImpl>, &mut VirtualMachine<ValueImpl>) -> Result<(), VMError>>,
    class_instances: HashMap<String, ValueImpl>, // they're all objects, but we use the generic ValueImpl here
    static_instances: HashMap<ClassFileRef, HashMap<String, ValueImpl>>,
    native_instances: HashMap<ValueRef<ValueImpl>, RefCell<NativeObject>>, // TODO: should probably be an ObjectImpl?
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

impl<ValueImpl: JvmValue> VirtualMachine<ValueImpl> {
    pub fn new(initial_classpath: Vec<PathBuf>) -> Self {
        VirtualMachine {
            classes: HashMap::new(),
            native_impls: HashMap::new(),
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
        fields: Rc<RefCell<HashMap<String, ValueImpl>>>,
        name: &str,
        ty: &str,
    ) -> Option<ValueImpl> {
        if self.has_instance_field(&instance_class, name) {
            Some(fields.borrow_mut().entry(name.to_string()).or_insert_with(
                || ValueImpl::default_of(ty)
            ).clone())
        } else {
            None
        }
    }

    fn put_instance_field(
        &mut self,
        instance_class: Rc<ClassFile>,
        fields: Rc<RefCell<HashMap<String, ValueImpl>>>,
        name: &str,
        ty: &str,
        value: ValueImpl,
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
    ) -> Option<ValueImpl> {
        let fields = self
            .static_instances
            .entry(ClassFileRef::of(class_ref))
            .or_insert_with(|| HashMap::new());
        if class_ref.has_static_field(name) {
            Some(fields.entry(name.to_string()).or_insert_with(
                || ValueImpl::default_of(ty)
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
        value: ValueImpl,
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

    pub fn get_native_method(&self, cls: Rc<ClassFile>, descriptor: String) -> Option<fn(&mut VMState<ValueImpl>, &mut VirtualMachine<ValueImpl>) -> Result<(), VMError>> {
        if descriptor == "<clinit>()V" {
            // ok this arm probably isn't reachable huh, initializers are called before there's a
            // registered `Rc<ClassFile>` to be looking up `this_class` on ...
            eprintln!("get_native_method for clinit? seems unlikely");
            self.native_impls.get(&NativeImplKey::initializer(&cls.this_class.to_string())).cloned()
        } else {
            self.native_impls.get(&NativeImplKey::method(&cls, descriptor)).cloned()
        }
    }

    pub fn resolve_class(&mut self, class_name: &str) -> Result<Rc<ClassFile>, VMError> {
//        eprintln!("resolve class: {}", referent);
        if let Some(cls) = self.classes.get(class_name) {
            return Ok(Rc::clone(cls));
        }

        let (new_class, patches) = if let Some((new_class, patches)) = jvm::synthetic::build_synthetic_class(class_name) {
            (new_class, patches)
        } else {
            use std::collections::hash_map::Entry;
            use std::fs::File;
            let mut res: Option<(ClassFile, HashMap<(String, String), crate::virtual_machine::jvm::synthetic::NativeJvmFn<ValueImpl>>)> = None;
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
            if let Some((cls, patches)) = res {
                (cls, patches)
            } else {
                return Err(VMError::BadClass("could not resolve class"));
            }
        };

        self.register(class_name.to_string(), new_class, patches)
    }

    pub fn register(
        &mut self,
        class_name: String,
        class_file: ClassFile,
        native_methods: HashMap<(String, String), crate::virtual_machine::jvm::synthetic::NativeJvmFn<ValueImpl>>,
    ) -> Result<Rc<ClassFile>, VMError> {
        eprintln!("registering class {}", class_name);
        let rc = Rc::new(class_file);
        self.classes.insert(class_name.clone(), Rc::clone(&rc));

        for ((method, sig), impl_fn) in native_methods.into_iter() {
            if method == "<clinit>" && sig == "()V" {
                self.native_impls.insert(NativeImplKey::initializer(&class_name), impl_fn);
            } else {
                self.native_impls.insert(NativeImplKey::method(&Rc::clone(&rc), format!("{}{}", method, sig)), impl_fn);
            }
        }

        if let Some(native_method) = self.native_impls.get(&NativeImplKey::initializer(&class_name)) {
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
                let native_method = self.native_impls.get(&NativeImplKey::initializer(&class_name)).expect("native clinit has native method impl");
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
        args: Vec<ValueImpl>,
    ) -> Result<Option<ValueImpl>, VMError> {
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

    fn interpret(&mut self, state: &mut VMState<ValueImpl>) -> Result<Option<ValueImpl>, VMError> {
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
                                if let Some(obj) = value.as_object() {
                                    if self.is_exception(&obj.cls(), exception_record.catch_type.as_str()) {
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
                        if let Some(obj) = value.as_object() {
                            if let Some(msg) = obj.get_field("message").as_type("java/lang/String") {
                                let field = msg.get_field("value");
                                let msg = field.as_array().expect("TODO: is array");
                                let msg = unsafe { msg.as_slice::<u8>() }.expect("TODO: can cast");
                                eprintln!("unhandled {}: {}", obj.cls().this_class.as_str(), String::from_utf8_lossy(msg));
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

fn interpreted_method_call<ValueImpl: JvmValue>(
    state: &mut VMState<ValueImpl>,
    _vm: &mut VirtualMachine<ValueImpl>,
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
