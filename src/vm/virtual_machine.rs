use std::collections::HashMap;
use std::cell::RefCell;
use std::io::Cursor;
use std::rc::Rc;
use std::str::FromStr;

use crate::class_file::attribute::Attribute;
use crate::class_file::instruction::Instruction;
use crate::class_file::read::FromReader;
use crate::class_file::AccessFlags;
use crate::class_file::ClassFile;
use crate::class_file::ClassFileRef;
use crate::class_file::Constant;
use crate::class_file::ConstantIdx;
use crate::class_file::FieldAccessFlags;
use crate::class_file::FieldInfo;
use crate::class_file::MethodAccessFlags;
use crate::class_file::MethodHandle;
use crate::class_file::MethodInfo;

struct CallFrame {
    offset: u32,
    arguments: Vec<Rc<RefCell<Value>>>,
    body: Rc<Attribute>,
    enclosing_class: Rc<ClassFile>,
    operand_stack: Vec<Rc<RefCell<Value>>>,
}

impl CallFrame {
    pub fn new(
        body: Rc<Attribute>,
        enclosing_class: Rc<ClassFile>,
        mut arguments: Vec<Rc<RefCell<Value>>>,
    ) -> Self {
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
}

impl VMState {
    pub fn new(
        code: Rc<Attribute>,
        method_class: Rc<ClassFile>,
        initial_args: Vec<Rc<RefCell<Value>>>,
    ) -> Self {
        let mut state = VMState {
            call_stack: Vec::new(),
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

    pub fn enter(
        &mut self,
        body: Rc<Attribute>,
        enclosing_class: Rc<ClassFile>,
        arguments: Vec<Rc<RefCell<Value>>>,
    ) {
        self.call_stack
            .push(CallFrame::new(body, enclosing_class, arguments));
    }

    pub fn leave(&mut self) {
        self.call_stack.pop().expect("stack is non-empty");
    }

    fn interpret_iload(&mut self, idx: u16) -> Result<Option<Rc<RefCell<Value>>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Rc<RefCell<Value>>> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => match &*argument.borrow() {
                Value::Integer(_) => Rc::clone(argument),
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

    fn interpret_istore(&mut self, idx: u16) -> Result<Option<Rc<RefCell<Value>>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let value = frame_mut.operand_stack.pop().expect("operand stack has value");
        frame_mut.arguments[idx as usize] = Rc::clone(&value);

        Ok(None)
    }

    fn interpret_lload(&mut self, idx: u16) -> Result<Option<Rc<RefCell<Value>>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Rc<RefCell<Value>>> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => match &*argument.borrow() {
                Value::Long(_) => Rc::clone(argument),
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

    fn interpret_fload(&mut self, idx: u16) -> Result<Option<Rc<RefCell<Value>>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Rc<RefCell<Value>>> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => match &*argument.borrow() {
                Value::Float(_) => Rc::clone(argument),
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

    fn interpret_dload(&mut self, idx: u16) -> Result<Option<Rc<RefCell<Value>>>, VMError> {
        let frame_mut = self.current_frame_mut();
        let argument: Option<&Rc<RefCell<Value>>> = frame_mut.arguments.get(idx as usize);
        let operand = match argument {
            Some(argument) => match &*argument.borrow() {
                Value::Double(_) => Rc::clone(argument),
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

    fn execute(
        &mut self,
        instruction: &Instruction,
        vm: &mut VirtualMachine,
    ) -> Result<Option<Rc<RefCell<Value>>>, VMError> {
        match instruction {
            Instruction::InvokeVirtual(idx) => {
                if let Some(Constant::Methodref(class_idx, name_and_type_idx)) =
                    self.current_frame().enclosing_class.get_const(*idx)
                {
                    let method_class = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*class_idx)
                        .unwrap();
                    let method_class_name = if let Constant::Class(class_name_idx) = method_class {
                        self.current_frame()
                            .enclosing_class
                            .get_str(*class_name_idx)
                            .unwrap()
                    } else {
                        panic!("method's class is not a class?");
                    };
                    let target_class = vm.resolve_class(method_class_name).unwrap();
                    if let Some(Constant::NameAndType(name_idx, type_idx)) = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*name_and_type_idx)
                    {
                        let method_name = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*name_idx)
                            .unwrap()
                            .to_string();
                        let method_type = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*type_idx)
                            .unwrap()
                            .to_string();
                        let method = target_class
                            .get_method(&method_name, &method_type)
                            .expect("method exists");
                        // get method by name `method_name`
                        if method.access_flags.is_native() {
                            if let Some(native_method) =
                                target_class.native_methods.get(&method_name)
                            {
                                native_method(self, vm)?;
                                Ok(None)
                            } else {
                                panic!("attempted to call native method with no implementation: {} - note, JNI resolution is not yet supported.", method_name);
                            }
                        } else {
                            interpreted_method_call(self, vm, method, target_class, &method_type)?;
                            Ok(None)
                        }
                    } else {
                        Err(VMError::BadClass(
                            "fieldref name_and_type does not index a NameAndType",
                        ))
                    }
                } else {
                    Err(VMError::BadClass(
                        "invokestatic constant pool idx does not index a Methodref",
                    ))
                }
            }
            Instruction::InvokeStatic(idx) => {
                if let Some(Constant::Methodref(class_idx, name_and_type_idx)) =
                    self.current_frame().enclosing_class.get_const(*idx)
                {
                    let method_class = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*class_idx)
                        .unwrap();
                    let method_class_name = if let Constant::Class(class_name_idx) = method_class {
                        self.current_frame()
                            .enclosing_class
                            .get_str(*class_name_idx)
                            .unwrap()
                    } else {
                        panic!("method's class is not a class?");
                    };
                    let target_class = vm.resolve_class(method_class_name).unwrap();
                    if let Some(Constant::NameAndType(name_idx, type_idx)) = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*name_and_type_idx)
                    {
                        let method_name = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*name_idx)
                            .unwrap()
                            .to_string();
                        let method_type = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*type_idx)
                            .unwrap()
                            .to_string();
                        let method = target_class
                            .get_method(&method_name, &method_type)
                            .expect("method exists");
                        // get method by name `method_name`
                        if method.access_flags.is_native() {
                            if let Some(native_method) =
                                target_class.native_methods.get(&method_name)
                            {
                                native_method(self, vm)?;
                                Ok(None)
                            } else {
                                panic!("attempted to call native method with no implementation: {} - note, JNI resolution is not yet supported.", method_name);
                            }
                        } else {
                            interpreted_method_call(self, vm, method, target_class, &method_type)?;
                            Ok(None)
                        }
                    } else {
                        Err(VMError::BadClass(
                            "fieldref name_and_type does not index a NameAndType",
                        ))
                    }
                } else {
                    Err(VMError::BadClass(
                        "getstatic constant pool idx does not index a Fieldref",
                    ))
                }
            }
            Instruction::GetStatic(idx) => {
                if let Some(Constant::Fieldref(class_idx, name_and_type_idx)) =
                    self.current_frame().enclosing_class.get_const(*idx)
                {
                    let referent_class = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*class_idx)
                        .unwrap();
                    let referent_class_name =
                        if let Constant::Class(class_name_idx) = referent_class {
                            self.current_frame()
                                .enclosing_class
                                .get_str(*class_name_idx)
                                .unwrap()
                        } else {
                            panic!("referent class is not a class?");
                        };
                    let target_class = vm.resolve_class(referent_class_name).unwrap();
                    if let Some(Constant::NameAndType(name_idx, type_idx)) = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*name_and_type_idx)
                    {
                        let referent_name = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*name_idx)
                            .unwrap()
                            .to_string();
                        let referent_type = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*type_idx)
                            .unwrap()
                            .to_string();
                        let value = vm
                            .get_static_field(&target_class, &referent_name, &referent_type)
                            .unwrap();
                        self.current_frame_mut().operand_stack.push(value);
                        Ok(None)
                    } else {
                        Err(VMError::BadClass(
                            "fieldref name_and_type does not index a NameAndType",
                        ))
                    }
                } else {
                    Err(VMError::BadClass(
                        "getstatic constant pool idx does not index a Fieldref",
                    ))
                }
            }
            Instruction::PutStatic(idx) => {
                if let Some(Constant::Fieldref(class_idx, name_and_type_idx)) =
                    self.current_frame().enclosing_class.get_const(*idx)
                {
                    let referent_class = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*class_idx)
                        .unwrap();
                    let referent_class_name =
                        if let Constant::Class(class_name_idx) = referent_class {
                            self.current_frame()
                                .enclosing_class
                                .get_str(*class_name_idx)
                                .unwrap()
                        } else {
                            panic!("referent class is not a class?");
                        };
                    let target_class = vm.resolve_class(referent_class_name).unwrap();
                    if let Some(Constant::NameAndType(name_idx, type_idx)) = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*name_and_type_idx)
                    {
                        let referent_name = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*name_idx)
                            .unwrap()
                            .to_string();
                        let referent_type = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*type_idx)
                            .unwrap()
                            .to_string();

                        let value = if let Some(value) = self.current_frame_mut().operand_stack.pop() {
                            value
                        } else {
                            return Err(VMError::BadClass("iadd but insufficient arguments"));
                        };

                        vm
                            .put_static_field(&target_class, &referent_name, &referent_type, value);
                        Ok(None)
                    } else {
                        Err(VMError::BadClass(
                            "fieldref name_and_type does not index a NameAndType",
                        ))
                    }
                } else {
                    Err(VMError::BadClass(
                        "getstatic constant pool idx does not index a Fieldref",
                    ))
                }
            }
            Instruction::InvokeSpecial(idx) => {
                if let Some(Constant::Methodref(class_idx, name_and_type_idx)) =
                    self.current_frame().enclosing_class.get_const(*idx)
                {
                    let method_class = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*class_idx)
                        .unwrap();
                    let method_class_name = if let Constant::Class(class_name_idx) = method_class {
                        self.current_frame()
                            .enclosing_class
                            .get_str(*class_name_idx)
                            .unwrap()
                    } else {
                        panic!("method's class is not a class?");
                    };
                    let target_class = vm.resolve_class(method_class_name).unwrap();
                    if let Some(Constant::NameAndType(name_idx, type_idx)) = self
                        .current_frame()
                        .enclosing_class
                        .get_const(*name_and_type_idx)
                    {
                        let method_name = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*name_idx)
                            .unwrap()
                            .to_string();
                        let method_type = self
                            .current_frame()
                            .enclosing_class
                            .get_str(*type_idx)
                            .unwrap()
                            .to_string();
                        let method = target_class
                            .get_method(&method_name, &method_type)
                            .expect("method exists");
                        // get method by name `method_name`
                        if method.access_flags.is_native() {
                            if let Some(native_method) =
                                target_class.native_methods.get(&method_name)
                            {
                                native_method(self, vm)?;
                                Ok(None)
                            } else {
                                panic!("attempted to call native method with no implementation: {} - note, JNI resolution is not yet supported.", method_name);
                            }
                        } else {
                            interpreted_method_call(self, vm, method, target_class, &method_type)?;
                            Ok(None)
                        }
                    } else {
                        Err(VMError::BadClass(
                            "fieldref name_and_type does not index a NameAndType",
                        ))
                    }
                } else {
                    Err(VMError::BadClass(
                        "getstatic constant pool idx does not index a Fieldref",
                    ))
                }
            }
            Instruction::NewArray(tpe) => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                let mut elems = Vec::new();
                if let Value::Integer(size) = &*top.borrow() {
                    for _ in 0..*size {
                        elems.push(Rc::new(RefCell::new(Value::Integer(0))));
                    }
                }

                self.current_frame_mut().operand_stack.push(Rc::new(RefCell::new(Value::Array(elems.into_boxed_slice()))));
                Ok(None)
            }
            Instruction::New(idx) => {
                if let Some(Constant::Class(class_idx)) = self.current_frame().enclosing_class.get_const(*idx)
                {
                    let class_name = self
                        .current_frame()
                        .enclosing_class
                        .get_str(*class_idx)
                        .unwrap()
                        .to_string();

                    self.current_frame_mut().operand_stack.push(Rc::new(RefCell::new(Value::Null(class_name))));
                    Ok(None)
                } else {
                    Err(VMError::BadClass(
                        "new constant pool idx does not index a Class"
                    ))
                }
            }
            Instruction::BIPush(b) => {
                self.current_frame_mut().operand_stack.push(Rc::new(RefCell::new(Value::Integer(*b as i32))));
                Ok(None)
            }
            Instruction::SIPush(s) => {
                self.current_frame_mut().operand_stack.push(Rc::new(RefCell::new(Value::Integer(*s as i32))));
                Ok(None)
            }
            Instruction::Dup => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                self.current_frame_mut().operand_stack.push(Rc::clone(&top));
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

                self.current_frame_mut().operand_stack.push(Rc::clone(&next));
                self.current_frame_mut().operand_stack.push(Rc::clone(&top));
                self.current_frame_mut().operand_stack.push(next);
                self.current_frame_mut().operand_stack.push(top);
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

                if let (Value::Array(elements), Value::Integer(index)) = (&*array.borrow(), &*index.borrow()) {
                    // TODO: homogeneously typed arrays
                    if let Some(value) = elements.get(*index as usize) {
                        self.current_frame_mut().operand_stack.push(Rc::clone(value));
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

                if let (Value::Array(elements), Value::Integer(index)) = (&mut *array.borrow_mut(), &*index.borrow()) {
                    // TODO: homogeneously typed arrays
                    elements[*index as usize] = value;
                } else {
                    panic!("getting element out of non-array");
                }
                Ok(None)
            }
            Instruction::ArrayLength => {
                let top = self
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("stack has a value");

                // return length?
                self.current_frame_mut().operand_stack.push(Rc::new(RefCell::new(Value::Integer(10))));
                Ok(None)
            }
            Instruction::LConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut.operand_stack.push(Rc::new(RefCell::new(Value::Long(0))));
                Ok(None)
            }
            Instruction::LConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut.operand_stack.push(Rc::new(RefCell::new(Value::Long(1))));
                Ok(None)
            }
            Instruction::IConst0 => {
                let frame_mut = self.current_frame_mut();
                frame_mut.operand_stack.push(Rc::new(RefCell::new(Value::Integer(0))));
                Ok(None)
            }
            Instruction::IConst1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut.operand_stack.push(Rc::new(RefCell::new(Value::Integer(1))));
                Ok(None)
            }
            Instruction::IConst2 => {
                let frame_mut = self.current_frame_mut();
                frame_mut.operand_stack.push(Rc::new(RefCell::new(Value::Integer(2))));
                Ok(None)
            }
            Instruction::IConst3 => {
                let frame_mut = self.current_frame_mut();
                frame_mut.operand_stack.push(Rc::new(RefCell::new(Value::Integer(3))));
                Ok(None)
            }
            Instruction::IConst4 => {
                let frame_mut = self.current_frame_mut();
                frame_mut.operand_stack.push(Rc::new(RefCell::new(Value::Integer(4))));
                Ok(None)
            }
            Instruction::IConst5 => {
                let frame_mut = self.current_frame_mut();
                frame_mut.operand_stack.push(Rc::new(RefCell::new(Value::Integer(5))));
                Ok(None)
            }
            Instruction::IConstM1 => {
                let frame_mut = self.current_frame_mut();
                frame_mut.operand_stack.push(Rc::new(RefCell::new(Value::Integer(-1))));
                Ok(None)
            }
            Instruction::IfGe(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match &*Rc::clone(&value).borrow() {
                    Value::Integer(v) => {
                        if *v >= 0 {
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
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match &*Rc::clone(&value).borrow() {
                    Value::Integer(v) => {
                        if *v <= 0 {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IfNe(offset) => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("iadd but insufficient arguments"));
                };

                match &*Rc::clone(&value).borrow() {
                    Value::Integer(v) => {
                        if *v != 0 {
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

                match (&*Rc::clone(&left).borrow(), &*Rc::clone(&right).borrow()) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        if *l != *r {
                            frame_mut.offset += *offset as i32 as u32 - 3;
                            Ok(None)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::ILoad0 => self.interpret_iload(0),
            Instruction::ILoad1 => self.interpret_iload(1),
            Instruction::ILoad2 => self.interpret_iload(2),
            Instruction::ILoad3 => self.interpret_iload(3),
            Instruction::ILoad(idx) => self.interpret_iload(*idx),
            Instruction::LLoad0 => self.interpret_lload(0),
            Instruction::LLoad1 => self.interpret_lload(1),
            Instruction::LLoad2 => self.interpret_lload(2),
            Instruction::LLoad3 => self.interpret_lload(3),
            Instruction::LLoad(idx) => self.interpret_lload(*idx),
            Instruction::FLoad0 => self.interpret_fload(0),
            Instruction::FLoad1 => self.interpret_fload(1),
            Instruction::FLoad2 => self.interpret_fload(2),
            Instruction::FLoad3 => self.interpret_fload(3),
            Instruction::FLoad(idx) => self.interpret_fload(*idx),
            Instruction::DLoad0 => self.interpret_dload(0),
            Instruction::DLoad1 => self.interpret_dload(1),
            Instruction::DLoad2 => self.interpret_dload(2),
            Instruction::DLoad3 => self.interpret_dload(3),
            Instruction::DLoad(idx) => self.interpret_dload(*idx),
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

                match (&*Rc::clone(&left).borrow(), &*Rc::clone(&right).borrow()) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Rc::new(RefCell::new(Value::Integer(l.wrapping_add(*r)))));
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

                match (&*Rc::clone(&left).borrow(), &*Rc::clone(&right).borrow()) {
                    (Value::Long(l), Value::Long(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Rc::new(RefCell::new(Value::Long(l.wrapping_add(*r)))));
                        Ok(None)
                    }
                    _ => {
                        Err(VMError::BadClass("ladd but invalid operand types"))
                    }
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

                match (&*Rc::clone(&left).borrow(), &*Rc::clone(&right).borrow()) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        frame_mut
                            .operand_stack
                            .push(Rc::new(RefCell::new(Value::Integer(l.wrapping_sub(*r)))));
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

                match &*Rc::clone(&value).borrow() {
                    Value::Integer(l) => {
                        frame_mut
                            .operand_stack
                            .push(Rc::new(RefCell::new(Value::Integer((*l) as i8 as i32))));
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

                match &*Rc::clone(&value).borrow() {
                    Value::Integer(l) => {
                        frame_mut
                            .operand_stack
                            .push(Rc::new(RefCell::new(Value::Integer((*l) as i16 as u16 as i32))));
                        Ok(None)
                    }
                    _ => Err(VMError::BadClass("iadd but invalid operand types")),
                }
            }
            Instruction::IReturn => {
                let frame_mut = self.current_frame_mut();
                let value = if let Some(value) = frame_mut.operand_stack.pop() {
                    value
                } else {
                    return Err(VMError::BadClass("ireturn but insufficient arguments"));
                };

                match &*Rc::clone(&value).borrow() {
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
                    return Err(VMError::BadClass("ireturn but insufficient arguments"));
                };

                match &*Rc::clone(&value).borrow() {
                    Value::Array(_) => {
                        self.leave();
                        Ok(Some(value))
                    }
                    _ => Err(VMError::BadClass("ireturn but invalid operand types")),
                }
            }
            Instruction::Ldc(idx) => {
                let value = match self.current_frame().enclosing_class.get_const(*idx) {
                    Some(Constant::Integer(i)) => Rc::new(RefCell::new(Value::Integer(*i as i32))),
                    Some(Constant::Long(l)) => Rc::new(RefCell::new(Value::Long(*l as i64))),
                    Some(Constant::String(idx)) => {
                        if let Some(Constant::Utf8(data)) =
                            self.current_frame().enclosing_class.get_const(*idx)
                        {
                            Rc::new(RefCell::new(Value::String(data.clone())))
                        } else {
                            return Err(VMError::BadClass("string ref is not utf8 data"));
                        }
                    }
                    _ => {
                        return Err(VMError::Unsupported("unsupported constant type for ldc"));
                    }
                };

                self.current_frame_mut().operand_stack.push(value);
                Ok(None)
            }
            Instruction::Return => {
                self.leave();
                Ok(None)
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
    Array(Box<[Rc<RefCell<Value>>]>),
    String(Vec<u8>),
    Null(String), // Null, of type `String`
}

impl Value {
    pub fn default_of(s: &str) -> Value {
        match s {
            "J" => {
                Value::Long(0)
            }
            "B" | "C" | "S" | "Z" | "I" => {
                Value::Integer(0)
            },
            "F" => {
                Value::Float(0.0)
            },
            "D" => {
                Value::Double(0.0)
            },
            // Lasdf;   reference type
            // [        array
            // [[Lasdf; asdf[][]
            other => {
                Value::Null(other.to_string())
            }
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
            return Some(Value::String(s[1..][..s.len() - 2].bytes().collect()));
        }

        return None;
    }
}

pub struct VirtualMachine {
    classes: HashMap<String, Rc<ClassFile>>,
    static_instances: HashMap<ClassFileRef, HashMap<String, Rc<RefCell<Value>>>>,
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
            classes: HashMap::new(),
            static_instances: HashMap::new(),
        }
    }

    fn get_static_field(&mut self, class_ref: &Rc<ClassFile>, name: &str, ty: &str) -> Option<Rc<RefCell<Value>>> {
        let fields = self
            .static_instances
            .entry(ClassFileRef::of(class_ref))
            .or_insert_with(|| HashMap::new());
        if class_ref.has_static_field(name) {
            Some(Rc::clone(
                fields
                    .entry(name.to_string())
                    .or_insert_with(|| Rc::new(RefCell::new(Value::default_of(ty)))),
            ))
        } else {
            None
        }
    }

    fn put_static_field(&mut self, class_ref: &Rc<ClassFile>, name: &str, ty: &str, value: Rc<RefCell<Value>>) {
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
        match referent {
            "java/lang/System" => {
                let constants = vec![
                    Constant::Utf8(b"java/lang/System".to_vec()),
                    Constant::Utf8(b"out".to_vec()),
                    Constant::Utf8(b"();".to_vec()),
                ];

                let synthetic_class = ClassFile {
                    major_version: 55,
                    minor_version: 0,
                    constant_pool: constants,
                    access_flags: AccessFlags { flags: 0x0001 },
                    this_class: ConstantIdx::new(1).unwrap(),
                    super_class: None,
                    interfaces: Vec::new(),
                    fields: vec![FieldInfo {
                        access_flags: FieldAccessFlags { flags: 0x0001 },
                        name_index: ConstantIdx::new(2).unwrap(),
                        descriptor_index: ConstantIdx::new(3).unwrap(),
                        attributes: Vec::new(),
                    }],
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

                let mut native_methods: HashMap<
                    String,
                    fn(&mut VMState, &mut VirtualMachine) -> Result<(), VMError>,
                > = HashMap::new();
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
                    methods: vec![MethodInfo {
                        access_flags: MethodAccessFlags { flags: 0x0101 },
                        name_index: ConstantIdx::new(2).unwrap(),
                        descriptor_index: ConstantIdx::new(3).unwrap(),
                        attributes: Vec::new(),
                    }],
                    attributes: vec![],
                    native_methods,
                };

                Ok(Rc::new(synthetic_class))
            }
            class_name => {
                //                println!("Looking up class {}", class_name);
                match self.classes.get(class_name) {
                    Some(class_ref) => Ok(Rc::clone(class_ref)),
                    None => Err(VMError::BadClass("unknown class, cannot dynamically ")),
                }
            }
        }
    }

    pub fn register(
        &mut self,
        class_name: String,
        class_file: ClassFile,
    ) -> Result<Rc<ClassFile>, VMError> {
        //        println!("Registering class {}", class_name);;
        let rc = Rc::new(class_file);
        self.classes.insert(class_name, Rc::clone(&rc));
        Ok(rc)
    }

    pub fn get_method(
        &self,
        class_ref: &Rc<ClassFile>,
        method: &str,
        desc: &str,
    ) -> Result<Rc<MethodHandle>, VMError> {
        class_ref
            .get_method(method, desc)
            .map_err(|_| VMError::NameResolutionError)
    }

    pub fn get_methods(
        &self,
        class_ref: &Rc<ClassFile>,
        method: &str,
    ) -> Result<Vec<Rc<MethodHandle>>, VMError> {
        class_ref
            .get_methods(method)
            .map_err(|_| VMError::NameResolutionError)
    }

    pub fn execute(
        &mut self,
        method: Rc<MethodHandle>,
        class_ref: &Rc<ClassFile>,
        args: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Option<Rc<RefCell<Value>>>, VMError> {
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

        let code = method.body().ok_or(VMError::AccessError(
            "attempted to initiate VM with function that has no body",
        ))?;

        // TODO: verify arguments? verify that `method` does not take arguments??

        let mut state = VMState::new(code, Rc::clone(class_ref), args);
        self.interpret(&mut state)
    }

    fn interpret(&mut self, state: &mut VMState) -> Result<Option<Rc<RefCell<Value>>>, VMError> {
        // magic incantation to awaken the machine
        println!("zoom zoom");

        while let Some(instruction) = state.next_instruction() {
            //            println!("Executing {:?}", instruction);
            //            let enc = &*state.current_frame().enclosing_class;
            //            println!("Executing {}", instruction.display(enc));
            if let Some(value) = state.execute(&instruction, self)? {
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
) -> Result<(), VMError> {
    // TODO: parse out arguments from method type, check against available operands, do the call
    //
    // today: [...], do the call

    if method_type == "(I)I" {
        let frame = state.current_frame_mut();
        let arg = frame.operand_stack.pop().expect("argument is present");
        state.enter(
            method.body().expect("method has a body"),
            method_class,
            vec![arg],
        );
    } else {
        state.enter(
            method.body().expect("method has a body"),
            method_class,
            vec![],
        );
    }
    Ok(())
}

fn system_out_println(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let Value::String(data) = &*argument.borrow() {
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
