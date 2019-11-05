use std::rc::Rc;
use std::io::Cursor;
use std::collections::HashMap;

use crate::class_file::ClassFile;
use crate::class_file::MethodHandle;
use crate::class_file::read::FromReader;
use crate::class_file::attribute::Attribute;
use crate::class_file::instruction::Instruction;

struct CallFrame {
    offset: u32,
    body: Rc<Attribute>,
    enclosing_class: Rc<ClassFile>,
    operand_stack: Vec<Value>,
}

impl CallFrame {
    pub fn new(body: Rc<Attribute>, enclosing_class: Rc<ClassFile>) -> Self {
        CallFrame {
            offset:0 ,
            body,
            enclosing_class,
            operand_stack: Vec::new()
        }
    }
}

pub struct VMState {
    // Attribute is actually a Code (anything else is an error)
    call_stack: Vec<CallFrame>,
}

impl VMState {
    pub fn new(code: Rc<Attribute>, method: Rc<ClassFile>) -> Self {
        let mut state = VMState {
            call_stack: Vec::new()
        };
        state.call_stack.push(CallFrame::new(code, method));
        state
    }

    fn current_frame(&self) -> &CallFrame {
        &self.call_stack.iter().rev().next().unwrap()
    }

    fn next_instruction(&self) -> Option<Instruction> {
        let frame = self.current_frame();
        let attr_ref: &Attribute = &*frame.body;
        if let Attribute::Code(_, _, code, _, _) = attr_ref {
            let mut instr_bytes = Cursor::new(code.as_slice());

            while (instr_bytes.position() as u32) < frame.offset {
                Instruction::read_from(&mut instr_bytes).unwrap();
            }

            Instruction::read_from(&mut instr_bytes).ok()
        } else {
            panic!("call stack has a call record that does not have code");
        }
    }

    fn execute(&mut self, instruction: &Instruction, vm: &VirtualMachine) -> Result<(), VMError> {
        println!("Executing {}", instruction.display(&*self.current_frame().enclosing_class));
        unimplemented!();
    }

    fn return_value(&mut self) -> Option<Value> {
        panic!("Hello there");
        None
    }
}

pub enum Value {
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
}

impl VirtualMachine {
    pub fn new() -> Self {
        VirtualMachine {
            classes: HashMap::new()
        }
    }

    pub fn register(&mut self, class_name: String, class_file: ClassFile) -> Result<Rc<ClassFile>, VMError> {
        let rc = Rc::new(class_file);
        self.classes.insert(class_name, Rc::clone(&rc));
        Ok(rc)
    }

    pub fn get_method(&self, class_ref: &Rc<ClassFile>, method: &str) -> Result<Rc<MethodHandle>, VMError> {
        class_ref.get_method(method).map_err(|_| VMError::NameResolutionError)
    }

    pub fn execute(&mut self, method: Rc<MethodHandle>, class_ref: &Rc<ClassFile>) -> Result<Option<Value>, VMError> {
        if !method.access().is_static() {
            return Err(VMError::AccessError("attempted to call an instance method without an instance"));
        }

        if !method.access().is_public() {
            return Err(VMError::AccessError("attempted to initiate VM with non-public function"))
        }

        let code = method.body().ok_or(VMError::AccessError("attempted to initiate VM with function that has no body"))?;

        // TODO: verify arguments? verify that `method` does not take arguments??

        let mut state = VMState::new(code, Rc::clone(class_ref));
        self.interpret(&mut state)
    }

    fn interpret(&mut self, state: &mut VMState) -> Result<Option<Value>, VMError> {
        // magic incantation to awaken the machine
        println!("zoom zoom");

        while let Some(instruction) = state.next_instruction() {
            state.execute(&instruction, self)?;
        }

        Ok(state.return_value())
    }
}
