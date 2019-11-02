use std::rc::Rc;

use crate::class_file::ClassFile;

pub struct VirtualMachine {
    classes: Vec<Rc<ClassFile>>,
}

impl VirtualMachine {
    pub fn new() -> Self {
        VirtualMachine {
            classes: Vec::new(),
        }
    }

    pub fn load(&mut self, class_file: ClassFile) -> Rc<ClassFile> {
        let rc = Rc::new(class_file);
        self.classes.push(Rc::clone(&rc));
        rc
    }

    pub fn get_method(&self, class_ref: &Rc<ClassFile>, method: &str) -> Result<(), ()> {
        Ok(())
    }

    pub fn execute(&mut self, method: ()) {
        println!("zoom zoom");
    }
}
