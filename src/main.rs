pub mod class_file;
pub mod vm;

use crate::class_file::read;
use crate::vm::Value;
use crate::vm::VirtualMachine;

use std::fs::File;
use std::rc::Rc;
use std::path::Path;

fn main() {
    let env = std::env::args().collect::<Vec<_>>();
    let filename = &env[1];
    let methodname = &env[2];

    let values = parse_args(&env[3..]).unwrap();

    let class_file = read::class_header(
        &mut File::open(filename).unwrap_or_else(|_| panic!("No such file {}", filename)),
    )
    .unwrap();
    let mut vm = VirtualMachine::new();
    let class_file_path = Path::new(filename);
    let class_name = class_file_path.file_stem().unwrap().to_str().unwrap();
    let class_ref = vm.register(class_name.to_string(), class_file).unwrap();
    let entrypoint = vm.get_method(&class_ref, methodname).unwrap();
    vm.execute(entrypoint, &class_ref, values).unwrap();
}

// the string is because i don't want to think right now
fn parse_args(env: &[String]) -> Result<Vec<Rc<Value>>, String> {
    let mut result = Vec::new();

    for s in env {
        if let Some(value) = Value::parse_from(s) {
            result.push(Rc::new(value))
        } else {
            return Err(format!("unable to parse {} as a value", s));
        }
    }

    Ok(result)
}
