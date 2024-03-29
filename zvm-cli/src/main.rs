use zvm::class_file::unvalidated::read;
use zvm::class_file::validated::ClassFile;
use zvm::{JvmValue, SimpleJvmValue, VirtualMachine};

use std::collections::HashMap;
use std::fs::File;
use std::path::Path;
use std::sync::Arc;

fn main() {
    let env = std::env::args().collect::<Vec<_>>();
    let filename = &env[1];
    let methodname = &env[2];

    let values = parse_args(&env[3..]).unwrap();

    let class_file = ClassFile::validate(
        &read::class_header(
            &mut File::open(filename).unwrap_or_else(|_| panic!("No such file {}", filename)),
        )
        .unwrap()
    )
    .unwrap();
    let mut classes_dir = Path::new(filename).to_path_buf();
    classes_dir.pop();
    let classpath = vec![classes_dir, Path::new("../build/jvm/java.base/").to_path_buf(), Path::new(".").to_path_buf()];
    let mut vm = VirtualMachine::new(classpath);
    let class_file_path = Path::new(filename);
    let class_name = class_file_path.file_stem().unwrap().to_str().unwrap();
    let class_ref = vm.register(class_name.to_string(), class_file, HashMap::new()).unwrap();
    let entrypoint_methods = class_ref.get_methods(methodname);
    if entrypoint_methods.len() != 1 {
        println!(
            "invalid number of methods matching entrypoint name: {:?}",
            entrypoint_methods.len()
        );
        return;
    }
    let entrypoint = Arc::clone(&entrypoint_methods[0]);
    if let Some(value) = vm.execute(entrypoint, &class_ref, values).unwrap() {
        println!("> {:?}", value);
    } else {
        // normal completion
    }
}

// the string is because i don't want to think right now
fn parse_args(env: &[String]) -> Result<Vec<SimpleJvmValue>, String> {
    let mut result = Vec::new();

    for s in env {
        if let Some(value) = SimpleJvmValue::parse_from(s) {
            result.push(value);
        } else {
            return Err(format!("unable to parse {} as a value", s));
        }
    }

    Ok(result)
}
