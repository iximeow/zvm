use zvm::class_file::unvalidated::read;

use std::fs::File;

pub fn main() {
    let env = std::env::args().collect::<Vec<_>>();
    let filename = &env[1];
    let method_name = env.get(2);
    let mut f = File::open(filename).unwrap_or_else(|_| panic!("No such file {}", filename));
    let header = read::class_header(&mut f).unwrap();

    match method_name {
        Some(name) => {
            // print all methods in the class matching `name`
            let methods = match header.get_methods(name) {
                Ok(methods) => methods,
                Err(e) => {
                    println!("could not find any method named {} in {}", name, header.debug_class_name());
                    return;
                }
            };
            for method in methods.iter() {
                println!("method: {:?}", method);
                for attr in method.attributes.iter() {
                    println!("    method attr {}", attr.display(&header));
                }
            }
        }
        None => {
            // print the entire class...
            println!("{}", header);
        }
    }
}
