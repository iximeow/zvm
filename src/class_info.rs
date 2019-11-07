mod class_file;
mod vm;

use crate::class_file::read;

use std::fs::File;

pub fn main() {
    let env = std::env::args().collect::<Vec<_>>();
    let filename = &env[1];
    let mut f = File::open(filename).unwrap_or_else(|_| panic!("No such file {}", filename));
    let header = read::class_header(&mut f).unwrap();
    println!("{}", header);
}
