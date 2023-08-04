use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::Ordering;

use crate::class_file::validated::assemble;
use crate::class_file::unvalidated::{attribute::Attribute, AttributeInfo, MethodAccessFlags, MethodInfo};
use crate::{VirtualMachine, VMError, VMState};
use crate::virtual_machine::{ClassFile, ClassFileRef, UnvalidatedClassFile, ValueRef};
use crate::virtual_machine::{MethodBody, JvmArray, JvmObject, JvmValue, NativeObject};

use crate::virtual_machine::NULL_COUNT;

fn new_string<ValueImpl: JvmValue>(vm: &mut VirtualMachine<ValueImpl>, elems: Vec<u8>) -> ValueImpl {
    let java_lang_char_class = vm.resolve_class("java/lang/Character").unwrap();
    let mut fields = HashMap::new();
    let mut data = Vec::new();
    for e in elems.into_iter() {
        // TODO: one day have a `::byte` that for `SimpleJvmValue` is just an integer, but for
        // specialized impls is an actual honest-to-goodness byte?
        data.push(ValueImpl::integer(e as i32));
    }
    fields.insert("value".to_owned(), ValueImpl::array_with_data(java_lang_char_class, data.into_boxed_slice()));
    ValueImpl::object_with_data(vm.resolve_class("java/lang/String").unwrap(), fields)
}

pub fn augment_classfile<ValueImpl: JvmValue>(class_file: ClassFile) -> (ClassFile, HashMap<(String, String), NativeJvmFn<ValueImpl>>) {
    let mut patches: HashMap<(String, String), NativeJvmFn<ValueImpl>> = HashMap::new();
    match class_file.this_class.as_str() {
        // log4j please go away
        "org/apache/logging/log4j/LogManager" => {
            patches.insert(("<clinit>".to_string(), "()V".to_string()), no_op);
            patches.insert(("getLogger".to_string(), "()Lorg/apache/logging/log4j/Logger;".to_string()), |state, vm| {
                state.current_frame_mut()
                    .operand_stack
                    .push(new_instance(vm, "org/apache/logging/log4j/Logger")?);
                Ok(())
            });
        }
        "java/lang/reflect/Array" => {
            patches.insert(("newArray".to_string(), "(Ljava/lang/Class;I)Ljava/lang/Object;".to_string()), array_newarray);
        }
        "java/lang/Runtime" => {
            patches.insert(("availableProcessors".to_string(), "()I".to_string()), runtime_availableprocessors);
        }
        "java/lang/Float" => {
            patches.insert(("floatToRawIntBits".to_string(), "(F)I".to_string()), float_to_raw_int_bits);
            patches.insert(("intBitsToFloat".to_string(), "(I)F".to_string()), int_bits_to_float);
        }
        "java/lang/Double" => {
            patches.insert(("doubleToRawLongBits".to_string(), "(D)J".to_string()), double_to_raw_long_bits);
            patches.insert(("longBitsToDouble".to_string(), "(J)D".to_string()), long_bits_to_double);
        }
        "java/lang/StrictMath" => {
            patches.insert(("log".to_string(), "(D)D".to_string()), double_log);
        }
        "java/lang/ClassLoader" => {
            patches.insert(("registerNatives".to_string(), "()V".to_string()), no_op);
        }
        "java/lang/Thread" => {
            patches.insert(("registerNatives".to_string(), "()V".to_string()), no_op);
            patches.insert(("currentThread".to_string(), "()Ljava/lang/Thread;".to_string()), thread_currentthread);
        }
        "jdk/internal/misc/VM" => {
            patches.insert(("initialize".to_string(), "()V".to_string()), no_op);
        }
        "jdk/internal/misc/Unsafe" => {
            patches.insert(("storeFence".to_string(), "()V".to_string()), no_op);
            patches.insert(("registerNatives".to_string(), "()V".to_string()), no_op);
            patches.insert(("arrayBaseOffset0".to_string(), "(Ljava/lang/Class;)I".to_string()), |state, _vm| {
                // the offset from the start of an array object to its first data element is a
                // constant for all classes
                let _cls = state
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("argument available");
                // it's 0.
                state
                    .current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::integer(0));
                Ok(())
            });
            patches.insert(("objectFieldOffset1".to_string(), "(Ljava/lang/Class;Ljava/lang/String;)J".to_string()), |state, _vm| {
                // the offset from the start of an object to its field is .. uh.. well. not
                // constant... let's hope lying is ok.
                let _field = state
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("argument available");
                let _cls = state
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("argument available");
                // claim that it's 0.
                state
                    .current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::long(0));
                Ok(())
            });
            patches.insert(("arrayIndexScale0".to_string(), "(Ljava/lang/Class;)I".to_string()), |state, _vm| {
                // the scale for each array item iss constant for all classes
                let _cls = state
                    .current_frame_mut()
                    .operand_stack
                    .pop()
                    .expect("argument available");
                // it's the size of an rc.
                state
                    .current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::integer(std::mem::size_of::<Rc<ValueImpl>>() as i32));
                Ok(())
            });
            patches.insert(("addressSize0".to_string(), "()I".to_string()), |state, _vm| {
                state
                    .current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::integer(std::mem::size_of::<usize>() as i32));
                Ok(())
            });
            patches.insert(("isBigEndian0".to_string(), "()Z".to_string()), |state, _vm| {
                // don't run zvm on a big-endian machine for now thanks
                state
                    .current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::integer(0));
                Ok(())
            });
            patches.insert(("unalignedAccess0".to_string(), "()Z".to_string()), |state, _vm| {
                // sure, x86 allows unaligned access
                state
                    .current_frame_mut()
                    .operand_stack
                    .push(ValueImpl::integer(1));
                Ok(())
            });
        }
        _ => {}
    }
    (class_file, patches)
}

pub type NativeJvmFn<ValueImpl> =
    fn(&mut VMState<ValueImpl>, &mut VirtualMachine<ValueImpl>) ->
        Result<(), VMError>;

pub struct SyntheticClassBuilder<ValueImpl: JvmValue> {
    cls: UnvalidatedClassFile,
    bytecode_methods: HashMap<(String, String), MethodBody>,
    native_methods: HashMap<(String, String), NativeJvmFn<ValueImpl>>,
}

impl<ValueImpl: JvmValue> SyntheticClassBuilder<ValueImpl> {
    pub fn new(name: &str) -> Self {
        Self {
            cls: UnvalidatedClassFile::synthetic(name),
            bytecode_methods: HashMap::new(),
            native_methods: HashMap::new(),
        }
    }

    pub fn extends(mut self, name: &str) -> Self {
        Self {
            cls: self.cls.extends(name),
            bytecode_methods: self.bytecode_methods,
            native_methods: self.native_methods,
        }
    }

    pub fn with_method(mut self, name: &str, sig: &str, native: Option<NativeJvmFn<ValueImpl>>) -> Self {
        let new_cls = self.cls.with_method(name, sig);
        let mut native_methods = self.native_methods;
        let mut bytecode_methods = self.bytecode_methods;
        if let Some(native) = native {
            native_methods.insert((name.to_string(), sig.to_string()), native);
        }
        Self {
            cls: new_cls,
            bytecode_methods,
            native_methods,
        }
    }

    pub fn with_method_bytecode(mut self, name: &str, sig: &str, instructions: Vec<crate::class_file::validated::Instruction>) -> Self {
        let name_index = self.cls.add_string(name);
        let sig_index = self.cls.add_string(sig);
        let code_index = self.cls.add_string("Code");

        let code_bytes = assemble(instructions, Some(&mut self.cls));
        eprintln!("synthetic code bytes: {:x?}", &code_bytes.bytes);
        let mut data = Vec::new();
        // max_stack
        data.extend_from_slice(&[0xff, 0xff]);
        // max_locals
        data.extend_from_slice(&[0xff, 0xff]);
        // code_length
        data.extend_from_slice(&(code_bytes.bytes.len() as u32).to_be_bytes());
        data.extend_from_slice(&code_bytes.bytes);
        // exceptions_length
        data.extend_from_slice(&0u16.to_le_bytes());
        // attr_length
        data.extend_from_slice(&0u16.to_le_bytes());

        let raw_handle = MethodInfo {
            access_flags: MethodAccessFlags { flags: 0x0001 },
            name_index: name_index,
            descriptor_index: sig_index,
            attributes: vec![
                AttributeInfo {
                    name_index: code_index,
                    data,
                }
            ]
        };
        self.cls.methods.push(raw_handle);
        self
    }

    pub fn with_field(mut self, name: &str, sig: &str) -> Self {
        let new_cls = self.cls.with_field(name, sig);
        let native_methods = self.native_methods;
        let bytecode_methods = self.bytecode_methods;
        Self {
            cls: new_cls,
            bytecode_methods,
            native_methods,
        }
    }

    pub fn validate(self) -> (
        ClassFile,
        HashMap<
            (String, String),
            NativeJvmFn<ValueImpl>,
        >
    ) {
        eprintln!("{:?}", &self.cls);
        let mut cls = ClassFile::validate(&self.cls).unwrap();
        (cls, self.native_methods)
    }
}

pub fn build_synthetic_class<
    ValueImpl: JvmValue,
>(name: &str) -> Option<
    (
        ClassFile,
        HashMap<
            (String, String),
            NativeJvmFn<ValueImpl>,
        >
    )
> {
    let classfile = match name {
        "java/lang/Class" => {
            SyntheticClassBuilder::new("java/lang/Class")
                .extends("java/lang/Object")
                .with_method("<init>", "()V", Some(object_init))
                .with_method("isPrimitive", "()Z", Some(class_isprimitive))
                .with_method("desiredAssertionStatus", "()Z", Some(class_desired_assertion_status))
                .with_method("getPrimitiveClass", "(Ljava/lang/String;)Ljava/lang/Class;", Some(class_get_primitive_class))
                .with_method("getClassLoader", "()Ljava/lang/ClassLoader;", Some(class_get_classloader))
                .with_method("getName", "()Ljava/lang/String;", Some(class_get_name))
                .with_method("getComponentType", "()Ljava/lang/Class;", Some(class_get_componenttype))
                .validate()
        }
        "java/lang/ThreadLocal" => {
            SyntheticClassBuilder::new("java/lang/ThreadLocal")
                .extends("java/lang/Object")
                .with_method("<init>", "()V", Some(object_init))
                .with_method("get", "()Ljava/lang/Object;", Some(thread_local_get))
                .validate()
        }
        "java/lang/Throwable" => {
            SyntheticClassBuilder::new("java/lang/Throwable")
                .extends("java/lang/Object")
                .with_method("<init>", "()V", Some(object_init))
                .with_method("<init>", "(Ljava/lang/String;)V", Some(throwable_init_string))
                .validate()
        }
        "java/lang/Object" => {
            SyntheticClassBuilder::new("java/lang/Object")
                .with_method("<init>", "()V", Some(object_init))
                .with_method("hashCode", "()I", Some(object_hashcode))
                .with_method("equals", "(Ljava/lang/Object;)Z", Some(object_equals))
                .with_method("getClass", "()Ljava/lang/Class;", Some(object_getclass))
                .validate()
        }
        "java/lang/reflect/Method" => {
            SyntheticClassBuilder::new("java/lang/reflect/Method")
                .validate()
        }
        "java/lang/String" => {
            SyntheticClassBuilder::new("java/lang/String")
                .extends("java/lang/Object")
                .with_method("<init>", "(Ljava/lang/String;)V", Some(string_init_string))
                .with_method("<init>", "([B)V", Some(string_init_bytearray))
                .with_method("<init>", "([C)V", Some(string_init_chararray))
                .with_method("valueOf", "([C)Ljava/lang/String;", Some(string_valueof_chararray))
                .with_method("valueOf", "(C)Ljava/lang/String;", Some(string_valueof_char))
                .with_method("startsWith", "(Ljava/lang/String;)Z", Some(string_startswith))
                .with_method("charAt", "(I)C", Some(string_charat))
                .with_method("hashCode", "()I", Some(string_hashcode))
                .with_method("concat", "(Ljava/lang/String;)Ljava/lang/String;", Some(string_concat))
                .with_method("substring", "(II)Ljava/lang/String;", Some(string_substring))
                .with_method("length", "()I", Some(string_length))
                .with_method("getChars", "(II[CI)V", Some(string_get_chars))
                .with_field("value", "[B")
                .validate()
        }
        /*
        "java/lang/StringBuilder" => {
            SyntheticClassBuilder::new("java/lang/StringBuilder")
                .extends("java/lang/Object")
                .with_method("<init>", "()V", Some(stringbuilder_init))
                .with_method("<init>", "(Ljava/lang/String;)V", Some(string_init_string))
                .with_method("append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", Some(stringbuilder_append_string))
                .with_method("toString", "()Ljava/lang/String;", Some(stringbuilder_tostring))
                .validate()
        }
        */
        "java/lang/System" => {
            SyntheticClassBuilder::new("java/lang/System")
                .extends("java/lang/Object")
                .with_method("<clinit>", "()V", Some(system_clinit))
                .with_method("exit", "(I)V", Some(system_exit))
                .with_method("identityHashCode", "(Ljava/lang/Object;)I", Some(system_identity_hash_code))
                .with_method("getSecurityManager", "()Ljava/lang/SecurityManager;", Some(system_get_security_manager))
                .with_method("getProperty", "(Ljava/lang/String;)Ljava/lang/String;", Some(system_get_property))
                .with_method("arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", Some(system_arraycopy))
                .validate()
        }
        "java/io/PrintStream" => {
            SyntheticClassBuilder::new("java/io/PrintStream")
                .extends("java/lang/Object")
                .with_method("println", "(Ljava/lang/String;)V", Some(system_out_println_string))
                .with_method("println", "(Ljava/lang/Object;)V", Some(system_out_println_object))
                .with_method("println", "(I)V", Some(system_out_println_int))
                .with_method("println", "(J)V", Some(system_out_println_long))
                .validate()
        }
        "java/io/InputStreamReader" => {
            SyntheticClassBuilder::new("java/io/InputStreamReader")
                .extends("java/lang/Object")
                .with_method("<init>", "(Ljava/io/InputStream;)V", Some(input_stream_reader_init))
                .validate()
        }
        _ => {
            return None;
        }
    };
    Some(classfile)
}

fn no_op<ValueImpl: JvmValue>(_state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    Ok(())
}

fn new_instance_with_data<ValueImpl: JvmValue>(vm: &mut VirtualMachine<ValueImpl>, cls: &str, data: HashMap<String, ValueImpl>) -> Result<ValueImpl, VMError> {
    let resolved = vm.resolve_class(cls).unwrap();
    Ok(ValueImpl::object_with_data(resolved, data))
}

fn new_instance<ValueImpl: JvmValue>(vm: &mut VirtualMachine<ValueImpl>, cls: &str) -> Result<ValueImpl, VMError> {
    new_instance_with_data(vm, cls, HashMap::new())
}

fn system_out_println_string<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let Some(obj) = argument.as_type("java/lang/String") {
        if let Some(elements) = obj.get_field("value").as_array() {
            for i in 0..elements.len() {
                if let Some(v) = elements.get_elem(i).expect("TODO: bounds check").as_integer() {
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

fn system_out_println_object<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let Some(obj) = argument.as_type("java/lang/String") {
        if let Some(elements) = obj.get_field("value").as_array() {
            for i in 0..elements.len() {
                if let Some(v) = elements.get_elem(i).expect("TODO: bounds check").as_integer() {
                    print!("{}", *v as u8 as char);
                } else {
                    panic!("string contains non-byte element")
                }
            }
            print!("\n");
        } else {
            panic!("could not get `value` on a `java/lang/String`");
        }
    } else if let Some(obj) = argument.as_object() {
//        println!("{}: {:?}", cls.this_class, fields);
        println!("[object Object]");
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

fn system_out_println_int<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let Some(v) = argument.as_integer() {
        println!("{}", v);
    } else {
        panic!("type error, expected int, got {:?}", argument);
    }
    Ok(())
}

fn system_out_println_long<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let Some(v) = argument.as_long() {
        println!("{}", v);
    } else {
        panic!("type error, expected long, got {:?}", argument);
    }
    Ok(())
}

// "<init>()V"
/*
fn stringbuilder_init<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let data = RefCell::new(NativeObject::StringBuilder(Vec::new()));

    vm.native_instances.insert(ValueRef::of(&receiver), data);
    Ok(())
}
*/

// "append(Ljava/lang/String;);Ljava/lang/StringBuilder"
/*
fn stringbuilder_append_string<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
        if let Some(obj) = appendee.as_type("java/lang/String") {
            if let Some(addend) = obj.get_field("value").as_array() {
                // TODO: properly handle the utf16/non-utf8 data in strings
                // Safety: this is not. the underlying data should be java `char`. right now, zvm
                // fakes it a bit. and treats strings as ascii. this won't suffice in general, is
                // "ok" for now.
                for i in 0..addend.len() {
                    if let Some(el) = addend.get_elem(i).expect("valid index").as_integer() {
                        data.push(*el as u16);
                    }
                }
            } else {
                panic!("could not get `value` on a `java/lang/String`");
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
*/

// "toString()Ljava/lang/String"
/*
fn stringbuilder_tostring<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    // really just consume the argument
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let data = vm.native_instances.get(&ValueRef::of(&receiver)).expect("stringbuilder receiver has associated native data");

    let mut str_data: Vec<ValueImpl> = Vec::new();

    if let NativeObject::StringBuilder(data) = &*data.borrow() {
        for el in data.iter() {
            str_data.push(ValueImpl::integer(*el as i32));
        }
    } else {
        panic!("native object corresponding to stringbuilder receiver is not stringbuilder data");
    }

    let mut fields = HashMap::new();
    fields.insert("value".to_owned(), ValueImpl::array_with_data(vm.resolve_class("java/lang/Character").expect("character is defined"), str_data.into_boxed_slice()));

    let obj = ValueImpl::object_with_data(vm.resolve_class("java/lang/String")?, fields);

    state.current_frame_mut().operand_stack.push(obj);
    Ok(())
}
*/

/*
                native_methods.insert("<init>(Ljava/lang/String;)V".to_string(), string_init_string);
                native_methods.insert("append(Ljava/lang/String;)Ljava/lang/StringBuilder".to_string(), stringbuilder_append_string);
//                native_methods.insert("append([C)Ljava/lang/StringBuilder".to_string(), stringbuilder_append_chars);
                native_methods.insert("toString()Ljava/lang/String".to_string(), stringbuilder_tostring);
*/

// "<init>()V"
fn object_init<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    Ok(())
}
// "hashCode()I"
fn object_hashcode<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    state
        .current_frame_mut()
        .operand_stack
        .push(ValueImpl::integer(0));
    Ok(())
}
fn object_equals<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let (Some(obj1), Some(obj2)) = (receiver.as_object(), argument.as_object()) {
        let res = if obj1 == obj2 {
            1
        } else {
            0
        };
        state.current_frame_mut()
            .operand_stack
            .push(ValueImpl::integer(res));
    } else {
        state.current_frame_mut()
            .operand_stack
            .push(ValueImpl::integer(0));
    }
    Ok(())
}
fn object_getclass<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Some(obj) = receiver.as_object() {
        state.current_frame_mut()
            .operand_stack
            .push(class_object_new(vm, &obj.cls().this_class));
    } else if let Some(_) = receiver.as_array() {
        // well.. this needs to be right one day
        state.current_frame_mut()
            .operand_stack
            .push(class_object_new(vm, "java/lang/Object"));
    } else {
        panic!();
    }
    Ok(())
}
fn class_get_componenttype<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    state.current_frame_mut()
        .operand_stack
        .push(class_object_new(vm, "java/lang/Object"));
    Ok(())
}

// "<init>(Ljava/lang/String;)V"
fn string_init_string<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let (Some(argument), Some(receiver)) =
        (argument.as_object(), receiver.as_object())
    {
        let new_value = argument.get_field("value").clone();
        receiver.set_field("value", new_value);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

// "availableProcessors()I"
fn runtime_availableprocessors<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    state
        .current_frame_mut()
        .operand_stack
        .push(ValueImpl::integer(1));
    Ok(())
}

fn throwable_init_string<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let (Some(_), Some(receiver)) =
        (argument.as_type("java/lang/String"), receiver.as_object())
    {
        receiver.set_field("message", argument);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "<init>(Ljava/io/InputStream;)V"
fn input_stream_reader_init<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let (Some(_), Some(receiver)) =
        (argument.as_object(), receiver.as_object())
    {
        receiver.set_field("stream", argument);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "<init>([B)"
fn string_init_bytearray<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let (Some(new_elems), Some(receiver)) =
        (argument.as_array(), receiver.as_object())
    {
        let mut str_elems = Vec::new();
        for i in 0..new_elems.len() {
            if let Some(el) = new_elems.get_elem(i).expect("valid index").as_integer() {
                if (*el as u8) < 128 {
                    str_elems.push(ValueImpl::integer(*el));
                } else {
                    str_elems.push(ValueImpl::integer(0xfffd));
                }
            } else {
                panic!("bad string");
            }
        }
        receiver.set_field(
            "value",
            ValueImpl::array_with_data(
                vm.resolve_class("java/lang/Character").unwrap(),
                str_elems.into_boxed_slice()
            ),
        );
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "<init>([C)"
fn string_init_chararray<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let (Some(new_elems), Some(receiver)) =
        (argument.as_array(), receiver.as_object())
    {
        let mut str_elems = Vec::new();
        for i in 0..new_elems.len() {
            if let Some(el) = new_elems.get_elem(i).expect("valid index").as_integer() {
                str_elems.push(ValueImpl::integer(*el));
            } else {
                panic!("bad string");
            }
        }
        let java_lang_char_class = vm.resolve_class("java/lang/Character").unwrap();
        receiver.set_field(
            "value",
            ValueImpl::array_with_data(java_lang_char_class, str_elems.into_boxed_slice()),
        );
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "charAt(I)C"
fn string_charat<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if !receiver.as_type("java/lang/String").is_some() {
        panic!("type error, receiver is not a string, got {:?}", argument);
    }
    if let (Some(receiver), Some(index)) = (receiver.as_type("java/lang/String"), argument.as_integer()) {
        let field = receiver.get_field("value");
        let data = field.as_array().expect("string has value");
        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::integer(*data.get_elem(*index as usize).expect("valid index").as_integer().expect("works") as i32));
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "(Ljava/lang/String;)Z"
fn string_startswith<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let (Some(receiver), Some(new_elems)) = (receiver.as_type("java/lang/String"), argument.as_type("java/lang/String")) {
        let field = receiver.get_field("value");
        let receiver = field.as_array().expect("is array");
        let field = new_elems.get_field("value");
        let new_elems = field.as_array().expect("is array");
        // Safety: not
        let receiver = unsafe { receiver.as_slice::<u8>() }.expect("cast works");
        let new_elems = unsafe { new_elems.as_slice::<u8>() }.expect("cast works");
        let mut i = 0;
        let mut beginswith = true;
        while i < new_elems.len() {
            if i == receiver.len() {
                beginswith = false;
                break;
            }

            if receiver[i] != new_elems[i] {
                beginswith = false;
                break;
            }
            i += 1;
        }

        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::integer(if beginswith { 1 } else { 0 }));
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "valueOf([C)Ljava/lang/String;"
fn string_valueof_chararray<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Some(new_elems) = argument.as_array() {
        let mut str_elems: Vec<u8> = Vec::new();
        for i in 0..new_elems.len() {
            if let Some(el) = new_elems.get_elem(i).expect("valid index").as_integer() {
                // TODO: clean up string handling..
                if *el > u8::MAX as i32 {
                    panic!("non-ascii string");
                }
                str_elems.push(*el as u8);
            } else {
                panic!("bad string");
            }
        }
        state
            .current_frame_mut()
            .operand_stack
            .push(new_string(vm, str_elems));
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "valueOf(C)Ljava/lang/String;"
fn string_valueof_char<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Some(elem) = argument.as_integer() {
        let mut str_elems: Vec<u8> = Vec::new();
        let elem = *elem;
        if elem > u8::MAX as i32 {
            panic!("non-ascii string");
        }
        str_elems.push(elem as u8);
        state
            .current_frame_mut()
            .operand_stack
            .push(new_string(vm, str_elems));
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "hashCode()I"
fn string_hashcode<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Some(data) = receiver.as_type("java/lang/String") {
        let field = data.get_field("value");
        let data = field.as_array().expect("is array");
        let data = unsafe { data.as_slice::<u8>() }.expect("cast works");
        let mut hashcode: i32 = 0;
        for c in data.iter().cloned() {
            // value is actually a char array
            hashcode = hashcode.wrapping_mul(31).wrapping_add(c as u16 as i32);
        }
        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::integer(hashcode));
    } else {
        panic!("type error, expected string, got {:?}", receiver);
    }
    Ok(())
}
// "concat(Ljava/lang/String;)Ljava/lang/String;"
fn string_concat<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
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
    if let (Some(base), Some(ext)) = (receiver.as_type("java/lang/String"), argument.as_type("java/lang/String")) {
        let field = base.get_field("value");
        let base = field.as_array().expect("is array");
        let field = ext.get_field("value");
        let ext = field.as_array().expect("is array");
        let base = unsafe { base.as_slice::<u8>() }.expect("cast works");
        let ext = unsafe { ext.as_slice::<u8>() }.expect("cast works");
        let result = new_string(vm, base.iter().cloned().chain(ext.iter().cloned()).collect());
        state
            .current_frame_mut()
            .operand_stack
            .push(result);
    } else {
        panic!("type error, expected string, string, got {:?}", argument);
    }
    Ok(())
}
// "substring(II)Ljava/lang/String;"
fn string_substring<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let end = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let start = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let (Some(s), Some(base), Some(end)) = (receiver.as_type("java/lang/String"), start.as_integer(), end.as_integer()) {
        let field = s.get_field("value");
        let s = field.as_array().expect("is array");
        let s = unsafe { s.as_slice::<u8>() }.expect("cast works");
        if *base < 0 || *end < 0 {
            panic!("invalid base or end in substring");
        }
        let result = new_string(vm, s[(*base as usize)..(*end as usize)].to_vec());
        state
            .current_frame_mut()
            .operand_stack
            .push(result);
    } else {
        panic!("type error, expected string, int, int, got {:?}, {:?}, {:?}", receiver, start, end);
    }
    Ok(())
}
// "length()I"
fn string_length<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Some(s) = receiver.as_type("java/lang/String") {
        let field = s.get_field("value");
        let s = field.as_array().expect("is array");
        let s = unsafe { s.as_slice::<u8>() }.expect("cast works");
        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::integer(s.len() as i32));
    } else {
        panic!("type error, expected string got {:?}", receiver);
    }
    Ok(())
}
// "getChars(II[CI)V"
fn string_get_chars<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let dst_start = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let dst = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let src_end = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let src_start = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let (
        Some(src),
        Some(src_start),
        Some(src_end),
        Some(dst),
        Some(dst_start)
    ) = (receiver.as_type("java/lang/String"), src_start.as_integer(), src_end.as_integer(), dst.as_array(), dst_start.as_integer()) {
        let field = src.get_field("value");
        let src = field.as_array().expect("is array");
        let src_start = *src_start;
        let dst_start = *dst_start;
        let src_end = *src_end;

        let count = src_end - src_start;

        for i in 0..count {
            let i = i as usize;
            *dst.get_elem_mut(dst_start as usize + i).expect("TODO: valid index?") = src.get_elem(src_start as usize + i).expect("TODO: valid index?").clone();
        }
    } else {
        panic!("type error, expected string, int, int, array, int {:?}, {:?}, {:?}, {:?}, {:?}", receiver, src_start, src_end, dst, dst_start);
    }
    Ok(())
}

fn system_clinit<ValueImpl: JvmValue>(_state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let java_lang_system_class = vm.resolve_class("java/lang/System").unwrap();
    let cls_ref = ClassFileRef::of(&java_lang_system_class);
    let mut statics = HashMap::new();
    fn make_fd_instance<ValueImpl: JvmValue>(vm: &mut VirtualMachine<ValueImpl>, fd: i32) -> ValueImpl {
        let mut fields = HashMap::new();
        fields.insert("fd".to_owned(), ValueImpl::integer(fd));
        ValueImpl::object_with_data(vm.resolve_class("java/io/PrintStream").unwrap(), fields)
    }
    statics.insert("in".to_string(), make_fd_instance(vm, 0));
    statics.insert("out".to_string(), make_fd_instance(vm, 1));
    statics.insert("err".to_string(), make_fd_instance(vm, 2));
    // shouldn't have run initializers for java/lang/System yet
    assert!(vm.static_instances.insert(cls_ref, statics).is_none());
    Ok(())
}

fn system_exit<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(i) = argument.as_integer() {
        std::process::exit(*i);
    } else {
        panic!("attempted to exit with non-int operand");
    }
}

fn system_identity_hash_code<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(obj) = argument.as_object() {
        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::integer(obj.internal_obj_id() as i32));
        Ok(())
    } else {
        panic!("invalid argument for identityHashCode");
    }
}

fn system_get_security_manager<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    NULL_COUNT.fetch_add(1, Ordering::SeqCst);
    state
        .current_frame_mut()
        .operand_stack
        .push(ValueImpl::null(String::new()));
    Ok(())
}

fn system_get_property<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let property = if argument.as_type("java/lang/String").is_some() {
        let data: &[u8] = panic!("get string somehow");
        match &data[..] {
            b"file.encoding" => {
                ValueImpl::string(vm, "UTF-8")
            }
            _ => {
                let property_name = unsafe { std::str::from_utf8_unchecked(&data) };
                eprintln!("------------ get_property {:?}", property_name);
                ValueImpl::null(String::new())
            }
        }
    } else {
        panic!("invalid argument for getProperty {:?}", argument);
    };

    state.current_frame_mut().operand_stack.push(property);
    Ok(())
}

fn array_newarray<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let count = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let cls = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let (
        Some(obj),
        Some(count)
    ) = (cls.as_object(), count.as_integer()) {
        let mut elems = Vec::new();
        for _ in 0..*count {
            NULL_COUNT.fetch_add(1, Ordering::SeqCst);
            elems.push(ValueImpl::null(String::new()));
        }

        let class_name_string = {
            let field = obj.get_field("class");
            let obj = field.as_object().expect("is object");
            let classname = obj.get_field("value");
            std::mem::drop(classname);
            panic!("todo: turn class name array into a real string");
        };

        state.current_frame_mut()
            .operand_stack
            .push(ValueImpl::array_with_data(
                vm.resolve_class(class_name_string).expect("TODO: need to fish out the right string from `cls`."),
                elems.into_boxed_slice(),
            ));

        Ok(())
    } else {
        panic!("arraycopy with bad types");
    }
}


fn system_arraycopy<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let count = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let dest_offset = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let dest = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let src_offset = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    let src = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let (
        Some(src_data),
        Some(dest_data),
        Some(src_offset),
        Some(dest_offset),
        Some(count)
    ) = (src.as_array(), dest.as_array(), src_offset.as_integer(), dest_offset.as_integer(), count.as_integer()) {
        for i in 0..*count as usize {
            let src_el = src_data.get_elem_mut(*src_offset as usize + i);
            let dest_el = dest_data.get_elem_mut(*dest_offset as usize + i);
            *dest_el.expect("valid index") = src_el.expect("valid index").clone();
        }
        Ok(())
    } else {
        panic!("arraycopy with bad types");
    }
}

fn class_isprimitive<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(obj) = receiver.as_object() {
        if let Some(value) = obj.get_field("class").as_type("java/lang/String") {
            const PRIMITIVES: &[&[u8]] = &[
                b"java/lang/Byte",
                b"java/lang/Character",
                b"java/lang/Short",
                b"java/lang/Integer",
                b"java/lang/Long",
                b"java/lang/Float",
                b"java/lang/Double",
            ];
            let field = value.get_field("value");
            let value = field.as_array().expect("is array");
            let value = unsafe { value.as_slice::<u8>() }.expect("cast works");
            let value = if PRIMITIVES.contains(&value) {
                ValueImpl::integer(1)
            } else {
                ValueImpl::integer(0)
            };
            state
                .current_frame_mut()
                .operand_stack
                .push(value);
        } else {
            panic!("called isPrimitive on a class with no fields?");
        }
    } else {
        panic!("isPrimitive called on non-object value?");
    }
    Ok(())
}

fn class_desired_assertion_status<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    state
        .current_frame_mut()
        .operand_stack
        .push(ValueImpl::integer(0));
    Ok(())
}

// getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;
fn class_get_primitive_class<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let primitive = if let Some(s) = receiver.as_type("java/lang/String") {
        let field = s.get_field("value");
        let s = field.as_array().expect("is array");
        let s = unsafe { s.as_slice::<u8>() }.expect("cast works?");
        match s {
            b"byte" => {
                class_object_new(vm, "java/lang/Byte")
            },
            b"short" => {
                class_object_new(vm, "java/lang/Short")
            },
            b"int" => {
                class_object_new(vm, "java/lang/Integer")
            },
            b"long" => {
                class_object_new(vm, "java/lang/Long")
            },
            b"char" => {
                class_object_new(vm, "java/lang/Character")
            },
            b"float" => {
                class_object_new(vm, "java/lang/Float")
            },
            b"double" => {
                class_object_new(vm, "java/lang/Double")
            },
            b"boolean" => {
                class_object_new(vm, "java/lang/Boolean")
            },
            b"void" => {
                class_object_new(vm, "java/lang/Void")
            },
            other => {
                panic!("unsupported class {:?}", other);
            }
        }
    } else {
        panic!("invalid string for getPrimitiveClass");
    };

    state
        .current_frame_mut()
        .operand_stack
        .push(primitive);
    Ok(())
}

fn class_get_name<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(obj) = receiver.as_object() {
        state
            .current_frame_mut()
            .operand_stack
            .push(obj.get_field("class"));
    }
    Ok(())
}

pub fn class_object_new<ValueImpl: JvmValue>(vm: &mut VirtualMachine<ValueImpl>, class_name: &str) -> ValueImpl {
    // TODO: entry api..
    if vm.class_instances.contains_key(class_name) {
        vm.class_instances.get(class_name).unwrap().clone()
    } else {
        let mut fields = HashMap::new();
        fields.insert("class".to_owned(), ValueImpl::string(vm, class_name));
        let jvm_obj = ValueImpl::object_with_data(vm.resolve_class("java/lang/Class").unwrap(), fields);
        vm.class_instances.insert(class_name.to_string(), jvm_obj.clone());
        jvm_obj
    }
}

fn thread_local_get<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(obj) = receiver.as_object() {
        state
            .current_frame_mut()
            .operand_stack
            .push(obj.get_field("value"));
    }
    Ok(())
}

fn float_to_raw_int_bits<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(f) = argument.as_float() {
        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::integer(f.to_bits() as i32));
    } else {
        panic!("bad operand type for float_to_raw_int_bits");
    }
    Ok(())
}

fn long_bits_to_double<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(l) = argument.as_long() {
        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::double(f64::from_bits(*l as u64)));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn int_bits_to_float<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(i) = argument.as_integer() {
        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::float(f32::from_bits(*i as u32)));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn double_to_raw_long_bits<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(d) = argument.as_double() {
        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::long(d.to_bits() as i64));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn double_log<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, _vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Some(d) = argument.as_double() {
        state
            .current_frame_mut()
            .operand_stack
            .push(ValueImpl::double(d.ln()));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

// TODO: should build a singleton here probably....
fn thread_currentthread<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    state
        .current_frame_mut()
        .operand_stack
        .push(new_instance(vm, "java/lang/Thread")?);
    Ok(())
}

// TODO: should build a singleton here probably....
fn class_get_classloader<ValueImpl: JvmValue>(state: &mut VMState<ValueImpl>, vm: &mut VirtualMachine<ValueImpl>) -> Result<(), VMError> {
    state
        .current_frame_mut()
        .operand_stack
        .pop(); // TODO: should return the classloader that loaded this object's class
    // TODO: also verify the above was an object...
    state
        .current_frame_mut()
        .operand_stack
        .push(new_instance(vm, "java/lang/ClassLoader")?);
    Ok(())
}
