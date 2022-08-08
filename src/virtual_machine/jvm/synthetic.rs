use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::Ordering;

use crate::{VirtualMachine, VMError, VMState, Value};
use crate::virtual_machine::{ClassFile, ClassFileRef, UnvalidatedClassFile, ValueRef};
use crate::virtual_machine::{JvmObject, NativeObject};

use crate::virtual_machine::NULL_COUNT;

pub fn augment_classfile(mut class_file: ClassFile) -> ClassFile {
    match class_file.this_class.as_str() {
        // log4j please go away
        "org/apache/logging/log4j/LogManager" => {
            class_file.native_methods.insert("<clinit>()V".to_string(), no_op);
            class_file.native_methods.insert("getLogger()Lorg/apache/logging/log4j/Logger;".to_string(), |state, vm| {
                state.current_frame_mut()
                    .operand_stack
                    .push(new_instance(vm, "org/apache/logging/log4j/Logger")?);
                Ok(())
            });
        }
        "java/lang/reflect/Array" => {
            class_file.native_methods.insert("newArray(Ljava/lang/Class;I)Ljava/lang/Object;".to_string(), array_newarray);
        }
        "java/lang/Runtime" => {
            class_file.native_methods.insert("availableProcessors()I".to_string(), runtime_availableprocessors);
        }
        "java/lang/Float" => {
            class_file.native_methods.insert("floatToRawIntBits(F)I".to_string(), float_to_raw_int_bits);
            class_file.native_methods.insert("intBitsToFloat(I)F".to_string(), int_bits_to_float);
        }
        "java/lang/Double" => {
            class_file.native_methods.insert("doubleToRawLongBits(D)J".to_string(), double_to_raw_long_bits);
            class_file.native_methods.insert("longBitsToDouble(J)D".to_string(), long_bits_to_double);
        }
        "java/lang/StrictMath" => {
            class_file.native_methods.insert("log(D)D".to_string(), double_log);
        }
        "java/lang/ClassLoader" => {
            class_file.native_methods.insert("registerNatives()V".to_string(), no_op);
        }
        "java/lang/Thread" => {
            class_file.native_methods.insert("registerNatives()V".to_string(), no_op);
            class_file.native_methods.insert("currentThread()Ljava/lang/Thread;".to_string(), thread_currentthread);
        }
        "jdk/internal/misc/VM" => {
            class_file.native_methods.insert("initialize()V".to_string(), no_op);
        }
        "jdk/internal/misc/Unsafe" => {
            class_file.native_methods.insert("storeFence()V".to_string(), no_op);
            class_file.native_methods.insert("registerNatives()V".to_string(), no_op);
            class_file.native_methods.insert("arrayBaseOffset0(Ljava/lang/Class;)I".to_string(), |state, _vm| {
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
                    .push(Value::Integer(0));
                Ok(())
            });
            class_file.native_methods.insert("objectFieldOffset1(Ljava/lang/Class;Ljava/lang/String;)J".to_string(), |state, _vm| {
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
                    .push(Value::Long(0));
                Ok(())
            });
            class_file.native_methods.insert("arrayIndexScale0(Ljava/lang/Class;)I".to_string(), |state, _vm| {
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
                    .push(Value::Integer(std::mem::size_of::<Rc<Value>>() as i32));
                Ok(())
            });
            class_file.native_methods.insert("addressSize0()I".to_string(), |state, _vm| {
                state
                    .current_frame_mut()
                    .operand_stack
                    .push(Value::Integer(std::mem::size_of::<usize>() as i32));
                Ok(())
            });
            class_file.native_methods.insert("isBigEndian0()Z".to_string(), |state, _vm| {
                // don't run zvm on a big-endian machine for now thanks
                state
                    .current_frame_mut()
                    .operand_stack
                    .push(Value::Integer(0));
                Ok(())
            });
            class_file.native_methods.insert("unalignedAccess0()Z".to_string(), |state, _vm| {
                // sure, x86 allows unaligned access
                state
                    .current_frame_mut()
                    .operand_stack
                    .push(Value::Integer(1));
                Ok(())
            });
        }
        _ => {}
    }
    class_file
}

pub fn build_synthetic_class(name: &str) -> Option<ClassFile> {
    let classfile = match name {
        "java/lang/Class" => {
            let cls = UnvalidatedClassFile::synthetic("java/lang/Class")
                .extends("java/lang/Object")
                .with_method("<init>", "()V", Some(object_init))
                .with_method("isPrimitive", "()Z", Some(class_isprimitive))
                .with_method("desiredAssertionStatus", "()Z", Some(class_desired_assertion_status))
                .with_method("getPrimitiveClass", "(Ljava/lang/String;)Ljava/lang/Class;", Some(class_get_primitive_class))
                .with_method("getClassLoader", "()Ljava/lang/ClassLoader;", Some(class_get_classloader))
                .with_method("getName", "()Ljava/lang/String;", Some(class_get_name))
                .with_method("getComponentType", "()Ljava/lang/Class;", Some(class_get_componenttype));
            ClassFile::validate(&cls).unwrap()
        }
        "java/lang/ThreadLocal" => {
            let cls = UnvalidatedClassFile::synthetic("java/lang/ThreadLocal")
                .extends("java/lang/Object")
                .with_method("<init>", "()V", Some(object_init))
                .with_method("get", "()Ljava/lang/Object;", Some(thread_local_get));
            ClassFile::validate(&cls).unwrap()
        }
        "java/lang/Throwable" => {
            let cls = UnvalidatedClassFile::synthetic("java/lang/Throwable")
                .extends("java/lang/Object")
                .with_method("<init>", "()V", Some(object_init))
                .with_method("<init>", "(Ljava/lang/String;)V", Some(throwable_init_string));
            ClassFile::validate(&cls).unwrap()
        }
        "java/lang/Object" => {
            let cls = UnvalidatedClassFile::synthetic("java/lang/Object")
                .with_method("<init>", "()V", Some(object_init))
                .with_method("hashCode", "()I", Some(object_hashcode))
                .with_method("equals", "(Ljava/lang/Object;)Z", Some(object_equals))
                .with_method("getClass", "()Ljava/lang/Class;", Some(object_getclass));
            ClassFile::validate(&cls).unwrap()
        }
        "java/lang/reflect/Method" => {
            let cls = UnvalidatedClassFile::synthetic("java/lang/reflect/Method");
            ClassFile::validate(&cls).unwrap()
        }
        "java/lang/String" => {
            let cls = UnvalidatedClassFile::synthetic("java/lang/String")
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
                .with_field("value", "[B");
            ClassFile::validate(&cls).unwrap()
        }
        "java/lang/StringBuilder" => {
            let cls = UnvalidatedClassFile::synthetic("java/lang/StringBuilder")
                .extends("java/lang/Object")
                .with_method("<init>", "()V", Some(stringbuilder_init))
                .with_method("<init>", "(Ljava/lang/String;)V", Some(string_init_string))
                .with_method("append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", Some(stringbuilder_append_string))
                .with_method("toString", "()Ljava/lang/String;", Some(stringbuilder_tostring));
            ClassFile::validate(&cls).unwrap()
        }
        "java/lang/System" => {
            let cls = UnvalidatedClassFile::synthetic("java/lang/System")
                .extends("java/lang/Object")
                .with_method("<clinit>", "()V", Some(system_clinit))
                .with_method("exit", "(I)V", Some(system_exit))
                .with_method("identityHashCode", "(Ljava/lang/Object;)I", Some(system_identity_hash_code))
                .with_method("getSecurityManager", "()Ljava/lang/SecurityManager;", Some(system_get_security_manager))
                .with_method("getProperty", "(Ljava/lang/String;)Ljava/lang/String;", Some(system_get_property))
                .with_method("arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", Some(system_arraycopy));
            ClassFile::validate(&cls).unwrap()
        }
        "java/io/PrintStream" => {
            let cls = UnvalidatedClassFile::synthetic("java/io/PrintStream")
                .extends("java/lang/Object")
                .with_method("println", "(Ljava/lang/String;)V", Some(system_out_println_string))
                .with_method("println", "(Ljava/lang/Object;)V", Some(system_out_println_object))
                .with_method("println", "(I)V", Some(system_out_println_int))
                .with_method("println", "(J)V", Some(system_out_println_long));
            ClassFile::validate(&cls).unwrap()
        }
        "java/io/InputStreamReader" => {
            let cls = UnvalidatedClassFile::synthetic("java/io/InputStreamReader")
                .extends("java/lang/Object")
                .with_method("<init>", "(Ljava/io/InputStream;)V", Some(input_stream_reader_init));
            ClassFile::validate(&cls).unwrap()
        }
        _ => {
            return None;
        }
    };
    Some(classfile)
}

fn no_op(_state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    Ok(())
}

fn new_instance_with_data(vm: &mut VirtualMachine, cls: &str, data: HashMap<String, Value>) -> Result<Value, VMError> {
    let resolved = vm.resolve_class(cls).unwrap();
    Ok(Value::Object(JvmObject::new_with_data(resolved, data)))
}

fn new_instance(vm: &mut VirtualMachine, cls: &str) -> Result<Value, VMError> {
    new_instance_with_data(vm, cls, HashMap::new())
}

fn system_out_println_string(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let Value::String(data) = argument {
        if let Ok(string) = std::str::from_utf8(data.as_slice()) {
            println!("{}", string);
        } else {
            panic!("executing System.out.println(\"{:?}\")", data);
        }
    } else if let Value::Object(obj) = argument {
        if let Value::Array(elements) = obj.get_field("value") {
            for el in elements.borrow().iter() {
                if let Value::Integer(v) = el {
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

fn system_out_println_object(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let Value::String(data) = argument {
        if let Ok(string) = std::str::from_utf8(data.as_slice()) {
            println!("{}", string);
        } else {
            panic!("executing System.out.println(\"{:?}\")", data);
        }
    } else if let Value::Object(_obj) = argument {
//        println!("{}: {:?}", cls.this_class, fields);
        println!("[object Object]");
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

fn system_out_println_int(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let Value::Integer(v) = argument {
        println!("{}", v);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

fn system_out_println_long(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let Value::Long(v) = argument {
        println!("{}", v);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

// "<init>()V"
fn stringbuilder_init(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let data = RefCell::new(NativeObject::StringBuilder(Vec::new()));

    vm.native_instances.insert(ValueRef::of(&receiver), data);
    Ok(())
}

// "append(Ljava/lang/String;);Ljava/lang/StringBuilder"
fn stringbuilder_append_string(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
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
        if let Value::String(str_data) = appendee {
            // do thing
            for el in str_data.iter() {
                data.push(*el as u16);
            }
        } else if let Value::Object(obj) = appendee {
            // do thing
            if let Value::Array(str_data) = obj.get_field("value") {
                let str_data = str_data.borrow();
                for el in str_data.iter() {
                    if let Value::Integer(i) = el {
                        data.push(*i as u16);
                    }
                }
            } else {
                panic!("appendee of stringbuilder is a non-string object");
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

// "toString()Ljava/lang/String"
fn stringbuilder_tostring(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
    // really just consume the argument
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let data = vm.native_instances.get(&ValueRef::of(&receiver)).expect("stringbuilder receiver has associated native data");

    let mut str_data: Vec<Value> = Vec::new();

    if let NativeObject::StringBuilder(data) = &*data.borrow() {
        for el in data.iter() {
            str_data.push(Value::Integer(*el as i32));
        }
    } else {
        panic!("native object corresponding to stringbuilder receiver is not stringbuilder data");
    }

    let obj = JvmObject::create(vm.resolve_class("java/lang/String")?)
        .with_field("value", Value::Array(Rc::new(RefCell::new(str_data.into_boxed_slice()))));
    let s = Value::Object(obj);

    state.current_frame_mut().operand_stack.push(s);
    Ok(())
}

/*
                native_methods.insert("<init>(Ljava/lang/String;)V".to_string(), string_init_string);
                native_methods.insert("append(Ljava/lang/String;)Ljava/lang/StringBuilder".to_string(), stringbuilder_append_string);
//                native_methods.insert("append([C)Ljava/lang/StringBuilder".to_string(), stringbuilder_append_chars);
                native_methods.insert("toString()Ljava/lang/String".to_string(), stringbuilder_tostring);
*/

// "<init>()V"
fn object_init(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    Ok(())
}
// "hashCode()I"
fn object_hashcode(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    state
        .current_frame_mut()
        .operand_stack
        .push(Value::Integer(0));
    Ok(())
}
fn object_equals(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::Object(obj1), Value::Object(obj2)) = (&receiver, &argument) {
        let res = if obj1 == obj2 {
            1
        } else {
            0
        };
        state.current_frame_mut()
            .operand_stack
            .push(Value::Integer(res));
    } else {
        state.current_frame_mut()
            .operand_stack
            .push(Value::Integer(0));
    }
    Ok(())
}
fn object_getclass(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::Object(obj) = receiver {
        state.current_frame_mut()
            .operand_stack
            .push(class_object_new(vm, &obj.cls().this_class));
    } else if let Value::Array(_) = receiver {
        // well.. this needs to be right one day
        state.current_frame_mut()
            .operand_stack
            .push(class_object_new(vm, "java/lang/Object"));
    } else {
        panic!();
    }
    Ok(())
}
fn class_get_componenttype(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
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
fn string_init_string(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::Object(argument), Value::Object(receiver)) =
        (&argument, &receiver)
    {
        let new_value = argument.get_field("value").clone();
        receiver.set_field("value", new_value);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}

// "availableProcessors()I"
fn runtime_availableprocessors(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let _receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    state
        .current_frame_mut()
        .operand_stack
        .push(Value::Integer(1));
    Ok(())
}

fn throwable_init_string(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::String(_), Value::Object(receiver)) =
        (&argument, &receiver)
    {
        receiver.set_field("message", argument);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "<init>(Ljava/io/InputStream;)V"
fn input_stream_reader_init(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::Object(_), Value::Object(receiver)) =
        (&argument, &receiver)
    {
        receiver.set_field("stream", argument);
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "<init>([B)"
fn string_init_bytearray(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::Array(new_elems), Value::Object(receiver)) =
        (&argument, &receiver)
    {
        let mut str_elems = Vec::new();
        for el in new_elems.borrow().iter() {
            if let Value::Integer(i) = el {
                if (*i as u8) < 128 {
                    str_elems.push(Value::Integer(*i));
                } else {
                    str_elems.push(Value::Integer(0xfffd));
                }
            } else {
                panic!("bad string");
            }
        }
        receiver.set_field(
            "value",
            Value::Array(Rc::new(RefCell::new(str_elems.into_boxed_slice()))),
        );
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "<init>([C)"
fn string_init_chararray(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::Array(new_elems), Value::Object(receiver)) =
        (&argument, &receiver)
    {
        let mut str_elems = Vec::new();
        for el in new_elems.borrow().iter() {
            if let Value::Integer(i) = el {
                str_elems.push(Value::Integer(*i));
            } else {
                panic!("bad string");
            }
        }
        receiver.set_field(
            "value",
            Value::Array(Rc::new(RefCell::new(str_elems.into_boxed_slice()))),
        );
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "charAt(I)C"
fn string_charat(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::String(receiver), Value::Integer(index)) = (&receiver, &argument) {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Integer(receiver[*index as usize] as i32));
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "(Ljava/lang/String;)Z"
fn string_startswith(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::String(receiver), Value::String(new_elems)) = (&receiver, &argument) {
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
            .push(Value::Integer(if beginswith { 1 } else { 0 }));
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "valueOf([C)Ljava/lang/String;"
fn string_valueof_chararray(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::Array(new_elems) = &argument {
        let mut str_elems: Vec<u8> = Vec::new();
        for el in new_elems.borrow().iter() {
            if let Value::Integer(i) = el {
                let i = *i;
                if i > u8::MAX as i32 {
                    panic!("non-ascii string");
                }
                str_elems.push(i as u8);
            } else {
                panic!("bad string");
            }
        }
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::String(Rc::new(str_elems)));
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "valueOf(C)Ljava/lang/String;"
fn string_valueof_char(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::Integer(elem) = &argument {
        let mut str_elems: Vec<u8> = Vec::new();
        let elem = *elem;
        if elem > u8::MAX as i32 {
            panic!("non-ascii string");
        }
        str_elems.push(elem as u8);
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::String(Rc::new(str_elems)));
    } else {
        panic!("type error, expected string, got {:?}", argument);
    }
    Ok(())
}
// "hashCode()I"
fn string_hashcode(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::String(data) = receiver {
        let mut hashcode: i32 = 0;
        for c in data.iter().cloned() {
            // value is actually a char array
            hashcode = hashcode.wrapping_mul(31).wrapping_add(c as u16 as i32);
        }
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Integer(hashcode));
    } else if let Value::Object(obj) = receiver {
        let mut hashcode: i32 = 0;
        if let Value::Array(elems) = obj.get_field("value") {
            for c in elems.borrow().iter() {
                if let Value::Integer(v) = c {
                    // value is actually a char array
                    hashcode = hashcode.wrapping_mul(31).wrapping_add(*v as u16 as i32);
                } else {
                    panic!("string contains non-byte element");
                }
            }
            state
                .current_frame_mut()
                .operand_stack
                .push(Value::Integer(hashcode));
        } else {
            panic!("string does not have a value?");
        }
    } else {
        panic!("type error, expected string, got {:?}", receiver);
    }
    Ok(())
}
// "concat(Ljava/lang/String;)Ljava/lang/String;"
fn string_concat(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::String(base), Value::String(ext)) = (&receiver, &argument) {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::String(
                Rc::new(base.iter().cloned().chain(ext.iter().cloned()).collect()),
            ));
    } else {
        panic!("type error, expected string, string, got {:?}", argument);
    }
    Ok(())
}
// "substring(II)Ljava/lang/String;"
fn string_substring(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
    if let (Value::String(s), Value::Integer(base), Value::Integer(end)) = (&receiver, &start, &end) {
        if *base < 0 || *end < 0 {
            panic!("invalid base or end in substring");
        }
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::String(
                Rc::new(s[(*base as usize)..(*end as usize)].to_vec())
            ));
    } else {
        panic!("type error, expected string, int, int, got {:?}, {:?}, {:?}", receiver, start, end);
    }
    Ok(())
}
// "length()I"
fn string_length(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");
    if let Value::String(s) = &receiver {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Integer(s.len() as i32));
    } else {
        panic!("type error, expected string got {:?}", receiver);
    }
    Ok(())
}
// "getChars(II[CI)V"
fn string_get_chars(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
        Value::String(src),
        Value::Integer(src_start),
        Value::Integer(src_end),
        Value::Array(dst),
        Value::Integer(dst_start)
    ) = (&receiver, &src_start, &src_end, &dst, &dst_start) {
        let src_start = *src_start;
        let dst_start = *dst_start;
        let src_end = *src_end;
        let mut dst = dst.borrow_mut();

        let count = src_end - src_start;

        for i in 0..count {
            let i = i as usize;
            dst[dst_start as usize + i] = Value::Integer(src[src_start as usize + i] as i32);
        }
    } else {
        panic!("type error, expected string, int, int, array, int {:?}, {:?}, {:?}, {:?}, {:?}", receiver, src_start, src_end, dst, dst_start);
    }
    Ok(())
}

fn system_clinit(_state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
    let java_lang_system_class = vm.resolve_class("java/lang/System").unwrap();
    let cls_ref = ClassFileRef::of(&java_lang_system_class);
    let mut statics = HashMap::new();
    fn make_fd_instance(vm: &mut VirtualMachine, fd: i32) -> Value {
        Value::Object(
            JvmObject::create(vm.resolve_class("java/io/PrintStream").unwrap())
                .with_field("fd", Value::Integer(fd))
        )
    }
    statics.insert("in".to_string(), make_fd_instance(vm, 0));
    statics.insert("out".to_string(), make_fd_instance(vm, 1));
    statics.insert("err".to_string(), make_fd_instance(vm, 2));
    // shouldn't have run initializers for java/lang/System yet
    assert!(vm.static_instances.insert(cls_ref, statics).is_none());
    Ok(())
}

fn system_exit(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Integer(i) = argument {
        std::process::exit(i);
    } else {
        panic!("attempted to exit with non-int operand");
    }
}

fn system_identity_hash_code(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Object(obj) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Integer(obj.fields_ptr() as i32));
        Ok(())
    } else {
        panic!("invalid argument for identityHashCode");
    }
}

fn system_get_security_manager(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    NULL_COUNT.fetch_add(1, Ordering::SeqCst);
    state
        .current_frame_mut()
        .operand_stack
        .push(Value::Null(String::new()));
    Ok(())
}

fn system_get_property(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let property = match argument {
        Value::String(data) => {
            match &data[..] {
                b"file.encoding" => {
                    Value::String(Rc::new("UTF-8".bytes().collect()))
                }
                _ => {
                    let property_name = unsafe { std::str::from_utf8_unchecked(&data) };
                    eprintln!("------------ get_property {:?}", property_name);
                    Value::Null(String::new())
                }
            }
        }
        Value::Object(obj) => {
            panic!("get_property doesn't get support dynamic strings: {:?}", &obj.cls().this_class);
        }
        argument => {
            panic!("invalid argument for getProperty {:?}", argument);
        }
    };

    state.current_frame_mut().operand_stack.push(property);
    Ok(())
}

fn array_newarray(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
        Value::Object(_obj),
        Value::Integer(count)
    ) = (cls, count) {
        let mut elems = Vec::new();
        for _ in 0..count {
            NULL_COUNT.fetch_add(1, Ordering::SeqCst);
            elems.push(Value::Null(String::new()));
        }

        state.current_frame_mut()
            .operand_stack
            .push(Value::Array(
                Rc::new(RefCell::new(elems.into_boxed_slice())),
            ));

        Ok(())
    } else {
        panic!("arraycopy with bad types");
    }
}


fn system_arraycopy(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
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
        Value::Array(src_data),
        Value::Array(dest_data),
        Value::Integer(src_offset),
        Value::Integer(dest_offset),
        Value::Integer(count)
    ) = (src, dest, src_offset, dest_offset, count) {
        let src = &mut src_data.borrow_mut()[src_offset as usize..];
        let dest = &mut dest_data.borrow_mut()[dest_offset as usize..];

        for i in 0..count {
            let i = i as usize;
            dest[i] = src[i].clone();
        }
        Ok(())
    } else {
        panic!("arraycopy with bad types");
    }
}

fn class_isprimitive(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Object(obj) = receiver {
        if let Value::String(value) = obj.get_field("class") {
            const PRIMITIVES: &[&[u8]] = &[
                b"java/lang/Byte",
                b"java/lang/Character",
                b"java/lang/Short",
                b"java/lang/Integer",
                b"java/lang/Long",
                b"java/lang/Float",
                b"java/lang/Double",
            ];
            let value = if PRIMITIVES.contains(&value.as_slice()) {
                Value::Integer(1)
            } else {
                Value::Integer(0)
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

fn class_desired_assertion_status(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    state
        .current_frame_mut()
        .operand_stack
        .push(Value::Integer(0));
    Ok(())
}

// getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;
fn class_get_primitive_class(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    let primitive = if let Value::String(s) = receiver {
        match s.as_slice() {
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

fn class_get_name(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Object(obj) = receiver {
        state
            .current_frame_mut()
            .operand_stack
            .push(obj.get_field("class"));
    }
    Ok(())
}

pub fn class_object_new(vm: &mut VirtualMachine, class_name: &str) -> Value {
    // TODO: entry api..
    let obj = if vm.class_instances.contains_key(class_name) {
        vm.class_instances.get(class_name).unwrap().clone()
    } else {
        let jvm_obj = JvmObject::create(vm.resolve_class("java/lang/Class").unwrap())
            .with_field("class", Value::String(Rc::new(class_name.bytes().collect())));
        vm.class_instances.insert(class_name.to_string(), jvm_obj.clone());
        jvm_obj
    };
    Value::Object(obj)
}

fn thread_local_get(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let receiver = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Object(obj) = receiver {
        state
            .current_frame_mut()
            .operand_stack
            .push(obj.get_field("value"));
    }
    Ok(())
}

fn float_to_raw_int_bits(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Float(f) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Integer(f.to_bits() as i32));
    } else {
        panic!("bad operand type for float_to_raw_int_bits");
    }
    Ok(())
}

fn long_bits_to_double(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Long(l) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Double(f64::from_bits(l as u64)));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn int_bits_to_float(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Integer(i) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Float(f32::from_bits(i as u32)));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn double_to_raw_long_bits(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Double(d) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Long(d.to_bits() as i64));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

fn double_log(state: &mut VMState, _vm: &mut VirtualMachine) -> Result<(), VMError> {
    let argument = state
        .current_frame_mut()
        .operand_stack
        .pop()
        .expect("argument available");

    if let Value::Double(d) = argument {
        state
            .current_frame_mut()
            .operand_stack
            .push(Value::Double(d.ln()));
    } else {
        panic!("bad operand type for double_to_raw_long_bits");
    }
    Ok(())
}

// TODO: should build a singleton here probably....
fn thread_currentthread(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
    state
        .current_frame_mut()
        .operand_stack
        .push(new_instance(vm, "java/lang/Thread")?);
    Ok(())
}

// TODO: should build a singleton here probably....
fn class_get_classloader(state: &mut VMState, vm: &mut VirtualMachine) -> Result<(), VMError> {
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
