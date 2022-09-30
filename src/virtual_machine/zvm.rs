// an identifier for a struct layout registered with zvm. when interpreting
// `zvm::ir::Instruction`s, this is useful for consistency checks. when compiling native code, the
// referenced layout is used to define offsets for field accesses.
struct LayoutId(usize);
