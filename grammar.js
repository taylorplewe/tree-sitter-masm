/**
 * @file Microsoft Macro Assembler x86 syntax
 * @author Taylor Plewe <tplewe@outlook.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
  name: "masm",

  rules: {
    source_file: $ => "hello",


    // terminals

    alpha: _ => /[a-z]|@|_|\$|\?/,

    context_item: _ => choice("assumes", "radix", "listing", "cpu", "all"),
    data_type: _ => choice("byte", "sbyte", "word", "sword", "dword", "sdword", "fword", "qword", "sqword", "tbyte", "oword", "real4", "real8", "real10", "mmword", "xmmword", "ymmword"),
    dec_digit: _ => choice("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
    hex_digit: _ => choice("a", "b", "c", "d", "e", "f"),
    radix_override: _ => choice("h", "o", "q", "t", "y"),

    sign: _ => choice("+", "-"),
    binary_op: _ => choice("==", "!=", ">=", "<=", ">", "<", "&"),
    add_op: _ => choice("+", "-"),
    mul_op: _ => choice("*", "/", "mod"),
    rel_op: _ => choice("eq", "ne", "lt", "le", "gt", "ge"),
    shift_op: _ => choice("shr", "shl"),
    quote: _ => choice(`"`, "'"),

    coprocessor: _ => choice(".8087", ".287", ".387", ".NO87"),
    processor: _ => choice(".386", ".386p", ".486", ".486P", ".586", ".586P", ".686", ".686P", ".387"),

    byte_register: _ => choice("al", "ah", "cl", "ch", "dl", "dh", "bl", "bh", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"),
    gp_register: _ => choice("ax", "eax", "cx", "ecx", "dx", "edx", "bx", "ebx",  "di", "edi", "si", "esi", "bp", "ebp", "sp", "esp", "r8w", "r8d", "r9w", "r9d", "r12d", "r13w", "r13d", "r14w", "r14d"),
    qword_register: _ => choice("rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"),
    special_register: _ => choice("cr0", "cr2", "cr3", "dr0", "dr1", "dr2", "dr3", "dr6", "dr7", "tr3", "tr4", "tr5", "tr6", "tr7"),
    xmm_register: _ => choice("xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"),
    simd_register: $ => choice($.xmm_register, "mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7", "ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"),
    segment_register: _ => choice("cs", "ds", "es", "fs", "gs", "ss"),

    struct_hdr: _ => choice("struc", "struct", "union"),

    stack_option: _ => choice("nearstack", "farstack"),
    offset_type: _ => choice("group", "segment", "flat"),
    extern_key: _ => choice("extrn", "extern", "externdef"),
    for_dir: _ => choice("for", "irp"),
    forc_dir: _ => choice("forc", "irpc"),
    instr_prefix: _ => choice("rep", "repe", "repz", "repne", "repnz", "lock"),
    list_option: _ => choice(".list", ".nolist", ".xlist", ".listall", ".listif", ".lfcond", ".nolistif", ".sfcond", ".tfcond", ".listmacroall", ".lall", ".nolistmacro", ".sall", ".listmacro", ".xall"),
    mem_option: _ => choice("tiny", "small", "medium", "compact", "large", "huge", "flat"),
    near_far: _ => choice("near", "far"),
    o_visibility: _ => choice("public", "private", "export"),
    seg_align: _ => choice("byte", "word", "dword", "para", "page"),
    seg_order_dir: _ => choice(".alpha", ".seq", ".dosseg", "dosseg"),
    seg_ro: _ => "readonly",
    seg_size: _ => choice("use16", "use32", "flat"),
    title_type: _ => choice("title", "subtitle", "subttl"),
  }
});
