/**
 * @file Microsoft Macro Assembler x86 syntax
 * @author Taylor Plewe <tplewe@outlook.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const CHARACTER = /[^\n]/;
const NON_WHITESPACE_CHARACTER = /[^\s\n]/;
const ANY_CHAR_EXCEPT_QUOTE = /[^'"\n]/;
const ALPHA = /[a-zA-Z]|@|_|\$|\?/;
const DEC_DIGIT = /[0-9]/;
const DEC_NUMBER = /[0-9]+/;
const HEX_DIGIT = /[a-fA-F]/;
const DELIMITER = /\S/;
const DIGITS = /[0-9][0-9A-Fa-f]*/;
const RADIX_OVERRIDE = /[hoqtyHOQTY]/;
const TEXT = /\!?[^>\n]+/;
const CONSTANT = /[0-9][0-9A-Fa-f]*[hoqtyHOQTY]?/;
const IDENTIFIER = /[a-zA-Z@_$?][a-zA-Z0-9@_$?]*/;

// id aliases
const GROUP_ID = IDENTIFIER;
const TYPE_ID = IDENTIFIER;
const ALT_ID = IDENTIFIER;
const BIT_FIELD_ID = IDENTIFIER;
const MACRO_FUNC_ID = IDENTIFIER;
const MACRO_PROC_ID = IDENTIFIER;
const MACRO_ID = IDENTIFIER;
const MACRO_LABEL = IDENTIFIER;
const TEXT_MACRO_ID = IDENTIFIER;
const PARM_ID = IDENTIFIER;
const PROC_ID = IDENTIFIER;
const SEG_ID = IDENTIFIER;
const RECORD_TAG = IDENTIFIER;
const STRUCT_TAG = IDENTIFIER;
const UNION_TAG = IDENTIFIER;

const SIGN = /\+|\-/;
const BINARY_OP = /==|\!=|>=|<=|>|<|&/;
const ADD_OP = /\+|\-/;
const OR_OP = /or|xor/;
const MUL_OP = /\*|\/|mod/;
const REL_OP = /eq|ne|lt|le|gt|ge/;
const SHIFT_OP = /shr|shl/;
const QUOTE = /['"]/;
const BOOL = /true|false/;

const list = listItem => seq(listItem, repeat(seq(",", listItem)));
const listWithEol = (listItem, eol) => seq(listItem, repeat(seq(",", optional(eol), listItem)));

const PREC_ARR = [
  "logical_or", // cExpr
  "logical_and", // aExpr
  "logical_not", // term
  "paren", // simpleExpr
  "primary",
  "expr",
  "bitwise_or", // e01
  "bitwise_and", // e02
  "bitwise_not", // e03
  "comparitive", // e04
  "add", // e05
  "mul_shift", // e06
  "bit_section", // e08
  "offset", // e09
  "dot", // e10
  "e11",
];
const PREC = PREC_ARR.reduce((obj, key, index) => {
  obj[key] = index + 1;
  return obj;
}, {});

export default grammar({
  name: "masm",

  extras: $ => [
    /[ \t\s]+/,
    $.comment_line,
  ],

  conflicts: $ => [
    [$.register, $.assume_seg_reg],
    [$.proto_spec],
    [$.proto_list],
    [$.proto_arg_list],
    [$.qualifier],
    [$.prefix_expression, $.binary_expression],
  ],

  rules: {
    // NOTE: will be 'module'
    source_file: $ => $.text_macro_dir,

    eol: $ => choice($.comment_line, /\n/),


    // building blocks
    
    id_list: $ => list(IDENTIFIER),
    char_list: _ => repeat1(CHARACTER),
    stext: $ => repeat1(ANY_CHAR_EXCEPT_QUOTE),
    string: $ => seq($.quote, optional($.stext), $.quote, $.eol),
    text_literal: $ => seq("<", TEXT, ">", $.eol),
    exponent: $ => seq("e", optional($.sign), DEC_NUMBER),
    float_number: $ => choice(
      seq(optional($.sign), DEC_NUMBER, ".", optional(DEC_NUMBER), optional($.exponent)),
      seq(DIGITS, "r"),
    ),
    bcd_const: $ => seq(optional($.sign), DEC_NUMBER),


    // expressions

    expression: $ => choice(
      $.binary_expression,
      $.prefix_expression,
      prec(PREC.e11, $.expression_terminal),
    ),

    prefix_expression: $ => {
      const table = [
        [PREC.bitwise_not, "not"],
        [PREC.bit_section, /high|low|highword|lowword/],
        [PREC.offset, /offset|seg|lroffset|type|this/],
      ];

      return choice(...table.map(([precedence, prefix]) => prec(precedence, seq(
        field('prefix', prefix),
        field('right', $.expression),
      ))));
    },

    binary_expression: $ => {
      const table = [
        [PREC.logical_or, "||"],
        [PREC.logical_and, "&&"],
        [PREC.bitwise_or, OR_OP],
        [PREC.bitwise_and, "and"],
        [PREC.comparitive, REL_OP],
        [PREC.add, ADD_OP],
        [PREC.mul_shift, choice(MUL_OP, SHIFT_OP)],
        [PREC.offset, /ptr|:/],
        [PREC.dot, "."],
      ];

      return choice(...table.map(([precedence, operator]) => prec.left(precedence, seq(
        field('left', $.expression),
        field('operator', operator),
        field('right', $.expression),
      ))));
    },

    expression_terminal: $ => choice(
      seq("(", $.expression, ")"),
      seq("width", IDENTIFIER),
      seq("mask", IDENTIFIER),
      seq("size", $._size_arg),
      seq("sizeof", $._size_arg),
      seq("length", IDENTIFIER),
      seq("lengthof", IDENTIFIER),
      CONSTANT,
      $.string,
      $.type,
      prec(PREC.e11 + 1, IDENTIFIER),
      "$",
      $.register,
      prec(PREC.e11 + 1, "st"),
      seq("st", "(", $.expression, ")"),
    ),

    // c_expr: $ => choice($.a_expr, seq($.c_expr, "||", $.a_expr)),
    // a_expr: $ => choice($.term, seq($.a_expr, "&&", $.term)),
    // term: $ => choice($.simple_expr, seq("!", $.simple_expr)),
    // simple_expr: $ => choice($.primary, seq("(", $.c_expr, ")")),

      // binary_expression: $ => {
      //   const table = [
      //     [PREC.additive, choice('+', '-')],            // 2
      //     [PREC.shift, choice('<<', '>>')],             // 3
      //     [PREC.bitand, '&'], // 4
      //     [PREC.bitxor, '^'], // 5
      //     [PREC.bitor, '|'],  // 6
      //     [PREC.comparative, choice('==', '!=', '<', '<=', '>', '>=')], // 7
      //     [PREC.and, '&&'],   // 8
      //     [PREC.or, '||'],    // 9
      //     [PREC.multiplicative, choice('*', '/', '%')], // 10
      //   ];

      //   // @ts-ignore
      //   return choice(...table.map(([precedence, operator]) => prec.left(precedence, seq(
      //     field('left', $._expression),
      //     // @ts-ignore
      //     field('operator', operator),
      //     field('right', $._expression),
      //   ))));

      //   // WHICH BECOMES
      //   choice(
      //     prec.left(2, seq(field('left', $._expression), field('operator', choice('+', '-'), field('right', $._expression)))),
      //     prec.left(3, seq(field('left', $._expression), field('operator', choice('<<', '>>'), field('right', $._expression)))),
      //   ),
      // },

    /*
    masm bnf grammar hierarchy:

      cExpr
        aExpr | cExpr || aExpr

      aExpr
        term | aExpr && term

      term
        simpleExpr | ! simpleExpr
      
      simpleExpr
        ( cExpr ) | primary

      primary
        expr binaryOp expr | flagName | expr

      expr
        SHORT e05
        | .TYPE e01
        | OPATTR e01
        | e01
        
      e01
        e01 orOp e02 | e02

      e02
        e02 AND e03 | e03

      e03
        NOT e04 | e04

      e04
        e04 relOp e05 | e05

      e05
        e05 addOp e06 | e06

      e06
        e06 mulOp e07 | e06 shiftOp e07 | e07

      e07
        e07 addOp e08 | e08

      e08
        HIGH e09
        | LOW e09
        | HIGHWORD e09
        | LOWWORD e09
        | e09

      e09
        OFFSET e10
        | SEG e10
        | LROFFSET e10
        | TYPE e10
        | THIS e10
        | e09 PTR e10
        | e09 : e10
        | e10

      e10
        e10 . e11
        | e10 ⟦ expr ⟧
        | e11

      e11
        ( expr )
        | ⟦ expr ⟧
        | WIDTH id
        | MASK id
        | SIZE sizeArg
        | SIZEOF sizeArg
        | LENGTH id
        | LENGTHOF id
        | recordConst
        | string
        | constant
        | type
        | id
        | $
        | segmentRegister
        | register
        | ST
        | ST ( expr )    // masm bnf grammar hierarchy:
        
    OTHER

      cxzExpr
        expr
        | ! expr
        | expr == expr
        | expr != expr

      constExpr
        expr

      immExpr
        expr

      exprList
        expr | exprList , expr
    */


    // uses expresions

    expr_list: $ => list($.expression),
    eq_dir: $ => seq(IDENTIFIER, "=", $.expression, $.eol),
    _bit_field_size: $ => alias($.expression, $.bit_field_size),
    bit_def: $ => seq(BIT_FIELD_ID, ":", $._bit_field_size, optional(seq("=", $.expression))),
    bit_def_list: $ => listWithEol($.bit_def, $.eol),
    record_dir: $ => seq(RECORD_TAG, "record", $.bit_def_list, $.eol),

    _comm_type: $ => alias($.expression, $.comm_type),
    comm_decl: $ => seq(optional($.near_far), optional($.lang_type), IDENTIFIER, ":", $._comm_type, optional(seq(":", $.expression))),
    comm_list: $ => list($.comm_decl),
    comm_dir: $ => seq("comm", $.comm_list, $.eol),

    init_value: $ => choice(
      // $.string, // handled by expression
      "?",
      seq($.expression, optional(seq("dup", "(", $.scalar_inst_list, ")"))),
      $.float_number,
      $.bcd_const,
    ),
    scalar_inst_list: $ => listWithEol($.init_value, $.eol),

    struct_instance: $ => choice(
      seq("<", optional($.field_init_list), ">"),
      seq("{", optional($.eol), optional($.field_init_list), optional($.eol), "}"),
      seq($.expression, "dup", "(", $.struct_inst_list, ")"),
    ),
    struct_inst_list: $ => listWithEol($.struct_instance, $.eol),
    field_init: $ => choice($.init_value, $.struct_instance),
    field_init_list: $ => listWithEol($.field_init, $.eol),

    record_field_list: $ => listWithEol($.expression, $.eol),
    old_record_field_list: $ => list($.expression),
    record_instance: $ => choice(
      seq("{", optional($.eol), $.record_field_list, optional($.eol), "}"),
      seq("<", $.old_record_field_list, ">"),
      seq($.expression, "dup", "(", $.record_instance, ")"),
    ),
    record_inst_list: $ => listWithEol($.record_instance, $.eol),
    record_const: $ => choice(
      seq(RECORD_TAG, "{", $.old_record_field_list, "}"),
      seq(RECORD_TAG, "<", $.old_record_field_list, ">"),
    ),

    data_item: $ => choice(
      seq($.data_decl, $.scalar_inst_list),
      seq(IDENTIFIER, $.struct_inst_list),
      seq(RECORD_TAG, $.record_inst_list),
    ),
    data_dir: $ => seq(optional(IDENTIFIER), $.data_item, $.eol),

    seg_dir: $ => choice(
      seq(".code", optional(SEG_ID)),
      ".data",
      ".data?",
      ".const",
      seq(".fardata", optional(SEG_ID)),
      seq(".fardata?", optional(SEG_ID)),
      seq(".stack", optional($.expression)),
    ),
    simple_seg_dir: $ => seq($.seg_dir, $.eol),

    text_item: $ => choice($.text_literal, TEXT_MACRO_ID, seq("%", $.expression)),
    text_list: $ => listWithEol($.text_item, $.eol),
    _text_len: $ => alias($.expression, $.text_len),
    _text_start: $ => alias($.expression, $.text_start),
    text_macro_dir: $ => choice(
      seq("catstr", optional($.text_list)),
      seq("textequ", optional($.text_list)),
      seq("sizestr", $.text_item),
      seq("substr", $.text_item, ",", $._text_start, optional(seq(",", $._text_len))),
      seq("instr", optional(seq($._text_start, ",")), $.text_item, ",", $.text_item),
    ),
    text_dir: $ => seq(IDENTIFIER, $.text_macro_dir, $.eol),

    // masm bnf grammar error: missing a | to indicate choice
    until_dir: $ => choice(
      seq(".until", $.expression, $.eol),
      seq(".untilcxz", optional($.expression), $.eol),
    ),

    offset_dir_type: $ => choice(
      "even",
      seq("org", $.expression),
      seq("align", optional($.expression)),
    ),
    offset_dir: $ => seq($.offset_dir_type, $.eol),

    struct_item: $ => choice(
      $.data_dir,
      // $.general_dir,
      $.offset_dir,
      $.nested_struct,
    ),
    nested_struct: $ => seq($.struct_hdr, optional(IDENTIFIER), $.eol, $.struct_body, "ends", $.eol),
    struct_body: $ => repeat1(seq($.struct_item, $.eol)),


    // idk

    cref_option: $ => choice(
      ".cref",
      seq(".xcref", optional($.id_list)),
      seq(".nocref", optional($.id_list)),
    ),
    cref_dir: $ => seq($.cref_option, $.eol),
    data_decl: $ => choice("db", "dw", "dd", "df", "dq", "dt", $.data_type, TYPE_ID),
    distance: $ => choice($.near_far, "near16", "near32", "far16", "far32"),
    type: $ => choice(
      IDENTIFIER, // STRUCT_TAG, UNION_TAG, RECORD_TAG, TYPE_ID
      $.distance,
      $.data_type,
    ),
    qualified_type: $ => choice(
      $.type,
      seq(optional($.distance), "ptr", optional($.qualified_type)),
    ),

    proto_arg: $ => seq(optional(IDENTIFIER), ":", $.qualified_type),
    // proto_arg_list: $ => choice(
    //   seq(
    //     seq(",", optional($.eol), $.proto_list),
    //     optional(seq(",", optional($.eol), optional(IDENTIFIER), ":vararg")),
    //   ),
    // ),
    // proto_arg_list_scalar: $ => seq(",", optional($.eol), $.proto_list),
    // proto_arg_list_vararg: $ => seq(",", optional($.eol), optional(IDENTIFIER), ":vararg"),
    proto_list: $ => listWithEol($.proto_arg, $.eol),
    proto_arg_list: $ => seq(
      optional(seq(",", optional($.eol))),
      choice(
        seq($.proto_list, optional(seq(",", optional($.eol), optional(IDENTIFIER), ":vararg"))),
        seq(optional(IDENTIFIER), ":vararg"),
      ),
    ),
    proto_spec: $ => choice(
      // the masm bnf grammar has the following as a possible `protoSpec` rule:
      //   ⟦distance⟧ ⟦langType⟧ ⟦protoArgList⟧
      // the problem is that this can match an empty string. The following is how I got around that
      choice(
        seq($.distance, optional($.lang_type), optional($.proto_arg_list)),
        seq($.lang_type, optional($.proto_arg_list)),
        $.proto_arg_list,
      ),
      TYPE_ID,
    ),
    proto_type_dir: $ => seq(IDENTIFIER, "proto", optional($.proto_spec)),

    // proc_parm_list: $ => seq(
    //   optional(seq(",", optional($.eol))),
    //   choice(
    //     seq($.parm_list, optional(seq(",", optional($.eol), optional(PARM_ID), ":vararg"))),
    //     seq(optional(PARM_ID), ":vararg"),
    //   ),
    // ),

    pub_def: $ => seq(optional($.lang_type), IDENTIFIER),
    pub_list: $ => listWithEol($.pub_def, $.eol),
    public_dir: $ => seq("public", $.pub_list, $.eol),

    macro_id: $ => choice(MACRO_PROC_ID, MACRO_FUNC_ID),
    macro_id_list: $ => list(MACRO_ID),

    purge_dir: $ => seq("purge", $.macro_id_list),

    parm_type: $ => choice(
      "req",
      seq("=", $.text_literal),
      "vararg",
    ),
    macro_parm: $ => seq(IDENTIFIER, optional(seq(":", $.parm_type))),
    macro_parm_list: $ => listWithEol($.macro_parm, $.eol),

    model_opt: $ => choice($.lang_type, $.stack_option),
    model_opt_list: $ => list($.model_opt),
    model_dir: $ => seq(".model", $.mem_option, optional(seq(",", $.model_opt_list)), $.eol),

    name_dir: $ => seq("name", IDENTIFIER, $.eol),

    p_options: $ => choice(
      seq($.distance, optional($.lang_type), optional($.o_visibility)),
      seq($.lang_type, optional($.o_visibility)),
      $.o_visibility,
    ),

    for_parm_type: $ => choice("req", seq("=", $.text_literal)),
    for_parm: $ => seq(IDENTIFIER, optional(seq(":", $.for_parm_type))),

    frame_expr: $ => choice(
      seq("seg", IDENTIFIER),
      seq("dgroup", ":", IDENTIFIER),
      seq($.segment_register, ":", IDENTIFIER),
      IDENTIFIER,
    ),

    seg_id_list: $ => list(SEG_ID),
    group_dir: $ => seq(GROUP_ID, "group", $.seg_id_list),

    file_char_list: $ => repeat1($.file_char),
    file_spec: $ => choice($.file_char_list, $.text_literal),
    include_dir: $ => seq("include", $.file_spec, $.eol),
    include_lib_dir: $ => seq("includelib", $.file_spec, $.eol),

    label_def: $ => choice(
      seq(IDENTIFIER, ":"),
      seq(IDENTIFIER, "::"),
      "@@:",
    ),
    label_dir: $ => seq(IDENTIFIER, "label", $.qualified_type, $.eol),

    qualifier: $ => choice(
      $.qualified_type,
      seq("proto", optional($.proto_spec)),
    ),
    typedef_dir: $ => seq(TYPE_ID, "typedef", $.qualifier),

    // NOTE: I think the bnf grammar might be incorrect. There's no way to get from "extern" to "proto" in the grammar.
    // If instead of `qualified_type`, it was `qualifier`, then it would make sense.
    extern_type: $ => choice("abs", $.qualifier), 
    extern_def: $ => seq(optional($.lang_type), IDENTIFIER, optional(seq("(", ALT_ID, ")")), ":", $.extern_type),
    extern_list: $ => listWithEol($.extern_def, $.eol),
    extern_dir: $ => seq($.extern_key, $.extern_list, $.eol),

    assume_val: $ => choice($.qualified_type, "nothing", "error"),
    assume_seg_val: $ => choice($.frame_expr, "nothing", "error"),
    assume_seg_reg: $ => seq($.segment_register, ":", $.assume_seg_val),
    assume_register: $ => choice($.assume_seg_reg, $.assume_reg),
    assume_reg: $ => seq($.register, ":", $.assume_val),
    assume_list: $ => list($.assume_register),
    assume_dir: $ => choice(
      seq("assume", $.assume_list, $.eol),
      seq("assume nothing", $.eol),
    ),

    // another error in the bnf grammar--it omits an `|` indicating a choice between "echo" and "%out"
    echo_dir: $ => choice(
      seq("echo", $.arbitrary_text, $.eol),
      seq("%out", $.arbitrary_text, $.eol),
    ),

    list_dir: $ => seq($.list_option, $.eol),

    local_def: $ => seq("local", $.id_list, $.eol),
    local_list: $ => repeat1($.local_def),

    // fpu_register: $ => seq("st", $.expr), // TODO
    fpu_register: $ => seq("st"), // TODO
    register: $ => choice(
      $.special_register,
      $.gp_register,
      $.byte_register,
      $.qword_register,
      $.fpu_register,
      $.simd_register,
      $.segment_register
    ),
    reg_list: $ => repeat1($.register),

    seg_attrib: $ => choice(
      "public",
      "stack",
      "common",
      "memory",
      "at", // TODO: constExpr
      "private",
    ),
    seg_option: $ => choice(
      $.seg_align,
      $.seg_ro,
      $.seg_attrib,
      $.seg_size,
      $.class_name,
    ),
    seg_option_list: $ => repeat1($.seg_option),
    segment_dir: $ => seq(SEG_ID, "segment", optional($.seg_option_list), $.eol),

    context_dir: $ => choice(
      seq("pushcontext", $.context_item_list, $.eol),
      seq("popcontext", $.context_item_list, $.eol),
    ),

    endp_dir: $ => seq(PROC_ID, "endp", $.eol),
    ends_dir: $ => seq(IDENTIFIER, "ends", $.eol),
    exitm_dir: $ => choice(
      seq(":", "exitm"),
      seq("exitm", $.text_item),
    ),

    special_chars: _ => /:|\.|\[|\]|\(|\)|<|>|\{|\}|\+|-|\/|\*|&|%|!|'|\\|=|;|,|"|\s|\n/,

    startup_dir: $ => seq(".startup", $.eol),

    uses_regs: $ => seq("uses", $.reg_list),

    // size_arg: $ => choice(prec(1, IDENTIFIER), $.type), // TODO: and "e10"
    _size_arg: $ => alias($.expression, $.size_arg),


    // option

    option_item: $ => choice(
      seq("casemap", ":", $.map_type),
      "dotname",
      "nodotname",
      "emulator",
      "noemulator",
      seq("epilogue", ":", MACRO_ID),
      "expr16",
      "expr32",
      seq("language", ":", $.lang_type),
      "ljmp",
      "noljmp",
      "m510",
      "nom510",
      seq("nokeyword", ":", "<", $.id_list, ">"), // NOTE: in the bnf grammar this is a "keyword list"--a list of "keywords"--which are just "any reserved word". Might come back and properly implmenet that later, but this is sufficient for now
      "nosignextend",
      seq("offset", ":", $.offset_type),
      "oldmacros",
      "nooldmacros",
      "oldstructs",
      "nooldstructs",
      seq("proc", ":", $.o_visibility),
      seq("prologue", ":", MACRO_ID),
      "readonly",
      "noreadonly",
      "scoped",
      "noscoped",
      seq("segment", ":", $.seg_size),
      seq("setif2", ":", $.bool),
    ),
    option_list: $ => listWithEol($.option_item, $.eol),
    option_dir: $ => seq("option", $.option_list, $.eol),


    // aliases

    arbitrary_text: $ => alias($.char_list, $.arbitrary_text),
    class_name: $ => alias($.string, $.class_name),
    file_char: _ => DELIMITER,


    // terminals

    context_item: _ => choice("assumes", "radix", "listing", "cpu", "all"),
    context_item_list: $ => list($.context_item),
    data_type: _ => choice("byte", "sbyte", "word", "sword", "dword", "sdword", "fword", "qword", "sqword", "tbyte", "oword", "real4", "real8", "real10", "mmword", "xmmword", "ymmword"),

    sign: _ => choice("+", "-"),
    binary_op: _ => choice("==", "!=", ">=", "<=", ">", "<", "&"),
    add_op: _ => choice("+", "-"),
    or_op: _ => choice("or", "xor"),
    mul_op: _ => choice("*", "/", "mod"),
    rel_op: _ => choice("eq", "ne", "lt", "le", "gt", "ge"),
    shift_op: _ => choice("shr", "shl"),
    quote: _ => choice(`"`, "'"),
    bool: _ => choice("true", "false"),

    processor: _ => choice(".386", ".386p", ".486", ".486P", ".586", ".586P", ".686", ".686P", ".387"),
    coprocessor: _ => choice(".8087", ".287", ".387", ".NO87"),
    processor_dir: $ => choice(
      seq($.processor, $.eol),
      seq($.coprocessor, $.eol),
    ),

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
    repeat_dir: _ => choice("repeat", "rept"),
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
    lang_type: _ => choice("c", "pascal", "fortran", "basic", "syscall", "stdcall"),
    map_type: _ => choice("all", "none", "notpublic"),
    flag_name: _ => choice("zero?", "carry?", "overflow?", "sign?", "parity?"),


    // comments

    comment_line: _ => /;.*/,

    // TODO: MASM's COMMENT directive is gross and probably requires an external scanner to properly parse it
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq(TEXT, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, TEXT, $.eol),
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq(TEXT, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, TEXT, $.eol),
    // comment_dir: _ => /comment\s+(\S)\n([^\n]*\n)*$1/,
  }
});
