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
const HEX_DIGIT = /[a-fA-F]/;
const DELIMITER = /\S/;

const list = listItem => seq(listItem, repeat(seq(",", listItem)));
const listWithEol = (listItem, eol) => seq(listItem, repeat(seq(",", optional(eol), listItem)));

const PREC = {
  logical_or: 1,
  logical_and: 2,
  logical_not: 3,
};

export default grammar({
  name: "masm",

  extras: $ => [
    /[ \t\s]+/,
    $.comment_line,
  ],

  conflicts: $ => [
    [$.struct_tag, $.union_tag, $.record_tag, $.type_id],
    [$.register, $.assume_seg_reg],
    [$.proto_spec],
    [$.proto_list],
    [$.proto_arg_list],
    [$.qualifier],
    [$.macro_proc_id, $.macro_func_id],
    [$.id],
    [$.digits],
    [$.e11, $.fpu_register],
  ],

  rules: {
    // NOTE: will be 'module'
    source_file: $ => $.c_expr,

    eol: $ => choice($.comment_line, /\n/),


    // building blocks
    
    dec_number: _ => repeat1(DEC_DIGIT),
    id: $ => seq(ALPHA, repeat(choice(ALPHA, DEC_DIGIT))),
    id_list: $ => list($.id),
    char_list: _ => repeat1(CHARACTER),
    stext: $ => repeat1(ANY_CHAR_EXCEPT_QUOTE),
    string: $ => seq($.quote, optional($.stext), $.quote, $.eol),
    text: $ => seq(optional("!"), repeat1(CHARACTER)), // TODO: the MASM BNF grammar actually states that "text" can itself contain "text_literal"s
    text_literal: $ => seq("<", $.text, ">", $.eol),
    digits: _ => seq(DEC_DIGIT, repeat(choice(DEC_DIGIT, HEX_DIGIT))),
    exponent: $ => seq("e", optional($.sign), $.dec_number),
    float_number: $ => choice(
      seq(optional($.sign), $.dec_number, ".", optional($.dec_number), optional($.exponent)),
      seq($.digits, "r"),
    ),
    bcd_const: $ => seq(optional($.sign), $.dec_number),
    constant: $ => seq($.digits, optional($.radix_override)),


    // expressions

    c_expr: $ => choice($.a_expr, seq($.c_expr, "||", $.a_expr)),
    a_expr: $ => choice($.term, seq($.a_expr, "&&", $.term)),
    term: $ => choice($.simple_expr, seq("!", $.simple_expr)),
    simple_expr: $ => choice(seq("(", $.c_expr, ")"), $.primary),
    primary: $ => choice(
      seq($.expr, $.binary_op, $.expr),
      $.flag_name,
      $.expr,
    ),
    expr: $ => choice(
      seq("short", $.e05),
      seq(".type", $.e01),
      seq("opattr", $.e01),
      $.e01,
    ),
    e01: $ => choice(seq($.e01, $.or_op, $.e02), $.e02),
    e02: $ => choice(seq($.e02, "and", $.e03), $.e03),
    e03: $ => choice(seq("not", $.e04), $.e04),
    e04: $ => choice(seq($.e04, $.rel_op, $.e05), $.e05),
    e05: $ => choice(seq($.e05, $.add_op, $.e06), $.e06),
    e06: $ => choice(
      seq($.e06, $.mul_op, $.e07),
      seq($.e06, $.shift_op, $.e07),
      $.e07,
    ),
    e07: $ => choice(seq($.e07, $.add_op, $.e08), $.e08),
    e08: $ => choice(
      seq("high", $.e09),
      seq("low", $.e09),
      seq("highword", $.e09),
      seq("lowword", $.e09),
      $.e09,
    ),
    e09: $ => choice(
      seq("offset", $.e10),
      seq("seg", $.e10),
      seq("lroffset", $.e10),
      seq("type", $.e10),
      seq("this", $.e10),
      seq($.e09, "ptr", $.e10),
      seq($.e09, ":", $.e10),
      $.e10,
    ),
    e10: $ => choice(
      seq($.e10, ".", $.e11),
      seq($.e10, optional($.expr)),
      $.e11,
    ),
    e11: $ => choice(
      seq("(", $.expr, ")"),
      seq("width", $.id),
      seq("mask", $.id),
      seq("length", $.id),
      seq("lengthof", $.id),
      $.string,
      $.constant,
      $.type,
      $.id,
      "$",
      $.segment_register,
      $.register,
      "st",
      seq("st", "(", $.expr, ")"),
    ),

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


    THE WAY RUST TREE SITTER DOES IT

      binary_expression: $ => {
        const table = [
          [PREC.additive, choice('+', '-')],            // 2
          [PREC.shift, choice('<<', '>>')],             // 3
          [PREC.bitand, '&'], // 4
          [PREC.bitxor, '^'], // 5
          [PREC.bitor, '|'],  // 6
          [PREC.comparative, choice('==', '!=', '<', '<=', '>', '>=')], // 7
          [PREC.and, '&&'],   // 8
          [PREC.or, '||'],    // 9
          [PREC.multiplicative, choice('*', '/', '%')], // 10
        ];

        // @ts-ignore
        return choice(...table.map(([precedence, operator]) => prec.left(precedence, seq(
          field('left', $._expression),
          // @ts-ignore
          field('operator', operator),
          field('right', $._expression),
        ))));

        // WHICH BECOMES
        choice(
          prec.left(2, seq(field('left', $._expression), field('operator', choice('+', '-'), field('right', $._expression)))),
          prec.left(3, seq(field('left', $._expression), field('operator', choice('<<', '>>'), field('right', $._expression)))),
        ),
      },

      const PREC = {
        call: 15,
        field: 14,
        try: 13,
        unary: 12,
        cast: 11,
        multiplicative: 10,
        additive: 9,
        shift: 8,
        bitand: 7,
        bitxor: 6,
        bitor: 5,
        comparative: 4,
        and: 3,
        or: 2,
        range: 1,
        assign: 0,
        closure: -1,
      };

    RUBY TREE SITTER

      const PREC = {
        comment: -2,
        curly_block: 1,
        do_block: -1,

        and: -2,
        or: -2,
        not: 5,
        defined: 10,
        alias: 11,
        assign: 15,
        rescue: 16,
        conditional: 20,
        range: 25,
        boolean_or: 30,
        boolean_and: 35,
        relational: 40,
        comparison: 45,
        bitwise_or: 50,
        bitwise_and: 55,
        call: 56,
        shift: 60,
        additive: 65,
        multiplicative: 70,
        unary_minus: 75,
        exponential: 80,
        complement: 85,
      };

      binary: $ => {
        const operators = [
          [prec.left, PREC.AND, 'and'],
          [prec.left, PREC.OR, 'or'],
          [prec.left, PREC.BOOLEAN_OR, '||'],
          [prec.left, PREC.BOOLEAN_AND, '&&'],
          [prec.left, PREC.SHIFT, choice('<<', '>>')],
          [prec.left, PREC.COMPARISON, choice('<', '<=', '>', '>=')],
          [prec.left, PREC.BITWISE_AND, '&'],
          [prec.left, PREC.BITWISE_OR, choice('^', '|')],
          [prec.left, PREC.ADDITIVE, choice('+', alias($._binary_minus, '-'))],
          [prec.left, PREC.MULTIPLICATIVE, choice('/', '%', alias($._binary_star, '*'))],
          [prec.right, PREC.RELATIONAL, choice('==', '!=', '===', '<=>', '=~', '!~')],
          [prec.right, PREC.EXPONENTIAL, alias($._binary_star_star, '**')],
        ];

        // @ts-ignore
        return choice(...operators.map(([fn, precedence, operator]) => fn(precedence, seq(
          field('left', $._arg),
          // @ts-ignore
          field('operator', operator),
          field('right', $._arg),
        ))));
      },

      command_binary: $ => prec.left(seq(
        field('left', $._expression),
        field('operator', choice('or', 'and')),
        field('right', $._expression),
      )),

      unary: $ => {
        const operators = [
          [prec, PREC.DEFINED, 'defined?'],
          [prec.right, PREC.NOT, 'not'],
          [prec.right, PREC.UNARY_MINUS, choice(alias($._unary_minus, '-'), alias($._binary_minus, '-'), '+')],
          [prec.right, PREC.COMPLEMENT, choice('!', '~')],
        ];
        // @ts-ignore
        return choice(...operators.map(([fn, precedence, operator]) => fn(precedence, seq(
          // @ts-ignore
          field('operator', operator),
          field('operand', $._arg),
        ))));
      },

      command_unary: $ => {
        const operators = [
          [prec, PREC.DEFINED, 'defined?'],
          [prec.right, PREC.NOT, 'not'],
          [prec.right, PREC.UNARY_MINUS, choice(alias($._unary_minus, '-'), '+')],
          [prec.right, PREC.COMPLEMENT, choice('!', '~')],
        ];
        // @ts-ignore
        return choice(...operators.map(([fn, precedence, operator]) => fn(precedence, seq(
          // @ts-ignore
          field('operator', operator),
          field('operand', $._expression),
        ))));
      },

      parenthesized_unary: $ => prec(PREC.CALL, seq(
        field('operator', choice('defined?', 'not')),
        field('operand', $.parenthesized_statements),
      )),

      unary_literal: $ => prec.right(PREC.UNARY_MINUS, seq(
        field('operator', choice(alias($._unary_minus_num, '-'), '+')),
        field('operand', $._simple_numeric),
      )),
        
    */


    // idk

    cref_option: $ => choice(
      ".cref",
      seq(".xcref", optional($.id_list)),
      seq(".nocref", optional($.id_list)),
    ),
    data_decl: $ => choice("db", "dw", "dd", "df", "dq", "dt", $.data_type, $.type_id),
    distance: $ => choice($.near_far, "near16", "near32", "far16", "far32"),
    type: $ => choice(
      $.struct_tag,
      $.union_tag,
      $.record_tag,
      $.distance,
      $.data_type,
      $.type_id,
    ),
    qualified_type: $ => choice(
      $.type,
      seq(optional($.distance), "ptr", optional($.qualified_type)),
    ),

    proto_arg: $ => seq(optional($.id), ":", $.qualified_type),
    // proto_arg_list: $ => choice(
    //   seq(
    //     seq(",", optional($.eol), $.proto_list),
    //     optional(seq(",", optional($.eol), optional($.id), ":vararg")),
    //   ),
    // ),
    // proto_arg_list_scalar: $ => seq(",", optional($.eol), $.proto_list),
    // proto_arg_list_vararg: $ => seq(",", optional($.eol), optional($.id), ":vararg"),
    proto_list: $ => listWithEol($.proto_arg, $.eol),
    proto_arg_list: $ => seq(
      optional(seq(",", optional($.eol))),
      choice(
        seq($.proto_list, optional(seq(",", optional($.eol), optional($.id), ":vararg"))),
        seq(optional($.id), ":vararg"),
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
      $.type_id,
    ),
    proto_type_dir: $ => seq($.id, "proto", optional($.proto_spec)),

    // proc_parm_list: $ => seq(
    //   optional(seq(",", optional($.eol))),
    //   choice(
    //     seq($.parm_list, optional(seq(",", optional($.eol), optional($.parm_id), ":vararg"))),
    //     seq(optional($.parm_id), ":vararg"),
    //   ),
    // ),

    pub_def: $ => seq(optional($.lang_type), $.id),
    pub_list: $ => listWithEol($.pub_def, $.eol),
    public_dir: $ => seq("public", $.pub_list, $.eol),

    macro_id: $ => choice($.macro_proc_id, $.macro_func_id),
    macro_id_list: $ => list($.macro_id),

    purge_dir: $ => seq("purge", $.macro_id_list),

    parm_type: $ => choice(
      "req",
      seq("=", $.text_literal),
      "vararg",
    ),
    macro_parm: $ => seq($.id, optional(seq(":", $.parm_type))),
    macro_parm_list: $ => listWithEol($.macro_parm, $.eol),

    model_opt: $ => choice($.lang_type, $.stack_option),
    model_opt_list: $ => list($.model_opt),
    model_dir: $ => seq(".model", $.mem_option, optional(seq(",", $.model_opt_list)), $.eol),

    name_dir: $ => seq("name", $.id, $.eol),

    p_options: $ => choice(
      seq($.distance, optional($.lang_type), optional($.o_visibility)),
      seq($.lang_type, optional($.o_visibility)),
      $.o_visibility,
    ),

    for_parm_type: $ => choice("req", seq("=", $.text_literal)),
    for_parm: $ => seq($.id, optional(seq(":", $.for_parm_type))),

    frame_expr: $ => choice(
      seq("seg", $.id),
      seq("dgroup", ":", $.id),
      seq($.segment_register, ":", $.id),
      $.id,
    ),

    seg_id_list: $ => list($.seg_id),
    group_dir: $ => seq($.group_id, "group", $.seg_id_list),

    file_char_list: $ => repeat1($.file_char),
    file_spec: $ => choice($.file_char_list, $.text_literal),
    include_dir: $ => seq("include", $.file_spec, $.eol),
    include_lib_dir: $ => seq("includelib", $.file_spec, $.eol),

    label_def: $ => choice(
      seq($.id, ":"),
      seq($.id, "::"),
      "@@:",
    ),
    label_dir: $ => seq($.id, "label", $.qualified_type, $.eol),

    qualifier: $ => choice(
      $.qualified_type,
      seq("proto", optional($.proto_spec)),
    ),
    typedef_dir: $ => seq($.type_id, "typedef", $.qualifier),

    // NOTE: I think the bnf grammar might be incorrect. There's no way to get from "extern" to "proto" in the grammar.
    // If instead of `qualified_type`, it was `qualifier`, then it would make sense.
    extern_type: $ => choice("abs", $.qualifier), 
    extern_def: $ => seq(optional($.lang_type), $.id, optional(seq("(", $.alt_id, ")")), ":", $.extern_type),
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
    fpu_register: $ => seq("st"),
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
    segment_dir: $ => seq($.seg_id, "segment", optional($.seg_option_list), $.eol),

    context_dir: $ => choice(
      seq("pushcontext", $.context_item_list, $.eol),
      seq("popcontext", $.context_item_list, $.eol),
    ),

    endp_dir: $ => seq($.proc_id, "endp", $.eol),
    ends_dir: $ => seq($.id, "ends", $.eol),
    // exitm_dir: $ => choice(
    //   seq(":", "exitm"),
    //   seq("exitm", $.text_item),
    // )

    special_chars: _ => /:|\.|\[|\]|\(|\)|<|>|\{|\}|\+|-|\/|\*|&|%|!|'|\\|=|;|,|"|\n/,

    startup_dir: $ => seq(".startup", $.eol),

    uses_regs: $ => seq("uses", $.reg_list),


    // option

    option_item: $ => choice(
      seq("casemap", ":", $.map_type),
      "dotname",
      "nodotname",
      "emulator",
      "noemulator",
      seq("epilogue", ":", $.macro_id),
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
      seq("prologue", ":", $.macro_id),
      "readonly",
      "noreadonly",
      "scoped",
      "noscoped",
      seq("segment", ":", $.seg_size),
      seq("setif2", ":", $.bool),
    ),
    option_list: $ => listWithEol($.option_item, $.eol),
    option_dir: $ => seq("option", $.option_list, $.eol),


    // id aliases

    group_id: $ => alias($.id, $.group_id),
    type_id: $ => alias($.id, $.type_id),
    alt_id: $ => alias($.id, $.alt_id),
    bit_field_id: $ => alias($.id, $.bit_field_id),
    macro_func_id: $ => alias($.id, $.macro_func_id),
    macro_label: $ => alias($.id, $.macro_label),
    macro_proc_id: $ => alias($.id, $.macro_proc_id),
    text_macro_id: $ => alias($.id, $.text_macro_id),
    parm_id: $ => alias($.id, $.parm_id),
    proc_id: $ => alias($.id, $.proc_id),
    seg_id: $ => alias($.id, $.seg_id),
    record_tag: $ => alias($.id, $.record_tag),
    struct_tag: $ => alias($.id, $.struct_tag),
    union_tag: $ => alias($.id, $.union_tag),


    // other aliases

    arbitrary_text: $ => alias($.char_list, $.arbitrary_text),
    class_name: $ => alias($.string, $.class_name),
    file_char: _ => DELIMITER,


    // terminals

    context_item: _ => choice("assumes", "radix", "listing", "cpu", "all"),
    context_item_list: $ => list($.context_item),
    data_type: _ => choice("byte", "sbyte", "word", "sword", "dword", "sdword", "fword", "qword", "sqword", "tbyte", "oword", "real4", "real8", "real10", "mmword", "xmmword", "ymmword"),
    radix_override: _ => choice("h", "o", "q", "t", "y"),

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
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq($.text, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, $.text, $.eol),
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq($.text, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, $.text, $.eol),
    // comment_dir: _ => /comment\s+(\S)\n([^\n]*\n)*$1/,
  }
});
