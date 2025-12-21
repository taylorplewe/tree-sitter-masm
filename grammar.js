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
    [$.struct_body],
    [$.init_value, $.record_field_list],
    [$.init_value, $.old_record_field_list],
    [$.expression_terminal, $.macro_arg],
    [$.in_seg_dir_list],
    [$.segment_def],
    [$.parm_list],
  ],

  rules: {
    // NOTE: will be 'module'
    source_file: $ => $.module,

    module: $ => seq($.directive_list, $.end_dir),
    end_dir: $ => seq("end", optional($.expression), $.eol),

    eol: $ => choice($.comment_line, /\n/),
    comment_line: _ => /;.*/,


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


    // mnemonic: _ => choice("aaa", "aad", "aam", "aas", "adc", "adcx", "add", "addpd", "addps", "addsd", "addss", "addsubpd", "addsubps", "adox", "aesdec", "aesdeclast", "aesenc", "aesenclast", "aesimc", "aeskeygenassist", "and", "andn", "andnpd", "andnps", "andpd", "andps", "bextr", "blcfill", "blci", "blcic", "blcmsk", "blcs", "blendpd", "blendps", "blendvpd", "blendvps", "blsfill", "blsi", "blsic", "blsmsk", "blsr", "bndcl", "bndcn", "bndcu", "bndldx", "bndmk", "bndmov", "bndstx", "bsf", "bsr", "bswap", "bt", "btc", "btr", "bts", "bzhi", "call", "cbw", "cdq", "cdqe", "clac", "clc", "cld", "clflush", "clflushopt", "clwb", "clzero", "cmc", "cmova", "cmovae", "cmovb", "cmovbe", "cmovc", "cmove", "cmovg", "cmovge", "cmovl", "cmovle", "cmovna", "cmovnae", "cmovnb", "cmovnbe", "cmovnc", "cmovne", "cmovng", "cmovnge", "cmovnl", "cmovnle", "cmovno", "cmovnp", "cmovns", "cmovnz", "cmovo", "cmovp", "cmovpe", "cmovpo", "cmovs", "cmovz", "cmp", "cmppd", "cmpps", "cmpsb", "cmpsd", "cmpsq", "cmpss", "cmpsw", "cmpxchg", "cmpxchg16b", "cmpxchg8b", "comisd", "comiss", "cpuid", "cqo", "crc32", "cvtdq2pd", "cvtdq2ps", "cvtpd2dq", "cvtpd2pi", "cvtpd2ps", "cvtpi2pd", "cvtpi2ps", "cvtps2dq", "cvtps2pd", "cvtps2pi", "cvtsd2si", "cvtsd2ss", "cvtsi2sd", "cvtsi2ss", "cvtss2sd", "cvtss2si", "cvttpd2dq", "cvttpd2pi", "cvttps2dq", "cvttps2pi", "cvttsd2si", "cvttss2si", "cwd", "cwde", "daa", "das", "dec", "div", "divpd", "divps", "divsd", "divss", "dppd", "dpps", "emms", "enter", "extractps", "extrq", "f2xm1", "fabs", "fadd", "faddp", "fbld", "fbstp", "fchs", "fclex", "fcmovb", "fcmovbe", "fcmove", "fcmovnb", "fcmovnbe", "fcmovne", "fcmovnu", "fcmovu", "fcom", "fcomi", "fcomip", "fcomp", "fcompp", "fcos", "fdecstp", "fdiv", "fdivp", "fdivr", "fdivrp", "femms", "ffree", "fiadd", "ficom", "ficomp", "fidiv", "fidivr", "fild", "fimul", "fincstp", "finit", "fist", "fistp", "fisttp", "fisub", "fisubr", "fld", "fld1", "fldcw", "fldenv", "fldl2e", "fldl2t", "fldlg2", "fldln2", "fldpi", "fldz", "fmul", "fmulp", "fnclex", "fninit", "fnop", "fnsave", "fnstcw", "fnstenv", "fnstsw", "fpatan", "fprem", "fprem1", "fptan", "frndint", "frstor", "fsave", "fscale", "fsin", "fsincos", "fsqrt", "fst", "fstcw", "fstenv", "fstp", "fstsw", "fsub", "fsubp", "fsubr", "fsubrp", "ftst", "fucom", "fucomi", "fucomip", "fucomp", "fucompp", "fwait", "fxam", "fxch", "fxrstor", "fxrstor64", "fxsave", "fxsave64", "fxtract", "fyl2x", "fyl2xp1", "haddpd", "haddps", "hsubpd", "hsubps", "idiv", "imul", "in", "inc", "insb", "insd", "insertps", "insertq", "insw", "int", "int3", "into", "ja", "jae", "jb", "jbe", "jc", "je", "jecxz", "jg", "jge", "jl", "jle", "jmp", "jna", "jnae", "jnb", "jnbe", "jnc", "jne", "jng", "jnge", "jnl", "jnle", "jno", "jnp", "jns", "jnz", "jo", "jp", "jpe", "jpo", "js", "jz", "kaddb", "kaddd", "kaddq", "kaddw", "kandb", "kandd", "kandnb", "kandnd", "kandnq", "kandnw", "kandq", "kandw", "kmovb", "kmovd", "kmovq", "kmovw", "knotb", "knotd", "knotq", "knotw", "korb", "kord", "korq", "kortestb", "kortestd", "kortestq", "kortestw", "korw", "kshiftlb", "kshiftld", "kshiftlq", "kshiftlw", "kshiftrb", "kshiftrd", "kshiftrq", "kshiftrw", "ktestb", "ktestd", "ktestq", "ktestw", "kunpckbw", "kunpckdq", "kunpckwd", "kxnorb", "kxnord", "kxnorq", "kxnorw", "kxorb", "kxord", "kxorq", "kxorw", "lahf", "lddqu", "ldmxcsr", "lea", "leave", "lfence", "lodsb", "lodsd", "lodsq", "lodsw", "loop", "loope", "loopne", "lzcnt", "maskmovdqu", "maskmovq", "maxpd", "maxps", "maxsd", "maxss", "mfence", "minpd", "minps", "minsd", "minss", "mov", "movapd", "movaps", "movbe", "movd", "movddup", "movdq2q", "movdqa", "movdqu", "movhlps", "movhpd", "movhps", "movlhps", "movlpd", "movlps", "movmskpd", "movmskps", "movntdq", "movntdqa", "movnti", "movntpd", "movntps", "movntq", "movntsd", "movntss", "movq", "movq2dq", "movsb", "movsd", "movshdup", "movsldup", "movsq", "movss", "movsw", "movsx", "movsxd", "movupd", "movups", "movzx", "mpsadbw", "mul", "mulpd", "mulps", "mulsd", "mulss", "mulx", "neg", "nop", "not", "or", "orpd", "orps", "out", "outsb", "outsd", "outsw", "pabsb", "pabsd", "pabsw", "packssdw", "packsswb", "packusdw", "packuswb", "paddb", "paddd", "paddq", "paddsb", "paddsw", "paddusb", "paddusw", "paddw", "palignr", "pand", "pandn", "pause", "pavgb", "pavgusb", "pavgw", "pblendvb", "pblendw", "pclmulqdq", "pcmpeqb", "pcmpeqd", "pcmpeqq", "pcmpeqw", "pcmpestri", "pcmpestrm", "pcmpgtb", "pcmpgtd", "pcmpgtq", "pcmpgtw", "pcmpistri", "pcmpistrm", "pcommit", "pdep", "pext", "pextrb", "pextrd", "pextrq", "pextrw", "pf2id", "pf2iw", "pfacc", "pfadd", "pfcmpeq", "pfcmpge", "pfcmpgt", "pfmax", "pfmin", "pfmul", "pfnacc", "pfpnacc", "pfrcp", "pfrcpit1", "pfrcpit2", "pfrcpv", "pfrsqit1", "pfrsqrt", "pfrsqrtv", "pfsub", "pfsubr", "phaddd", "phaddsw", "phaddw", "phminposuw", "phsubd", "phsubsw", "phsubw", "pi2fd", "pi2fw", "pinsrb", "pinsrd", "pinsrq", "pinsrw", "pmaddubsw", "pmaddwd", "pmaxsb", "pmaxsd", "pmaxsw", "pmaxub", "pmaxud", "pmaxuw", "pminsb", "pminsd", "pminsw", "pminub", "pminud", "pminuw", "pmovmskb", "pmovsxbd", "pmovsxbq", "pmovsxbw", "pmovsxdq", "pmovsxwd", "pmovsxwq", "pmovzxbd", "pmovzxbq", "pmovzxbw", "pmovzxdq", "pmovzxwd", "pmovzxwq", "pmuldq", "pmulhrsw", "pmulhrw", "pmulhuw", "pmulhw", "pmulld", "pmullw", "pmuludq", "pop", "popa", "popad", "popcnt", "popf", "popfd", "popfq", "por", "prefetch", "prefetchnta", "prefetcht0", "prefetcht1", "prefetcht2", "prefetchw", "prefetchwt1", "psadbw", "pshufb", "pshufd", "pshufhw", "pshuflw", "pshufw", "psignb", "psignd", "psignw", "pslld", "pslldq", "psllq", "psllw", "psrad", "psraw", "psrld", "psrldq", "psrlq", "psrlw", "psubb", "psubd", "psubq", "psubsb", "psubsw", "psubusb", "psubusw", "psubw", "pswapd", "ptest", "punpckhbw", "punpckhdq", "punpckhqdq", "punpckhwd", "punpcklbw", "punpckldq", "punpcklqdq", "punpcklwd", "push", "pusha", "pushad", "pushf", "pushfd", "pushfq", "pxor", "rcl", "rcpps", "rcpss", "rcr", "rdfsbase", "rdgsbase", "rdrand", "rdseed", "rdtsc", "rdtscp", "ret", "rol", "ror", "rorx", "roundpd", "roundps", "roundsd", "roundss", "rsqrtps", "rsqrtss", "sahf", "sal", "sar", "sarx", "sbb", "scasb", "scasd", "scasq", "scasw", "seta", "setae", "setb", "setbe", "setc", "sete", "setg", "setge", "setl", "setle", "setna", "setnae", "setnb", "setnbe", "setnc", "setne", "setng", "setnge", "setnl", "setnle", "setno", "setnp", "setns", "setnz", "seto", "setp", "setpe", "setpo", "sets", "setz", "sfence", "sha1msg1", "sha1msg2", "sha1nexte", "sha1rnds4", "sha256msg1", "sha256msg2", "sha256rnds2", "shl", "shld", "shlx", "shr", "shrd", "shrx", "shufpd", "shufps", "sqrtpd", "sqrtps", "sqrtsd", "sqrtss", "stac", "stc", "std", "sti", "stmxcsr", "stosb", "stosd", "stosq", "stosw", "sub", "subpd", "subps", "subsd", "subss", "swapgs", "syscall", "sysenter", "sysexit", "sysexit64", "sysret", "sysret64", "t1mskc", "test", "tzcnt", "tzmsk", "ucomisd", "ucomiss", "ud2", "unpckhpd", "unpckhps", "unpcklpd", "unpcklps", "vaddpd", "vaddps", "vaddsd", "vaddss", "vaddsubpd", "vaddsubps", "vaesdec", "vaesdeclast", "vaesenc", "vaesenclast", "vaesimc", "vaeskeygenassist", "valignd", "valignq", "vandnpd", "vandnps", "vandpd", "vandps", "vblendmb", "vblendmd", "vblendmpd", "vblendmps", "vblendmq", "vblendmw", "vblendpd", "vblendps", "vblendvpd", "vblendvps", "vbroadcastf128", "vbroadcastf32x2", "vbroadcastf32x4", "vbroadcastf32x8", "vbroadcastf64x2", "vbroadcastf64x4", "vbroadcasti128", "vbroadcasti32x2", "vbroadcasti32x4", "vbroadcasti32x8", "vbroadcasti64x2", "vbroadcasti64x4", "vbroadcastsd", "vbroadcastss", "vcmppd", "vcmpps", "vcmpsd", "vcmpss", "vcomisd", "vcomiss", "vcompresspd", "vcompressps", "vcvtdq2pd", "vcvtdq2ps", "vcvtpd2dq", "vcvtpd2ps", "vcvtpd2qq", "vcvtpd2udq", "vcvtpd2uqq", "vcvtph2ps", "vcvtps2dq", "vcvtps2pd", "vcvtps2ph", "vcvtps2qq", "vcvtps2udq", "vcvtps2uqq", "vcvtqq2pd", "vcvtqq2ps", "vcvtsd2si", "vcvtsd2ss", "vcvtsd2usi", "vcvtsi2sd", "vcvtsi2ss", "vcvtss2sd", "vcvtss2si", "vcvtss2usi", "vcvttpd2dq", "vcvttpd2qq", "vcvttpd2udq", "vcvttpd2uqq", "vcvttps2dq", "vcvttps2qq", "vcvttps2udq", "vcvttps2uqq", "vcvttsd2si", "vcvttsd2usi", "vcvttss2si", "vcvttss2usi", "vcvtudq2pd", "vcvtudq2ps", "vcvtuqq2pd", "vcvtuqq2ps", "vcvtusi2sd", "vcvtusi2ss", "vdbpsadbw", "vdivpd", "vdivps", "vdivsd", "vdivss", "vdppd", "vdpps", "vexp2pd", "vexp2ps", "vexpandpd", "vexpandps", "vextractf128", "vextractf32x4", "vextractf32x8", "vextractf64x2", "vextractf64x4", "vextracti128", "vextracti32x4", "vextracti32x8", "vextracti64x2", "vextracti64x4", "vextractps", "vfixupimmpd", "vfixupimmps", "vfixupimmsd", "vfixupimmss", "vfmadd132pd", "vfmadd132ps", "vfmadd132sd", "vfmadd132ss", "vfmadd213pd", "vfmadd213ps", "vfmadd213sd", "vfmadd213ss", "vfmadd231pd", "vfmadd231ps", "vfmadd231sd", "vfmadd231ss", "vfmaddpd", "vfmaddps", "vfmaddsd", "vfmaddss", "vfmaddsub132pd", "vfmaddsub132ps", "vfmaddsub213pd", "vfmaddsub213ps", "vfmaddsub231pd", "vfmaddsub231ps", "vfmaddsubpd", "vfmaddsubps", "vfmsub132pd", "vfmsub132ps", "vfmsub132sd", "vfmsub132ss", "vfmsub213pd", "vfmsub213ps", "vfmsub213sd", "vfmsub213ss", "vfmsub231pd", "vfmsub231ps", "vfmsub231sd", "vfmsub231ss", "vfmsubadd132pd", "vfmsubadd132ps", "vfmsubadd213pd", "vfmsubadd213ps", "vfmsubadd231pd", "vfmsubadd231ps", "vfmsubaddpd", "vfmsubaddps", "vfmsubpd", "vfmsubps", "vfmsubsd", "vfmsubss", "vfnmadd132pd", "vfnmadd132ps", "vfnmadd132sd", "vfnmadd132ss", "vfnmadd213pd", "vfnmadd213ps", "vfnmadd213sd", "vfnmadd213ss", "vfnmadd231pd", "vfnmadd231ps", "vfnmadd231sd", "vfnmadd231ss", "vfnmaddpd", "vfnmaddps", "vfnmaddsd", "vfnmaddss", "vfnmsub132pd", "vfnmsub132ps", "vfnmsub132sd", "vfnmsub132ss", "vfnmsub213pd", "vfnmsub213ps", "vfnmsub213sd", "vfnmsub213ss", "vfnmsub231pd", "vfnmsub231ps", "vfnmsub231sd", "vfnmsub231ss", "vfnmsubpd", "vfnmsubps", "vfnmsubsd", "vfnmsubss", "vfpclasspd", "vfpclassps", "vfpclasssd", "vfpclassss", "vfrczpd", "vfrczps", "vfrczsd", "vfrczss", "vgatherdpd", "vgatherdps", "vgatherpf0dpd", "vgatherpf0dps", "vgatherpf0qpd", "vgatherpf0qps", "vgatherpf1dpd", "vgatherpf1dps", "vgatherpf1qpd", "vgatherpf1qps", "vgatherqpd", "vgatherqps", "vgetexppd", "vgetexpps", "vgetexpsd", "vgetexpss", "vgetmantpd", "vgetmantps", "vgetmantsd", "vgetmantss", "vhaddpd", "vhaddps", "vhsubpd", "vhsubps", "vinsertf128", "vinsertf32x4", "vinsertf32x8", "vinsertf64x2", "vinsertf64x4", "vinserti128", "vinserti32x4", "vinserti32x8", "vinserti64x2", "vinserti64x4", "vinsertps", "vlddqu", "vldmxcsr", "vmaskmovdqu", "vmaskmovpd", "vmaskmovps", "vmaxpd", "vmaxps", "vmaxsd", "vmaxss", "vminpd", "vminps", "vminsd", "vminss", "vmovapd", "vmovaps", "vmovd", "vmovddup", "vmovdqa", "vmovdqa32", "vmovdqa64", "vmovdqu", "vmovdqu16", "vmovdqu32", "vmovdqu64", "vmovdqu8", "vmovhlps", "vmovhpd", "vmovhps", "vmovlhps", "vmovlpd", "vmovlps", "vmovmskpd", "vmovmskps", "vmovntdq", "vmovntdqa", "vmovntpd", "vmovntps", "vmovq", "vmovsd", "vmovshdup", "vmovsldup", "vmovss", "vmovupd", "vmovups", "vmpsadbw", "vmulpd", "vmulps", "vmulsd", "vmulss", "vorpd", "vorps", "vpabsb", "vpabsd", "vpabsq", "vpabsw", "vpackssdw", "vpacksswb", "vpackusdw", "vpackuswb", "vpaddb", "vpaddd", "vpaddq", "vpaddsb", "vpaddsw", "vpaddusb", "vpaddusw", "vpaddw", "vpalignr", "vpand", "vpandd", "vpandn", "vpandnd", "vpandnq", "vpandq", "vpavgb", "vpavgw", "vpblendd", "vpblendvb", "vpblendw", "vpbroadcastb", "vpbroadcastd", "vpbroadcastmb2d", "vpbroadcastmb2q", "vpbroadcastq", "vpbroadcastw", "vpclmulqdq", "vpcmov", "vpcmpb", "vpcmpd", "vpcmpeqb", "vpcmpeqd", "vpcmpeqq", "vpcmpeqw", "vpcmpestri", "vpcmpestrm", "vpcmpgtb", "vpcmpgtd", "vpcmpgtq", "vpcmpgtw", "vpcmpistri", "vpcmpistrm", "vpcmpq", "vpcmpub", "vpcmpud", "vpcmpuq", "vpcmpuw", "vpcmpw", "vpcomb", "vpcomd", "vpcompressd", "vpcompressq", "vpcomq", "vpcomub", "vpcomud", "vpcomuq", "vpcomuw", "vpcomw", "vpconflictd", "vpconflictq", "vperm2f128", "vperm2i128", "vpermb", "vpermd", "vpermi2b", "vpermi2d", "vpermi2pd", "vpermi2ps", "vpermi2q", "vpermi2w", "vpermil2pd", "vpermil2ps", "vpermilpd", "vpermilps", "vpermpd", "vpermps", "vpermq", "vpermt2b", "vpermt2d", "vpermt2pd", "vpermt2ps", "vpermt2q", "vpermt2w", "vpermw", "vpexpandd", "vpexpandq", "vpextrb", "vpextrd", "vpextrq", "vpextrw", "vpgatherdd", "vpgatherdq", "vpgatherqd", "vpgatherqq", "vphaddbd", "vphaddbq", "vphaddbw", "vphaddd", "vphadddq", "vphaddsw", "vphaddubd", "vphaddubq", "vphaddubw", "vphaddudq", "vphadduwd", "vphadduwq", "vphaddw", "vphaddwd", "vphaddwq", "vphminposuw", "vphsubbw", "vphsubd", "vphsubdq", "vphsubsw", "vphsubw", "vphsubwd", "vpinsrb", "vpinsrd", "vpinsrq", "vpinsrw", "vplzcntd", "vplzcntq", "vpmacsdd", "vpmacsdqh", "vpmacsdql", "vpmacssdd", "vpmacssdqh", "vpmacssdql", "vpmacsswd", "vpmacssww", "vpmacswd", "vpmacsww", "vpmadcsswd", "vpmadcswd", "vpmadd52huq", "vpmadd52luq", "vpmaddubsw", "vpmaddwd", "vpmaskmovd", "vpmaskmovq", "vpmaxsb", "vpmaxsd", "vpmaxsq", "vpmaxsw", "vpmaxub", "vpmaxud", "vpmaxuq", "vpmaxuw", "vpminsb", "vpminsd", "vpminsq", "vpminsw", "vpminub", "vpminud", "vpminuq", "vpminuw", "vpmovb2m", "vpmovd2m", "vpmovdb", "vpmovdw", "vpmovm2b", "vpmovm2d", "vpmovm2q", "vpmovm2w", "vpmovmskb", "vpmovq2m", "vpmovqb", "vpmovqd", "vpmovqw", "vpmovsdb", "vpmovsdw", "vpmovsqb", "vpmovsqd", "vpmovsqw", "vpmovswb", "vpmovsxbd", "vpmovsxbq", "vpmovsxbw", "vpmovsxdq", "vpmovsxwd", "vpmovsxwq", "vpmovusdb", "vpmovusdw", "vpmovusqb", "vpmovusqd", "vpmovusqw", "vpmovuswb", "vpmovw2m", "vpmovwb", "vpmovzxbd", "vpmovzxbq", "vpmovzxbw", "vpmovzxdq", "vpmovzxwd", "vpmovzxwq", "vpmuldq", "vpmulhrsw", "vpmulhuw", "vpmulhw", "vpmulld", "vpmullq", "vpmullw", "vpmultishiftqb", "vpmuludq", "vpor", "vpord", "vporq", "vpperm", "vprold", "vprolq", "vprolvd", "vprolvq", "vprord", "vprorq", "vprorvd", "vprorvq", "vprotb", "vprotd", "vprotq", "vprotw", "vpsadbw", "vpscatterdd", "vpscatterdq", "vpscatterqd", "vpscatterqq", "vpshab", "vpshad", "vpshaq", "vpshaw", "vpshlb", "vpshld", "vpshlq", "vpshlw", "vpshufb", "vpshufd", "vpshufhw", "vpshuflw", "vpsignb", "vpsignd", "vpsignw", "vpslld", "vpslldq", "vpsllq", "vpsllvd", "vpsllvq", "vpsllvw", "vpsllw", "vpsrad", "vpsraq", "vpsravd", "vpsravq", "vpsravw", "vpsraw", "vpsrld", "vpsrldq", "vpsrlq", "vpsrlvd", "vpsrlvq", "vpsrlvw", "vpsrlw", "vpsubb", "vpsubd", "vpsubq", "vpsubsb", "vpsubsw", "vpsubusb", "vpsubusw", "vpsubw", "vpternlogd", "vpternlogq", "vptest", "vptestmb", "vptestmd", "vptestmq", "vptestmw", "vptestnmb", "vptestnmd", "vptestnmq", "vptestnmw", "vpunpckhbw", "vpunpckhdq", "vpunpckhqdq", "vpunpckhwd", "vpunpcklbw", "vpunpckldq", "vpunpcklqdq", "vpunpcklwd", "vpxor", "vpxord", "vpxorq", "vrangepd", "vrangeps", "vrangesd", "vrangess", "vrcp14pd", "vrcp14ps", "vrcp14sd", "vrcp14ss", "vrcp28pd", "vrcp28ps", "vrcp28sd", "vrcp28ss", "vrcpps", "vrcpss", "vreducepd", "vreduceps", "vreducesd", "vreducess", "vrndscalepd", "vrndscaleps", "vrndscalesd", "vrndscaless", "vroundpd", "vroundps", "vroundsd", "vroundss", "vrsqrt14pd", "vrsqrt14ps", "vrsqrt14sd", "vrsqrt14ss", "vrsqrt28pd", "vrsqrt28ps", "vrsqrt28sd", "vrsqrt28ss", "vrsqrtps", "vrsqrtss", "vscalefpd", "vscalefps", "vscalefsd", "vscalefss", "vscatterdpd", "vscatterdps", "vscatterpf0dpd", "vscatterpf0dps", "vscatterpf0qpd", "vscatterpf0qps", "vscatterpf1dpd", "vscatterpf1dps", "vscatterpf1qpd", "vscatterpf1qps", "vscatterqpd", "vscatterqps", "vshuff32x4", "vshuff64x2", "vshufi32x4", "vshufi64x2", "vshufpd", "vshufps", "vsqrtpd", "vsqrtps", "vsqrtsd", "vsqrtss", "vstmxcsr", "vsubpd", "vsubps", "vsubsd", "vsubss", "vtestpd", "vtestps", "vucomisd", "vucomiss", "vunpckhpd", "vunpckhps", "vunpcklpd", "vunpcklps", "vxorpd", "vxorps", "vzeroall", "vzeroupper", "wait", "wrfsbase", "wrgsbase", "xadd", "xchg", "xgetbv", "xor", "xorpd", "xorps", "xrstor", "xrstor64", "xrstors", "xrstors64", "xsave", "xsave64", "xsavec", "xsavec64", "xsaveopt", "xsaveopt64", "xsaves", "xsaves64", "xsetbv", "hlt", "movabs", "endbr32", "endbr64"),
    mnemonic: _ => choice("mov", "ret"),
    asm_instruction: $ => seq($.mnemonic, optional($.expr_list)),
    instruction: $ => seq(optional($.instr_prefix), $.asm_instruction, $.eol), // masm bnf grammar error (possible): I belive there should be an eol (;;) here


    // uses expresions

    expr_list: $ => list($.expression),
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
      prec(1, seq($.data_decl, $.scalar_inst_list)),
      prec(2, seq(IDENTIFIER, $.struct_inst_list)),
      prec(3, seq(RECORD_TAG, $.record_inst_list)),
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

    struct_instance: $ => choice(
      seq("<", optional($.field_init_list), ">"),
      seq("{", optional($.eol), optional($.field_init_list), optional($.eol), "}"),
      seq($.expression, "dup", "(", $.struct_inst_list, ")"),
    ),
    struct_inst_list: $ => listWithEol($.struct_instance, $.eol),
    struct_item: $ => choice(
      $.data_dir,
      // $.general_dir,
      $.offset_dir,
      $.nested_struct,
    ),
    nested_struct: $ => seq($.struct_hdr, optional(IDENTIFIER), $.eol, $.struct_body, "ends", $.eol),
    struct_body: $ => repeat1($.struct_item),
    _field_align: $ => alias($.expression, $.field_align),
    struct_dir: $ => seq(
      STRUCT_TAG, $.struct_hdr, optional($._field_align), optional(seq(",", "nonunique")), $.eol,
      $.struct_body,
      STRUCT_TAG, "ends", $.eol,
    ),

    macro_arg: $ => choice(
      seq("%", $.expression),
      seq("%", TEXT_MACRO_ID),
      seq("%", MACRO_FUNC_ID, "(", $.macro_arg_list, ")"),
      $.string,
      $.arbitrary_text,
      seq("<", $.arbitrary_text, ">"),
    ),
    macro_arg_list: $ => list($.macro_arg),
    macro_stmt: $ => choice(
      $.directive,
      $.exitm_dir,
      seq(":", MACRO_LABEL),
      seq("goto", MACRO_LABEL),
    ),
    macro_stmt_list: $ => repeat1(seq($.macro_stmt, $.eol)),
    macro_body: $ => seq(optional($.local_list), $.macro_stmt_list),
    macro_dir: $ => seq(
      IDENTIFIER, "macro", optional($.macro_parm_list), $.eol,
      $.macro_body,
      "endm", $.eol,
    ),
    macro_for: $ => seq(
      $.for_dir, $.for_parm, ",", "<", $.macro_parm_list, ">", $.eol,
      $.macro_body,
      "endm", $.eol,
    ),
    macro_forc: $ => seq(
      $.forc_dir, $.for_parm, ",", "<", $.macro_parm_list, ">", $.eol,
      $.macro_body,
      "endm", $.eol,
    ),
    macro_repeat: $ => seq(
      $.repeat_dir, $.expression, $.eol,
      $.macro_body,
      "endm", $.eol,
    ),
    macro_while: $ => seq(
      "while", $.expression, $.eol,
      $.macro_body,
      "endm", $.eol,
    ),
    macro_call: $ => choice(
      seq(IDENTIFIER, $.macro_arg_list, $.eol),
      seq(IDENTIFIER, "(", $.macro_arg_list, ")"),
    ),

    eq_dir: $ => seq(IDENTIFIER, "=", $.expression, $.eol),
    equ_type: $ => choice($.expression, $.text_literal),
    equ_dir: $ => seq(TEXT_MACRO_ID, "equ", $.equ_type, $.eol),

    radix_dir: $ => seq(".radix", $.expression, $.eol),

    title_dir: $ => seq($.title_type, $.arbitrary_text, $.eol),

    page_expr: $ => choice(
      "+",
      seq($.expression, optional(seq(",", $.expression))),
    ),
    page_dir: $ => seq("page", optional($.page_expr), $.eol),

    // masm bnf grammar error: this isn't even listed
    alias_dir: $ => seq("alias", $.text_literal, "=", $.text_literal),

    general_dir: $ => choice(
      $.alias_dir,
      $.assume_dir,
      $.comm_dir,
      $.context_dir,
      $.cref_dir,
      $.echo_dir,
      $.eq_dir,
      $.equ_dir,
      $.error_dir,
      $.extern_dir,
      $.group_dir,
      $.if_dir,
      $.include_dir,
      $.include_lib_dir,
      $.list_dir,
      $.macro_call,
      $.macro_dir,
      $.macro_for,
      $.macro_forc,
      $.macro_repeat,
      $.macro_while,
      $.model_dir,
      $.name_dir,
      $.option_dir,
      $.page_dir,
      $.processor_dir,
      $.proto_type_dir,
      $.public_dir,
      $.purge_dir,
      $.radix_dir,
      $.record_dir,
      $.seg_order_dir,
      $.struct_dir,
      $.text_dir,
      $.title_dir,
      $.typedef_dir,
    ),
    directive: $ => choice($.general_dir, $.segment_def),
    directive_list: $ => repeat1($.directive),

    if_statement: $ => choice(
      seq("if", $.expression),
      seq("ife", $.expression),
      seq("ifb", $.text_item),
      seq("ifnb", $.text_item),
      seq("ifdef", IDENTIFIER),
      seq("ifndef", IDENTIFIER),
      seq("ifdif", $.text_item , $.text_item),
      seq("ifdifi", $.text_item , $.text_item),
      seq("ifidn", $.text_item , $.text_item),
      seq("ifidni", $.text_item , $.text_item),
      "if1",
      "if2",
    ),
    if_dir: $ => seq($.if_statement, $.eol,
      $.directive_list,
      repeat($.elseif_block),
      optional(seq(
        "else", $.eol,
        $.directive_list,
      )),
      $.eol,
    ),
    elseif_statement: $ => choice(
      seq("elseif", $.expression),
      seq("elseife", $.expression),
      seq("elseifb", $.text_item),
      seq("elseifnb", $.text_item),
      seq("elseifdef", IDENTIFIER),
      seq("elseifndef", IDENTIFIER),
      seq("elseifdif", $.text_item, $.text_item),
      seq("elseifdifi", $.text_item, $.text_item),
      seq("elseifidn", $.text_item, $.text_item),
      seq("elseifidni", $.text_item, $.text_item),
      "elseif1",
      "elseif2",
    ),
    elseif_block: $ => seq($.elseif_statement, $.eol, $.directive_list),
    opt_text: $ => seq(",", $.text_item),
    error_opt: $ => choice(
      seq(".ERR", optional($.text_item)),
      seq(".ERRE", $.expression, optional($.opt_text)),
      seq(".ERRNZ", $.expression, optional($.opt_text)),
      seq(".ERRB", $.text_item, optional($.opt_text)),
      seq(".ERRNB", $.text_item, optional($.opt_text)),
      seq(".ERRDEF", IDENTIFIER, optional($.opt_text)),
      seq(".ERRNDEF", IDENTIFIER, optional($.opt_text)),
      seq(".ERRDIF", $.text_item, $.text_item, optional($.opt_text)),
      seq(".ERRDIFI", $.text_item, $.text_item, optional($.opt_text)),
      seq(".ERRIDN", $.text_item, $.text_item, optional($.opt_text)),
      seq(".ERRIDNI", $.text_item, $.text_item, optional($.opt_text)),
      seq(".ERR1", optional($.text_item)),
      seq(".ERR2", optional($.text_item)),
    ),
    error_dir: $ => seq($.error_opt, $.eol),


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
    proto_type_dir: $ => seq(IDENTIFIER, "proto", optional($.proto_spec), $.eol), // masm bnf grammar error (possibly): I believe there should be an eol (;;) here

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
    parm: $ => choice(
      seq(PARM_ID, optional(seq(":", $.qualified_type))),
      seq(PARM_ID, optional($.expression), optional(seq(":", $.qualified_type))),
    ),
    parm_list: $ => listWithEol($.parm, $.eol),

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
    typedef_dir: $ => seq(TYPE_ID, "typedef", $.qualifier, $.eol), // masm bnf grammar error (possibly): I believe there should be an eol (;;) here

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
    local_dir: $ => seq("local", $.parm_list, $.eol),
    local_dir_list: $ => repeat1($.local_dir),

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
      seq("at", $.expression),
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
    segment_def: $ => choice(
      seq($.segment_dir, optional($.in_seg_dir_list), $.ends_dir),
      seq($.simple_seg_dir, optional($.in_seg_dir_list), optional($.ends_dir)),
    ),

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

    _size_arg: $ => alias($.expression, $.size_arg),

    exit_dir: $ => seq(".exit", $.expression, $.eol),

    invoke_arg: $ => choice(
      seq($.register, "::", $.register),
      $.expression,
      seq("addr", $.expression),
    ),
    invoke_list: $ => listWithEol($.invoke_arg, $.eol),
    invoke_dir: $ => seq("invoke", $.expression, optional(seq(",", optional($.eol), $.invoke_list)), $.eol),

    in_segment_dir: $ => choice(
      $.instruction,
      $.data_dir,
      $.startup_dir,
      $.exit_dir,
      $.offset_dir,
      $.label_dir,
      $.invoke_dir,
      $.general_dir,
      $.control_dir,
      seq($.proc_dir, optional($.local_dir_list), optional($.in_seg_dir_list), $.endp_dir),
    ),
    in_seg_dir: $ => seq(optional($.label_def), $.in_segment_dir),
    in_seg_dir_list: $ => repeat1($.in_seg_dir),

    block_statements: $ => choice(
      $.directive_list,
      seq(".continue", optional(seq(".if", $.expression))),
      seq(".break", optional(seq(".if", $.expression))),
    ),
    control_if: $ => seq(
      ".if", $.expression, $.eol,
      $.directive_list,
      repeat($.control_elseif),
      optional(seq(
        ".else", $.eol,
        $.directive_list,
      )),
      ".endif", $.eol,
    ),
    control_elseif: $ => seq(
      ".elseif", $.expression, $.eol,
      $.directive_list,
    ),
    while_block: $ => seq(
      ".while", $.expression, $.eol,
      $.block_statements, $.eol,
      ".endw",
    ),
    repeat_block: $ => seq(
      ".repeat", $.eol,
      $.block_statements, $.eol, $.until_dir, $.eol,
    ),
    control_block: $ => choice($.while_block, $.repeat_block),
    control_dir: $ => choice($.control_if, $.control_block),

    proc_parm_list: $ => seq(
      optional(seq(",", optional($.eol))),
      choice(
        seq($.parm_list, optional(seq(",", optional($.eol), PARM_ID, ":vararg"))),
        seq(PARM_ID, ":vararg"),
      ),
    ),
    proc_dir: $ => seq(
      PROC_ID, "proc", optional($.p_options), optional(seq("<", $.macro_arg_list, ">")),
      optional($.uses_regs), optional($.proc_parm_list), $.eol,
    ),


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

    processor: _ => choice(".386", ".386p", ".486", ".486P", ".586", ".586P", ".686", ".686P"),
    coprocessor: _ => choice(".8087", ".287", ".387", ".NO87"),
    processor_dir: $ => choice(
      prec(2, seq($.processor, $.eol)),
      prec(1, seq($.coprocessor, $.eol)),
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


    // TODO: MASM's COMMENT directive is gross and probably requires an external scanner to properly parse it
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq(TEXT, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, TEXT, $.eol),
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq(TEXT, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, TEXT, $.eol),
    // comment_dir: _ => /comment\s+(\S)\n([^\n]*\n)*$1/,
  }
});
