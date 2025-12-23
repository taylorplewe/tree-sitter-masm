/**
 * @file Microsoft Macro Assembler x86 syntax
 * @author Taylor Plewe <tplewe@outlook.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const MNEMONIC = /aaa|aad|aam|aas|adc|adcx|add|addpd|addps|addsd|addss|addsubpd|addsubps|adox|aesdec|aesdeclast|aesenc|aesenclast|aesimc|aeskeygenassist|and|andn|andnpd|andnps|andpd|andps|bextr|blcfill|blci|blcic|blcmsk|blcs|blendpd|blendps|blendvpd|blendvps|blsfill|blsi|blsic|blsmsk|blsr|bndcl|bndcn|bndcu|bndldx|bndmk|bndmov|bndstx|bsf|bsr|bswap|bt|btc|btr|bts|bzhi|call|cbw|cdq|cdqe|clac|clc|cld|clflush|clflushopt|clwb|clzero|cmc|cmova|cmovae|cmovb|cmovbe|cmovc|cmove|cmovg|cmovge|cmovl|cmovle|cmovna|cmovnae|cmovnb|cmovnbe|cmovnc|cmovne|cmovng|cmovnge|cmovnl|cmovnle|cmovno|cmovnp|cmovns|cmovnz|cmovo|cmovp|cmovpe|cmovpo|cmovs|cmovz|cmp|cmppd|cmpps|cmpsb|cmpsd|cmpsq|cmpss|cmpsw|cmpxchg|cmpxchg16b|cmpxchg8b|comisd|comiss|cpuid|cqo|crc32|cvtdq2pd|cvtdq2ps|cvtpd2dq|cvtpd2pi|cvtpd2ps|cvtpi2pd|cvtpi2ps|cvtps2dq|cvtps2pd|cvtps2pi|cvtsd2si|cvtsd2ss|cvtsi2sd|cvtsi2ss|cvtss2sd|cvtss2si|cvttpd2dq|cvttpd2pi|cvttps2dq|cvttps2pi|cvttsd2si|cvttss2si|cwd|cwde|daa|das|dec|div|divpd|divps|divsd|divss|dppd|dpps|emms|enter|extractps|extrq|f2xm1|fabs|fadd|faddp|fbld|fbstp|fchs|fclex|fcmovb|fcmovbe|fcmove|fcmovnb|fcmovnbe|fcmovne|fcmovnu|fcmovu|fcom|fcomi|fcomip|fcomp|fcompp|fcos|fdecstp|fdiv|fdivp|fdivr|fdivrp|femms|ffree|fiadd|ficom|ficomp|fidiv|fidivr|fild|fimul|fincstp|finit|fist|fistp|fisttp|fisub|fisubr|fld|fld1|fldcw|fldenv|fldl2e|fldl2t|fldlg2|fldln2|fldpi|fldz|fmul|fmulp|fnclex|fninit|fnop|fnsave|fnstcw|fnstenv|fnstsw|fpatan|fprem|fprem1|fptan|frndint|frstor|fsave|fscale|fsin|fsincos|fsqrt|fst|fstcw|fstenv|fstp|fstsw|fsub|fsubp|fsubr|fsubrp|ftst|fucom|fucomi|fucomip|fucomp|fucompp|fwait|fxam|fxch|fxrstor|fxrstor64|fxsave|fxsave64|fxtract|fyl2x|fyl2xp1|haddpd|haddps|hsubpd|hsubps|idiv|imul|in|inc|insb|insd|insertps|insertq|insw|int|int3|into|ja|jae|jb|jbe|jc|je|jecxz|jg|jge|jl|jle|jmp|jna|jnae|jnb|jnbe|jnc|jne|jng|jnge|jnl|jnle|jno|jnp|jns|jnz|jo|jp|jpe|jpo|js|jz|kaddb|kaddd|kaddq|kaddw|kandb|kandd|kandnb|kandnd|kandnq|kandnw|kandq|kandw|kmovb|kmovd|kmovq|kmovw|knotb|knotd|knotq|knotw|korb|kord|korq|kortestb|kortestd|kortestq|kortestw|korw|kshiftlb|kshiftld|kshiftlq|kshiftlw|kshiftrb|kshiftrd|kshiftrq|kshiftrw|ktestb|ktestd|ktestq|ktestw|kunpckbw|kunpckdq|kunpckwd|kxnorb|kxnord|kxnorq|kxnorw|kxorb|kxord|kxorq|kxorw|lahf|lddqu|ldmxcsr|lea|leave|lfence|lodsb|lodsd|lodsq|lodsw|loop|loope|loopne|lzcnt|maskmovdqu|maskmovq|maxpd|maxps|maxsd|maxss|mfence|minpd|minps|minsd|minss|mov|movapd|movaps|movbe|movd|movddup|movdq2q|movdqa|movdqu|movhlps|movhpd|movhps|movlhps|movlpd|movlps|movmskpd|movmskps|movntdq|movntdqa|movnti|movntpd|movntps|movntq|movntsd|movntss|movq|movq2dq|movsb|movsd|movshdup|movsldup|movsq|movss|movsw|movsx|movsxd|movupd|movups|movzx|mpsadbw|mul|mulpd|mulps|mulsd|mulss|mulx|neg|nop|not|or|orpd|orps|out|outsb|outsd|outsw|pabsb|pabsd|pabsw|packssdw|packsswb|packusdw|packuswb|paddb|paddd|paddq|paddsb|paddsw|paddusb|paddusw|paddw|palignr|pand|pandn|pause|pavgb|pavgusb|pavgw|pblendvb|pblendw|pclmulqdq|pcmpeqb|pcmpeqd|pcmpeqq|pcmpeqw|pcmpestri|pcmpestrm|pcmpgtb|pcmpgtd|pcmpgtq|pcmpgtw|pcmpistri|pcmpistrm|pcommit|pdep|pext|pextrb|pextrd|pextrq|pextrw|pf2id|pf2iw|pfacc|pfadd|pfcmpeq|pfcmpge|pfcmpgt|pfmax|pfmin|pfmul|pfnacc|pfpnacc|pfrcp|pfrcpit1|pfrcpit2|pfrcpv|pfrsqit1|pfrsqrt|pfrsqrtv|pfsub|pfsubr|phaddd|phaddsw|phaddw|phminposuw|phsubd|phsubsw|phsubw|pi2fd|pi2fw|pinsrb|pinsrd|pinsrq|pinsrw|pmaddubsw|pmaddwd|pmaxsb|pmaxsd|pmaxsw|pmaxub|pmaxud|pmaxuw|pminsb|pminsd|pminsw|pminub|pminud|pminuw|pmovmskb|pmovsxbd|pmovsxbq|pmovsxbw|pmovsxdq|pmovsxwd|pmovsxwq|pmovzxbd|pmovzxbq|pmovzxbw|pmovzxdq|pmovzxwd|pmovzxwq|pmuldq|pmulhrsw|pmulhrw|pmulhuw|pmulhw|pmulld|pmullw|pmuludq|pop|popa|popad|popcnt|popf|popfd|popfq|por|prefetch|prefetchnta|prefetcht0|prefetcht1|prefetcht2|prefetchw|prefetchwt1|psadbw|pshufb|pshufd|pshufhw|pshuflw|pshufw|psignb|psignd|psignw|pslld|pslldq|psllq|psllw|psrad|psraw|psrld|psrldq|psrlq|psrlw|psubb|psubd|psubq|psubsb|psubsw|psubusb|psubusw|psubw|pswapd|ptest|punpckhbw|punpckhdq|punpckhqdq|punpckhwd|punpcklbw|punpckldq|punpcklqdq|punpcklwd|push|pusha|pushad|pushf|pushfd|pushfq|pxor|rcl|rcpps|rcpss|rcr|rdfsbase|rdgsbase|rdrand|rdseed|rdtsc|rdtscp|ret|rol|ror|rorx|roundpd|roundps|roundsd|roundss|rsqrtps|rsqrtss|sahf|sal|sar|sarx|sbb|scasb|scasd|scasq|scasw|seta|setae|setb|setbe|setc|sete|setg|setge|setl|setle|setna|setnae|setnb|setnbe|setnc|setne|setng|setnge|setnl|setnle|setno|setnp|setns|setnz|seto|setp|setpe|setpo|sets|setz|sfence|sha1msg1|sha1msg2|sha1nexte|sha1rnds4|sha256msg1|sha256msg2|sha256rnds2|shl|shld|shlx|shr|shrd|shrx|shufpd|shufps|sqrtpd|sqrtps|sqrtsd|sqrtss|stac|stc|std|sti|stmxcsr|stosb|stosd|stosq|stosw|sub|subpd|subps|subsd|subss|swapgs|syscall|sysenter|sysexit|sysexit64|sysret|sysret64|t1mskc|test|tzcnt|tzmsk|ucomisd|ucomiss|ud2|unpckhpd|unpckhps|unpcklpd|unpcklps|vaddpd|vaddps|vaddsd|vaddss|vaddsubpd|vaddsubps|vaesdec|vaesdeclast|vaesenc|vaesenclast|vaesimc|vaeskeygenassist|valignd|valignq|vandnpd|vandnps|vandpd|vandps|vblendmb|vblendmd|vblendmpd|vblendmps|vblendmq|vblendmw|vblendpd|vblendps|vblendvpd|vblendvps|vbroadcastf128|vbroadcastf32x2|vbroadcastf32x4|vbroadcastf32x8|vbroadcastf64x2|vbroadcastf64x4|vbroadcasti128|vbroadcasti32x2|vbroadcasti32x4|vbroadcasti32x8|vbroadcasti64x2|vbroadcasti64x4|vbroadcastsd|vbroadcastss|vcmppd|vcmpps|vcmpsd|vcmpss|vcomisd|vcomiss|vcompresspd|vcompressps|vcvtdq2pd|vcvtdq2ps|vcvtpd2dq|vcvtpd2ps|vcvtpd2qq|vcvtpd2udq|vcvtpd2uqq|vcvtph2ps|vcvtps2dq|vcvtps2pd|vcvtps2ph|vcvtps2qq|vcvtps2udq|vcvtps2uqq|vcvtqq2pd|vcvtqq2ps|vcvtsd2si|vcvtsd2ss|vcvtsd2usi|vcvtsi2sd|vcvtsi2ss|vcvtss2sd|vcvtss2si|vcvtss2usi|vcvttpd2dq|vcvttpd2qq|vcvttpd2udq|vcvttpd2uqq|vcvttps2dq|vcvttps2qq|vcvttps2udq|vcvttps2uqq|vcvttsd2si|vcvttsd2usi|vcvttss2si|vcvttss2usi|vcvtudq2pd|vcvtudq2ps|vcvtuqq2pd|vcvtuqq2ps|vcvtusi2sd|vcvtusi2ss|vdbpsadbw|vdivpd|vdivps|vdivsd|vdivss|vdppd|vdpps|vexp2pd|vexp2ps|vexpandpd|vexpandps|vextractf128|vextractf32x4|vextractf32x8|vextractf64x2|vextractf64x4|vextracti128|vextracti32x4|vextracti32x8|vextracti64x2|vextracti64x4|vextractps|vfixupimmpd|vfixupimmps|vfixupimmsd|vfixupimmss|vfmadd132pd|vfmadd132ps|vfmadd132sd|vfmadd132ss|vfmadd213pd|vfmadd213ps|vfmadd213sd|vfmadd213ss|vfmadd231pd|vfmadd231ps|vfmadd231sd|vfmadd231ss|vfmaddpd|vfmaddps|vfmaddsd|vfmaddss|vfmaddsub132pd|vfmaddsub132ps|vfmaddsub213pd|vfmaddsub213ps|vfmaddsub231pd|vfmaddsub231ps|vfmaddsubpd|vfmaddsubps|vfmsub132pd|vfmsub132ps|vfmsub132sd|vfmsub132ss|vfmsub213pd|vfmsub213ps|vfmsub213sd|vfmsub213ss|vfmsub231pd|vfmsub231ps|vfmsub231sd|vfmsub231ss|vfmsubadd132pd|vfmsubadd132ps|vfmsubadd213pd|vfmsubadd213ps|vfmsubadd231pd|vfmsubadd231ps|vfmsubaddpd|vfmsubaddps|vfmsubpd|vfmsubps|vfmsubsd|vfmsubss|vfnmadd132pd|vfnmadd132ps|vfnmadd132sd|vfnmadd132ss|vfnmadd213pd|vfnmadd213ps|vfnmadd213sd|vfnmadd213ss|vfnmadd231pd|vfnmadd231ps|vfnmadd231sd|vfnmadd231ss|vfnmaddpd|vfnmaddps|vfnmaddsd|vfnmaddss|vfnmsub132pd|vfnmsub132ps|vfnmsub132sd|vfnmsub132ss|vfnmsub213pd|vfnmsub213ps|vfnmsub213sd|vfnmsub213ss|vfnmsub231pd|vfnmsub231ps|vfnmsub231sd|vfnmsub231ss|vfnmsubpd|vfnmsubps|vfnmsubsd|vfnmsubss|vfpclasspd|vfpclassps|vfpclasssd|vfpclassss|vfrczpd|vfrczps|vfrczsd|vfrczss|vgatherdpd|vgatherdps|vgatherpf0dpd|vgatherpf0dps|vgatherpf0qpd|vgatherpf0qps|vgatherpf1dpd|vgatherpf1dps|vgatherpf1qpd|vgatherpf1qps|vgatherqpd|vgatherqps|vgetexppd|vgetexpps|vgetexpsd|vgetexpss|vgetmantpd|vgetmantps|vgetmantsd|vgetmantss|vhaddpd|vhaddps|vhsubpd|vhsubps|vinsertf128|vinsertf32x4|vinsertf32x8|vinsertf64x2|vinsertf64x4|vinserti128|vinserti32x4|vinserti32x8|vinserti64x2|vinserti64x4|vinsertps|vlddqu|vldmxcsr|vmaskmovdqu|vmaskmovpd|vmaskmovps|vmaxpd|vmaxps|vmaxsd|vmaxss|vminpd|vminps|vminsd|vminss|vmovapd|vmovaps|vmovd|vmovddup|vmovdqa|vmovdqa32|vmovdqa64|vmovdqu|vmovdqu16|vmovdqu32|vmovdqu64|vmovdqu8|vmovhlps|vmovhpd|vmovhps|vmovlhps|vmovlpd|vmovlps|vmovmskpd|vmovmskps|vmovntdq|vmovntdqa|vmovntpd|vmovntps|vmovq|vmovsd|vmovshdup|vmovsldup|vmovss|vmovupd|vmovups|vmpsadbw|vmulpd|vmulps|vmulsd|vmulss|vorpd|vorps|vpabsb|vpabsd|vpabsq|vpabsw|vpackssdw|vpacksswb|vpackusdw|vpackuswb|vpaddb|vpaddd|vpaddq|vpaddsb|vpaddsw|vpaddusb|vpaddusw|vpaddw|vpalignr|vpand|vpandd|vpandn|vpandnd|vpandnq|vpandq|vpavgb|vpavgw|vpblendd|vpblendvb|vpblendw|vpbroadcastb|vpbroadcastd|vpbroadcastmb2d|vpbroadcastmb2q|vpbroadcastq|vpbroadcastw|vpclmulqdq|vpcmov|vpcmpb|vpcmpd|vpcmpeqb|vpcmpeqd|vpcmpeqq|vpcmpeqw|vpcmpestri|vpcmpestrm|vpcmpgtb|vpcmpgtd|vpcmpgtq|vpcmpgtw|vpcmpistri|vpcmpistrm|vpcmpq|vpcmpub|vpcmpud|vpcmpuq|vpcmpuw|vpcmpw|vpcomb|vpcomd|vpcompressd|vpcompressq|vpcomq|vpcomub|vpcomud|vpcomuq|vpcomuw|vpcomw|vpconflictd|vpconflictq|vperm2f128|vperm2i128|vpermb|vpermd|vpermi2b|vpermi2d|vpermi2pd|vpermi2ps|vpermi2q|vpermi2w|vpermil2pd|vpermil2ps|vpermilpd|vpermilps|vpermpd|vpermps|vpermq|vpermt2b|vpermt2d|vpermt2pd|vpermt2ps|vpermt2q|vpermt2w|vpermw|vpexpandd|vpexpandq|vpextrb|vpextrd|vpextrq|vpextrw|vpgatherdd|vpgatherdq|vpgatherqd|vpgatherqq|vphaddbd|vphaddbq|vphaddbw|vphaddd|vphadddq|vphaddsw|vphaddubd|vphaddubq|vphaddubw|vphaddudq|vphadduwd|vphadduwq|vphaddw|vphaddwd|vphaddwq|vphminposuw|vphsubbw|vphsubd|vphsubdq|vphsubsw|vphsubw|vphsubwd|vpinsrb|vpinsrd|vpinsrq|vpinsrw|vplzcntd|vplzcntq|vpmacsdd|vpmacsdqh|vpmacsdql|vpmacssdd|vpmacssdqh|vpmacssdql|vpmacsswd|vpmacssww|vpmacswd|vpmacsww|vpmadcsswd|vpmadcswd|vpmadd52huq|vpmadd52luq|vpmaddubsw|vpmaddwd|vpmaskmovd|vpmaskmovq|vpmaxsb|vpmaxsd|vpmaxsq|vpmaxsw|vpmaxub|vpmaxud|vpmaxuq|vpmaxuw|vpminsb|vpminsd|vpminsq|vpminsw|vpminub|vpminud|vpminuq|vpminuw|vpmovb2m|vpmovd2m|vpmovdb|vpmovdw|vpmovm2b|vpmovm2d|vpmovm2q|vpmovm2w|vpmovmskb|vpmovq2m|vpmovqb|vpmovqd|vpmovqw|vpmovsdb|vpmovsdw|vpmovsqb|vpmovsqd|vpmovsqw|vpmovswb|vpmovsxbd|vpmovsxbq|vpmovsxbw|vpmovsxdq|vpmovsxwd|vpmovsxwq|vpmovusdb|vpmovusdw|vpmovusqb|vpmovusqd|vpmovusqw|vpmovuswb|vpmovw2m|vpmovwb|vpmovzxbd|vpmovzxbq|vpmovzxbw|vpmovzxdq|vpmovzxwd|vpmovzxwq|vpmuldq|vpmulhrsw|vpmulhuw|vpmulhw|vpmulld|vpmullq|vpmullw|vpmultishiftqb|vpmuludq|vpor|vpord|vporq|vpperm|vprold|vprolq|vprolvd|vprolvq|vprord|vprorq|vprorvd|vprorvq|vprotb|vprotd|vprotq|vprotw|vpsadbw|vpscatterdd|vpscatterdq|vpscatterqd|vpscatterqq|vpshab|vpshad|vpshaq|vpshaw|vpshlb|vpshld|vpshlq|vpshlw|vpshufb|vpshufd|vpshufhw|vpshuflw|vpsignb|vpsignd|vpsignw|vpslld|vpslldq|vpsllq|vpsllvd|vpsllvq|vpsllvw|vpsllw|vpsrad|vpsraq|vpsravd|vpsravq|vpsravw|vpsraw|vpsrld|vpsrldq|vpsrlq|vpsrlvd|vpsrlvq|vpsrlvw|vpsrlw|vpsubb|vpsubd|vpsubq|vpsubsb|vpsubsw|vpsubusb|vpsubusw|vpsubw|vpternlogd|vpternlogq|vptest|vptestmb|vptestmd|vptestmq|vptestmw|vptestnmb|vptestnmd|vptestnmq|vptestnmw|vpunpckhbw|vpunpckhdq|vpunpckhqdq|vpunpckhwd|vpunpcklbw|vpunpckldq|vpunpcklqdq|vpunpcklwd|vpxor|vpxord|vpxorq|vrangepd|vrangeps|vrangesd|vrangess|vrcp14pd|vrcp14ps|vrcp14sd|vrcp14ss|vrcp28pd|vrcp28ps|vrcp28sd|vrcp28ss|vrcpps|vrcpss|vreducepd|vreduceps|vreducesd|vreducess|vrndscalepd|vrndscaleps|vrndscalesd|vrndscaless|vroundpd|vroundps|vroundsd|vroundss|vrsqrt14pd|vrsqrt14ps|vrsqrt14sd|vrsqrt14ss|vrsqrt28pd|vrsqrt28ps|vrsqrt28sd|vrsqrt28ss|vrsqrtps|vrsqrtss|vscalefpd|vscalefps|vscalefsd|vscalefss|vscatterdpd|vscatterdps|vscatterpf0dpd|vscatterpf0dps|vscatterpf0qpd|vscatterpf0qps|vscatterpf1dpd|vscatterpf1dps|vscatterpf1qpd|vscatterpf1qps|vscatterqpd|vscatterqps|vshuff32x4|vshuff64x2|vshufi32x4|vshufi64x2|vshufpd|vshufps|vsqrtpd|vsqrtps|vsqrtsd|vsqrtss|vstmxcsr|vsubpd|vsubps|vsubsd|vsubss|vtestpd|vtestps|vucomisd|vucomiss|vunpckhpd|vunpckhps|vunpcklpd|vunpcklps|vxorpd|vxorps|vzeroall|vzeroupper|wait|wrfsbase|wrgsbase|xadd|xchg|xgetbv|xor|xorpd|xorps|xrstor|xrstor64|xrstors|xrstors64|xsave|xsave64|xsavec|xsavec64|xsaveopt|xsaveopt64|xsaves|xsaves64|xsetbv|hlt|movabs|endbr32|endbr64/;
// const MNEMONIC = /mov/;
// const MNEMONIC = "mov";
// const MNEMONIC = new RustRegex("mov");

const ARBITRARY_TEXT = /[^\n]+/; // used in title_dir and echo_dir
const SYNTACTICAL_TEXT = /[^\n,>]+/; // used in macro args
const NON_WHITESPACE_CHARACTER = /[^\s\n]/;
const ANY_CHAR_EXCEPT_QUOTE = /[^'"\n]/;
const ALPHA = /[a-zA-Z]|@|_|\$|\?/;
const DEC_DIGIT = /[0-9]/;
const DEC_NUMBER = /[0-9]+/;
const HEX_DIGIT = /[a-fA-F]/;
const DELIMITER = /\S/;
const FILE_CHAR_LIST = /\S+/;
const DIGITS = /[0-9][0-9A-Fa-f]*/;
const RADIX_OVERRIDE = /[hoqtyHOQTY]/;
const TEXT = /\!?[^>\n]+/;
const CONSTANT = /[0-9][0-9A-Fa-f]*[hoqtyHOQTY]?/;

const SIGN = /\+|\-/;
const BINARY_OP = /==|\!=|>=|<=|>|<|&/;
const ADD_OP = /\+|\-/;
const OR_OP = /x?or/;
const MUL_OP = /\*|\/|mod/;
const REL_OP = /eq|ne|lt|le|gt|ge/;
const SHIFT_OP = /sh[lr]/;
const QUOTE = /['"]/;
const BOOL = /true|false/;

const BYTE_REGISTER = /[abcd][lh]/;
const GP_REGISTER = /[er]?[abcd]x/;
const INDEX_REGISTER = /[er]?[ds]i/;
const STACK_REGISTER = /[er][sb]p/;
const AMD_REGISTERS = /r(8|9|10|11|12|13|14|15)[bwd]?/;
const SPECIAL_REGISTERS = /cr0|cr2|cr3|dr[0-3]|dr[67]|tr[3-7]/;
const SIMD_REGISTER = /[xyz]mm(0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15)/;
const SEGMENT_REGISTER = /[cdefgs]s/;

const CONTEXT_ITEM = /assumes|radix|listing|cpu|all/;
const DATA_TYPE = /byte|sbyte|word|sword|dword|sdword|fword|qword|sqword|tbyte|oword|real4|real8|real10|mmword|xmmword|ymmword/;

const PROCESSOR = /\.[3-6]86[pP]?/;
const COPROCESSOR = /.8087|.287|.387|.NO87/;

const STRUCT_HDR = /struct?|union/;

const STACK_OPTION = /(near|far)stack/;
const OFFSET_TYPE = /group|segment|flat/;
const EXTERN_KEY = /exte?rn|externdef/;
const REPEAT_DIR = /repeat|rept/;
const FOR_DIR = /for|irp/;
const FORC_DIR = /forc|irpc/;
const INSTR_PREFIX = /rep|repe|repz|repne|repnz|lock/;
const LIST_OPTION = /.list|.nolist|.xlist|.listall|.listif|.lfcond|.nolistif|.sfcond|.tfcond|.listmacroall|.lall|.nolistmacro|.sall|.listmacro|.xall/;
const MEM_OPTION = /tiny|small|medium|compact|large|huge|flat/;
const NEAR_FAR = /near|far/;
const O_VISIBILITY = /public|private|export/;
const SEG_ALIGN = /byte|word|dword|para|page/;
const SEG_ORDER_DIR = /.alpha|.seq|.dosseg|dosseg/;
const SEG_SIZE = /use16|use32|flat/;
const TITLE_TYPE = /title|subtitle|subttl/;
const LANG_TYPE = /c|pascal|fortran|basic|syscall|stdcall/;
const MAP_TYPE = /all|none|notpublic/;
const FLAG_NAME = /zero?|carry?|overflow?|sign?|parity?/;

    // seg_ro: _ => "readonly",


const list = listItem => seq(listItem, repeat(seq(",", listItem)));
const listWithEol = (listItem, eol) => seq(listItem, repeat(seq(",", optional(eol), listItem)));
const tokenFromRegex = regex => token(prec(1, regex));

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
    /[ \t]+/,
    $.comment_line,
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.proto_spec],
    [$.proto_list],
    [$.proto_arg_list],
    [$.qualifier],
    [$.prefix_expression, $.binary_expression],
    [$.struct_body],
    [$.init_value, $.record_field_list],
    [$.init_value, $.old_record_field_list],
    [$.in_seg_dir_list],
    [$.segment_def],
    [$.parm_list],
    [$.expression_terminal, $.macro_arg],
  ],

  // reserved: {
  //   global: $ => [
  //     "include",
  //     "proc",
  //   ],
  // },

  rules: {
    source_file: $ => $.module,

    module: $ => seq($.directive_list, optional($.end_dir)),
    end_dir: $ => seq("end", optional($.expression), $._eol),

    _eol: $ => choice($.comment_line, /\n+/),
    comment_line: _ => /;.*\n+/,

    _asm_instruction: $ => seq(
      field("mnemonic", token(prec(1, MNEMONIC))),
      field("args", optional($.expr_list)),
    ),
    instruction: $ => seq(optional($.instr_prefix), $._asm_instruction, $._eol), // official grammar error (possible): I belive there should be an eol (;;) here

    identifier: _ => /[a-zA-Z@_$?][a-zA-Z0-9@_$?]*/,

    register: $ => choice(
      tokenFromRegex(BYTE_REGISTER),
      tokenFromRegex(GP_REGISTER),
      tokenFromRegex(INDEX_REGISTER),
      tokenFromRegex(STACK_REGISTER),
      tokenFromRegex(AMD_REGISTERS),
      tokenFromRegex(SPECIAL_REGISTERS),
      tokenFromRegex(SIMD_REGISTER),
      tokenFromRegex(SEGMENT_REGISTER),
      seq("st", $.expression),
    ),
    reg_list: $ => repeat1($.register),


    // building blocks
    
    id_list: $ => list($.identifier),
    stext: $ => repeat1(ANY_CHAR_EXCEPT_QUOTE),
    string: $ => seq($.quote, optional($.stext), $.quote),
    text_literal: $ => seq("<", TEXT, ">", $._eol),
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
        [PREC.bit_section, choice("high", "low", "highword", "lowword")],
        [PREC.offset, choice("offset", "seg", "lroffset", "type", "this")],
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
      seq("[", $.expression, "]"),
      seq("width", $.identifier),
      seq("mask", $.identifier),
      seq("size", $._size_arg),
      seq("sizeof", $._size_arg),
      seq("length", $.identifier),
      seq("lengthof", $.identifier),
      CONSTANT,
      $.string,
      $.type,
      "@f", // official grammar error: missing
      "@b", // official grammar error: missing
      "$",
      prec(PREC.e11 + 4, seq("st", "(", $.expression, ")")),
      prec(PREC.e11 + 3, "st"),
      prec(PREC.e11 + 2, $.register),
      prec(PREC.e11 + 1, $.identifier),
    ),


    // uses expressions

    expr_list: $ => list($.expression),
    _bit_field_size: $ => alias($.expression, $.bit_field_size),
    bit_def: $ => seq($.identifier, ":", $._bit_field_size, optional(seq("=", $.expression))),
    bit_def_list: $ => listWithEol($.bit_def, $._eol),
    record_dir: $ => seq($.identifier, "record", $.bit_def_list, $._eol),

    _comm_type: $ => alias($.expression, $.comm_type),
    comm_decl: $ => seq(optional($.near_far), optional($.lang_type), $.identifier, ":", $._comm_type, optional(seq(":", $.expression))),
    comm_list: $ => list($.comm_decl),
    comm_dir: $ => seq("comm", $.comm_list, $._eol),

    init_value: $ => choice(
      // $.string, // handled by expression
      "?",
      seq($.expression, optional(seq("dup", "(", $.scalar_inst_list, ")"))),
      $.float_number,
      $.bcd_const,
    ),
    scalar_inst_list: $ => listWithEol($.init_value, $._eol),

    field_init: $ => choice($.init_value, $.struct_instance),
    field_init_list: $ => listWithEol($.field_init, $._eol),

    record_field_list: $ => listWithEol($.expression, $._eol),
    old_record_field_list: $ => list($.expression),
    record_instance: $ => choice(
      seq("{", optional($._eol), $.record_field_list, optional($._eol), "}"),
      seq("<", $.old_record_field_list, ">"),
      seq($.expression, "dup", "(", $.record_instance, ")"),
    ),
    record_inst_list: $ => listWithEol($.record_instance, $._eol),
    record_const: $ => choice(
      seq($.identifier, "{", $.old_record_field_list, "}"),
      seq($.identifier, "<", $.old_record_field_list, ">"),
    ),

    data_item: $ => choice(
      prec(1, seq(field("type", $._data_decl), field("value", $.scalar_inst_list))),
      prec(2, seq(field("type", $.identifier), field("value", $.struct_inst_list))),
      prec(3, seq(field("type", $.identifier), field("value", $.record_inst_list))),
    ),
    data_dir: $ => seq(optional($.identifier), $.data_item, $._eol),

    seg_dir: $ => choice(
      seq(".code", optional($.identifier)),
      ".data",
      ".data?",
      ".const",
      seq(".fardata", optional($.identifier)),
      seq(".fardata?", optional($.identifier)),
      seq(".stack", optional($.expression)),
    ),
    simple_seg_dir: $ => seq($.seg_dir, $._eol),

    text_item: $ => choice($.text_literal, $.identifier, seq("%", $.expression)),
    text_list: $ => listWithEol($.text_item, $._eol),
    _text_len: $ => alias($.expression, $.text_len),
    _text_start: $ => alias($.expression, $.text_start),
    text_macro_dir: $ => choice(
      seq("catstr", optional($.text_list)),
      seq("textequ", optional($.text_list)),
      seq("sizestr", $.text_item),
      seq("substr", $.text_item, ",", $._text_start, optional(seq(",", $._text_len))),
      seq("instr", optional(seq($._text_start, ",")), $.text_item, ",", $.text_item),
    ),
    text_dir: $ => seq($.identifier, $.text_macro_dir, $._eol),

    // official grammar error: missing a | to indicate choice
    until_dir: $ => choice(
      seq(".until", $.expression, $._eol),
      seq(".untilcxz", optional($.expression), $._eol),
    ),

    offset_dir_type: $ => choice(
      "even",
      seq("org", $.expression),
      seq("align", optional($.expression)),
    ),
    offset_dir: $ => seq($.offset_dir_type, $._eol),

    struct_instance: $ => choice(
      seq("<", optional($.field_init_list), ">"),
      seq("{", optional($._eol), optional($.field_init_list), optional($._eol), "}"),
      seq($.expression, "dup", "(", $.struct_inst_list, ")"),
    ),
    struct_inst_list: $ => listWithEol($.struct_instance, $._eol),
    struct_item: $ => choice(
      $.data_dir,
      $._general_dir,
      $.offset_dir,
      $.nested_struct,
    ),
    nested_struct: $ => seq($.struct_hdr, optional($.identifier), $._eol, $.struct_body, "ends", $._eol),
    struct_body: $ => repeat1($.struct_item),
    _field_align: $ => alias($.expression, $.field_align),
    struct_dir: $ => seq(
      $.identifier, $.struct_hdr, optional($._field_align), optional(seq(",", "nonunique")), $._eol,
      $.struct_body,
      $.identifier, "ends", $._eol,
    ),

    macro_arg: $ => choice(
      seq("%", $.expression),
      seq("%", $.identifier),
      seq("%", $.identifier, "(", $.macro_arg_list, ")"),
      $.string,
      SYNTACTICAL_TEXT,
      seq("<", SYNTACTICAL_TEXT, ">"),
    ),
    macro_arg_list: $ => list($.macro_arg),

    // official grammar error: having this just be `directive` does not allow for instructions. inSegDirList does. see `macro_body`.
    macro_stmt: $ => choice(
      // $._directive,
      $.exitm_dir,
      seq(":", $.identifier),
      seq("goto", $.identifier),
    ),
    macro_stmt_list: $ => repeat1(seq($.macro_stmt, $._eol)),
    macro_body: $ => seq(
      optional($.local_list),
      choice($.macro_stmt_list, $.in_seg_dir_list)
    ),
    macro_dir: $ => seq(
      $.identifier, "macro", optional($.macro_parm_list), $._eol,
      $.macro_body,
      "endm", $._eol,
    ),
    macro_for: $ => seq(
      $.for_dir, $.for_parm, ",", "<", $.macro_arg_list, ">", $._eol,
      optional($.macro_body),
      "endm", $._eol,
    ),
    macro_forc: $ => seq(
      FORC_DIR, $.identifier, ",", $.text_literal, $._eol,
      $.macro_body,
      "endm", $._eol,
    ),
    macro_repeat: $ => seq(
      $.repeat_dir, $.expression, $._eol,
      $.macro_body,
      "endm", $._eol,
    ),
    macro_while: $ => seq(
      "while", $.expression, $._eol,
      $.macro_body,
      "endm", $._eol,
    ),
    macro_call: $ => choice(
      seq($.identifier, optional($.macro_arg_list), $._eol),
      seq($.identifier, "(", optional($.macro_arg_list), ")"),
    ),

    eq_dir: $ => seq($.identifier, "=", $.expression, $._eol),
    equ_type: $ => choice($.expression, $.text_literal),
    equ_dir: $ => seq($.identifier, "equ", $.equ_type, $._eol),

    radix_dir: $ => seq(".radix", $.expression, $._eol),

    title_dir: $ => seq($.title_type, ARBITRARY_TEXT, $._eol),

    page_expr: $ => choice(
      "+",
      seq($.expression, optional(seq(",", $.expression))),
    ),
    page_dir: $ => seq("page", optional($.page_expr), $._eol),

    // official grammar error: this isn't even listed
    alias_dir: $ => seq("alias", $.text_literal, "=", $.text_literal),

    _general_dir: $ => choice(
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
      // $.macro_call,
    ),
    _directive: $ => choice($._general_dir, $.segment_def),
    directive_list: $ => repeat1($._directive),

    if_statement: $ => choice(
      seq("if", $.expression),
      seq("ife", $.expression),
      seq("ifb", $.text_item),
      seq("ifnb", $.text_item),
      seq("ifdef", $.identifier),
      seq("ifndef", $.identifier),
      seq("ifdif", $.text_item , $.text_item),
      seq("ifdifi", $.text_item , $.text_item),
      seq("ifidn", $.text_item , $.text_item),
      seq("ifidni", $.text_item , $.text_item),
      "if1",
      "if2",
    ),
    if_dir: $ => seq($.if_statement, $._eol,
      $.directive_list,
      repeat($.elseif_block),
      optional(seq(
        "else", $._eol,
        $.directive_list,
      )),
      "endif", $._eol, // official grammar error: "endif" is nowhere to be found
    ),
    elseif_statement: $ => choice(
      seq("elseif", $.expression),
      seq("elseife", $.expression),
      seq("elseifb", $.text_item),
      seq("elseifnb", $.text_item),
      seq("elseifdef", $.identifier),
      seq("elseifndef", $.identifier),
      seq("elseifdif", $.text_item, $.text_item),
      seq("elseifdifi", $.text_item, $.text_item),
      seq("elseifidn", $.text_item, $.text_item),
      seq("elseifidni", $.text_item, $.text_item),
      "elseif1",
      "elseif2",
    ),
    elseif_block: $ => seq($.elseif_statement, $._eol, $.directive_list),
    opt_text: $ => seq(",", $.text_item),
    error_opt: $ => choice(
      seq(".ERR", optional($.text_item)),
      seq(".ERRE", $.expression, optional($.opt_text)),
      seq(".ERRNZ", $.expression, optional($.opt_text)),
      seq(".ERRB", $.text_item, optional($.opt_text)),
      seq(".ERRNB", $.text_item, optional($.opt_text)),
      seq(".ERRDEF", $.identifier, optional($.opt_text)),
      seq(".ERRNDEF", $.identifier, optional($.opt_text)),
      seq(".ERRDIF", $.text_item, $.text_item, optional($.opt_text)),
      seq(".ERRDIFI", $.text_item, $.text_item, optional($.opt_text)),
      seq(".ERRIDN", $.text_item, $.text_item, optional($.opt_text)),
      seq(".ERRIDNI", $.text_item, $.text_item, optional($.opt_text)),
      seq(".ERR1", optional($.text_item)),
      seq(".ERR2", optional($.text_item)),
    ),
    error_dir: $ => seq($.error_opt, $._eol),


    // idk

    cref_option: $ => choice(
      ".cref",
      seq(".xcref", optional($.id_list)),
      seq(".nocref", optional($.id_list)),
    ),
    cref_dir: $ => seq($.cref_option, $._eol),
    _data_decl: $ => choice("db", "dw", "dd", "df", "dq", "dt", $._data_type, $.identifier),
    distance: $ => choice($.near_far, "near16", "near32", "far16", "far32"),
    type: $ => choice(
      $.identifier,
      $.distance,
      $._data_type,
    ),
    qualified_type: $ => choice(
      $.type,
      seq(optional($.distance), "ptr", optional($.qualified_type)),
    ),

    proto_arg: $ => seq(optional($.identifier), ":", $.qualified_type),
    proto_list: $ => listWithEol($.proto_arg, $._eol),
    proto_arg_list: $ => seq(
      optional(seq(",", optional($._eol))),
      choice(
        seq($.proto_list, optional(seq(",", optional($._eol), optional($.identifier), ":vararg"))),
        seq(optional($.identifier), ":vararg"),
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
      $.identifier,
    ),
    proto_type_dir: $ => seq($.identifier, "proto", optional($.proto_spec), $._eol), // official grammar error (possibly): I believe there should be an eol (;;) here

    pub_def: $ => seq(optional($.lang_type), $.identifier),
    pub_list: $ => listWithEol($.pub_def, $._eol),
    public_dir: $ => seq("public", $.pub_list, $._eol),

    purge_dir: $ => seq("purge", $.id_list),

    parm_type: $ => choice(
      "req",
      seq("=", $.text_literal),
      "vararg",
    ),
    macro_parm: $ => seq($.identifier, optional(seq(":", $.parm_type))),
    macro_parm_list: $ => listWithEol($.macro_parm, $._eol),
    parm: $ => choice(
      seq($.identifier, optional(seq(":", $.qualified_type))),
      seq($.identifier, optional($.expression), optional(seq(":", $.qualified_type))),
    ),
    parm_list: $ => listWithEol($.parm, $._eol),

    model_opt: $ => choice($.lang_type, $.stack_option),
    model_opt_list: $ => list($.model_opt),
    model_dir: $ => seq(".model", $.mem_option, optional(seq(",", $.model_opt_list)), $._eol),

    name_dir: $ => seq("name", $.identifier, $._eol),

    p_options: $ => choice(
      seq($.distance, optional($.lang_type), optional($.o_visibility)),
      seq($.lang_type, optional($.o_visibility)),
      $.o_visibility,
    ),

    for_parm_type: $ => choice("req", seq("=", $.text_literal)),
    for_parm: $ => seq($.identifier, optional(seq(":", $.for_parm_type))),

    frame_expr: $ => choice(
      seq("seg", $.identifier),
      seq("dgroup", ":", $.identifier),
      seq(SEGMENT_REGISTER, ":", $.identifier),
      $.identifier,
    ),

    seg_id_list: $ => list($.identifier),
    group_dir: $ => seq($.identifier, "group", $.seg_id_list),

    file_spec: $ => choice(FILE_CHAR_LIST, $.text_literal),
    include_dir: $ => seq("include", $.file_spec, $._eol),
    include_lib_dir: $ => seq("includelib", $.file_spec, $._eol),

    label_def: $ => choice(
      seq($.identifier, ":"),
      seq($.identifier, "::"),
      "@@:",
    ),
    label_dir: $ => seq($.identifier, "label", $.qualified_type, $._eol),

    qualifier: $ => choice(
      $.qualified_type,
      seq("proto", optional($.proto_spec)),
    ),
    typedef_dir: $ => seq($.identifier, "typedef", $.qualifier, $._eol), // official grammar error (possibly): I believe there should be an eol (;;) here

    // NOTE: I think the bnf grammar might be incorrect. There's no way to get from "extern" to "proto" in the grammar.
    // If instead of `qualified_type`, it was `qualifier`, then it would make sense.
    extern_type: $ => choice("abs", $.qualifier), 
    extern_def: $ => seq(optional($.lang_type), $.identifier, optional(seq("(", $.identifier, ")")), ":", $.extern_type),
    extern_list: $ => listWithEol($.extern_def, $._eol),
    extern_dir: $ => seq($.extern_key, $.extern_list, $._eol),

    assume_val: $ => choice($.qualified_type, "nothing", "error"),
    assume_seg_val: $ => choice($.frame_expr, "nothing", "error"),
    assume_seg_reg: $ => prec(1, seq(SEGMENT_REGISTER, ":", $.assume_seg_val)),
    assume_register: $ => choice($.assume_seg_reg, $.assume_reg),
    assume_reg: $ => seq($.register, ":", $.assume_val),
    assume_list: $ => list($.assume_register),
    assume_dir: $ => choice(
      seq("assume", $.assume_list, $._eol),
      seq("assume nothing", $._eol),
    ),

    // official grammar error: it omits an `|` indicating a choice between "echo" and "%out"
    echo_dir: $ => choice(
      seq("echo", ARBITRARY_TEXT, $._eol),
      seq("%out", ARBITRARY_TEXT, $._eol),
    ),

    list_dir: $ => seq($.list_option, $._eol),

    local_def: $ => seq("local", $.id_list, $._eol),
    local_list: $ => repeat1($.local_def),
    local_dir: $ => seq("local", $.parm_list, $._eol),
    local_dir_list: $ => repeat1($.local_dir),

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
      $._class_name,
    ),
    seg_option_list: $ => repeat1($.seg_option),
    segment_dir: $ => seq($.identifier, "segment", optional($.seg_option_list), $._eol),
    segment_def: $ => choice(
      seq($.segment_dir, optional($.in_seg_dir_list), $.ends_dir),
      seq($.simple_seg_dir, optional($.in_seg_dir_list), optional($.ends_dir)),
    ),

    context_dir: $ => choice(
      seq("pushcontext", $.context_item_list, $._eol),
      seq("popcontext", $.context_item_list, $._eol),
    ),

    endp_dir: $ => seq($.identifier, "endp", $._eol),
    ends_dir: $ => seq($.identifier, "ends", $._eol),
    exitm_dir: $ => choice(
      seq(":", "exitm"),
      seq("exitm", $.text_item),
    ),

    special_chars: _ => /:|\.|\[|\]|\(|\)|<|>|\{|\}|\+|-|\/|\*|&|%|!|'|\\|=|;|,|"|\s|\n/,

    startup_dir: $ => seq(".startup", $._eol),

    uses_regs: $ => seq("uses", $.reg_list),

    _size_arg: $ => alias($.expression, $.size_arg),

    exit_dir: $ => seq(".exit", $.expression, $._eol),

    invoke_arg: $ => choice(
      seq($.register, "::", $.register),
      $.expression,
      seq("addr", $.expression),
    ),
    invoke_list: $ => listWithEol($.invoke_arg, $._eol),
    invoke_dir: $ => seq("invoke", $.expression, optional(seq(",", optional($._eol), $.invoke_list)), $._eol),

    _in_segment_dir: $ => choice(
      $.instruction,
      $.data_dir,
      $.startup_dir,
      $.exit_dir,
      $.offset_dir,
      $.label_dir,
      $.invoke_dir,
      $._general_dir,
      $.control_dir,
      seq($.proc_dir, optional($.local_dir_list), optional($.in_seg_dir_list), $.endp_dir),
    ),
    // official grammar error: it is possible to have a label with no inSegmentDir
    _in_seg_dir: $ => choice(
      seq($.label_def, $._eol),
      seq($.label_def, $._in_segment_dir),
      // TODO: figure out how to parse either
      //  1. seq($.label_def, $.eol)
      //  OR
      //  2. seq($.label_def, $._in_segment_dir)
      //  in that order of precedence.
      //  The issue is that TS is always choosing 2, and _in_segment_dir is just consuming whatever is on the next line, which leads to incorrect parse trees
      //  prec() has no effect because the two choices technically do not conflict.
      //  Tree Sitter chooses the longest parse tree, which is always 2 here.
      //  might possibly need to do an external parser but that's last resort
      seq($._in_segment_dir),
    ),
    in_seg_dir_list: $ => repeat1($._in_seg_dir),

    block_statements: $ => choice(
      $.directive_list,
      seq(".continue", optional(seq(".if", $.expression))),
      seq(".break", optional(seq(".if", $.expression))),
    ),
    control_if: $ => seq(
      ".if", $.expression, $._eol,
      $.directive_list,
      repeat($.control_elseif),
      optional(seq(
        ".else", $._eol,
        $.directive_list,
      )),
      ".endif", $._eol,
    ),
    control_elseif: $ => seq(
      ".elseif", $.expression, $._eol,
      $.directive_list,
    ),
    while_block: $ => seq(
      ".while", $.expression, $._eol,
      $.block_statements, $._eol,
      ".endw",
    ),
    repeat_block: $ => seq(
      ".repeat", $._eol,
      $.block_statements, $._eol, $.until_dir, $._eol,
    ),
    control_block: $ => choice($.while_block, $.repeat_block),
    control_dir: $ => choice($.control_if, $.control_block),

    proc_parm_list: $ => seq(
      optional(seq(",", optional($._eol))),
      choice(
        seq($.parm_list, optional(seq(",", optional($._eol), $.identifier, ":vararg"))),
        seq($.identifier, ":vararg"),
      ),
    ),
    proc_dir: $ => seq(
      $.identifier, "proc", optional($.p_options), optional(seq("<", $.macro_arg_list, ">")),
      optional($.uses_regs), optional($.proc_parm_list), $._eol,
    ),


    // option

    option_item: $ => choice(
      seq("casemap", ":", $.map_type),
      "dotname",
      "nodotname",
      "emulator",
      "noemulator",
      seq("epilogue", ":", $.identifier),
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
      seq("prologue", ":", $.identifier),
      "readonly",
      "noreadonly",
      "scoped",
      "noscoped",
      seq("segment", ":", $.seg_size),
      seq("setif2", ":", $.bool),
    ),
    option_list: $ => listWithEol($.option_item, $._eol),
    option_dir: $ => seq("option", $.option_list, $._eol),


    // aliases

    _class_name: $ => alias($.string, $.class_name),


    // terminals

    context_item: _ => choice("assumes", "radix", "listing", "cpu", "all"),
    context_item_list: $ => list($.context_item),
    _data_type: _ => choice("byte", "sbyte", "word", "sword", "dword", "sdword", "fword", "qword", "sqword", "tbyte", "oword", "real4", "real8", "real10", "mmword", "xmmword", "ymmword"),

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
      prec(2, seq($.processor, $._eol)),
      prec(1, seq($.coprocessor, $._eol)),
    ),

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
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq(TEXT, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, TEXT, $._eol),
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq(TEXT, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, TEXT, $._eol),
    // comment_dir: _ => /comment\s+(\S)\n([^\n]*\n)*$1/,
  }
});
