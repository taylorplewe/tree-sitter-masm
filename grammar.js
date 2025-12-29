/**
 * @file Microsoft Macro Assembler x86 syntax
 * @author Taylor Plewe <tplewe@outlook.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const MNEMONIC = new RustRegex(
  "\baaa\b|\baad\b|\baam\b|\baas\b|\badc\b|\badcx\b|\badd\b|\baddpd\b|\baddps\b|\baddsd\b|\baddss\b|\baddsubpd\b|\baddsubps\b|\badox\b|\baesdec\b|\baesdeclast\b|\baesenc\b|\baesenclast\b|\baesimc\b|\baeskeygenassist\b|\band\b|\bandn\b|\bandnpd\b|\bandnps\b|\bandpd\b|\bandps\b|\bbextr\b|\bblcfill\b|\bblci\b|\bblcic\b|\bblcmsk\b|\bblcs\b|\bblendpd\b|\bblendps\b|\bblendvpd\b|\bblendvps\b|\bblsfill\b|\bblsi\b|\bblsic\b|\bblsmsk\b|\bblsr\b|\bbndcl\b|\bbndcn\b|\bbndcu\b|\bbndldx\b|\bbndmk\b|\bbndmov\b|\bbndstx\b|\bbsf\b|\bbsr\b|\bbswap\b|\bbt\b|\bbtc\b|\bbtr\b|\bbts\b|\bbzhi\b|\bcall\b|\bcbw\b|\bcdq\b|\bcdqe\b|\bclac\b|\bclc\b|\bcld\b|\bclflush\b|\bclflushopt\b|\bclwb\b|\bclzero\b|\bcmc\b|\bcmova\b|\bcmovae\b|\bcmovb\b|\bcmovbe\b|\bcmovc\b|\bcmove\b|\bcmovg\b|\bcmovge\b|\bcmovl\b|\bcmovle\b|\bcmovna\b|\bcmovnae\b|\bcmovnb\b|\bcmovnbe\b|\bcmovnc\b|\bcmovne\b|\bcmovng\b|\bcmovnge\b|\bcmovnl\b|\bcmovnle\b|\bcmovno\b|\bcmovnp\b|\bcmovns\b|\bcmovnz\b|\bcmovo\b|\bcmovp\b|\bcmovpe\b|\bcmovpo\b|\bcmovs\b|\bcmovz\b|\bcmp\b|\bcmppd\b|\bcmpps\b|\bcmpsb\b|\bcmpsd\b|\bcmpsq\b|\bcmpss\b|\bcmpsw\b|\bcmpxchg\b|\bcmpxchg16b\b|\bcmpxchg8b\b|\bcomisd\b|\bcomiss\b|\bcpuid\b|\bcqo\b|\bcrc32\b|\bcvtdq2pd\b|\bcvtdq2ps\b|\bcvtpd2dq\b|\bcvtpd2pi\b|\bcvtpd2ps\b|\bcvtpi2pd\b|\bcvtpi2ps\b|\bcvtps2dq\b|\bcvtps2pd\b|\bcvtps2pi\b|\bcvtsd2si\b|\bcvtsd2ss\b|\bcvtsi2sd\b|\bcvtsi2ss\b|\bcvtss2sd\b|\bcvtss2si\b|\bcvttpd2dq\b|\bcvttpd2pi\b|\bcvttps2dq\b|\bcvttps2pi\b|\bcvttsd2si\b|\bcvttss2si\b|\bcwd\b|\bcwde\b|\bdaa\b|\bdas\b|\bdec\b|\bdiv\b|\bdivpd\b|\bdivps\b|\bdivsd\b|\bdivss\b|\bdppd\b|\bdpps\b|\bemms\b|\benter\b|\bextractps\b|\bextrq\b|\bf2xm1\b|\bfabs\b|\bfadd\b|\bfaddp\b|\bfbld\b|\bfbstp\b|\bfchs\b|\bfclex\b|\bfcmovb\b|\bfcmovbe\b|\bfcmove\b|\bfcmovnb\b|\bfcmovnbe\b|\bfcmovne\b|\bfcmovnu\b|\bfcmovu\b|\bfcom\b|\bfcomi\b|\bfcomip\b|\bfcomp\b|\bfcompp\b|\bfcos\b|\bfdecstp\b|\bfdiv\b|\bfdivp\b|\bfdivr\b|\bfdivrp\b|\bfemms\b|\bffree\b|\bfiadd\b|\bficom\b|\bficomp\b|\bfidiv\b|\bfidivr\b|\bfild\b|\bfimul\b|\bfincstp\b|\bfinit\b|\bfist\b|\bfistp\b|\bfisttp\b|\bfisub\b|\bfisubr\b|\bfld\b|\bfld1\b|\bfldcw\b|\bfldenv\b|\bfldl2e\b|\bfldl2t\b|\bfldlg2\b|\bfldln2\b|\bfldpi\b|\bfldz\b|\bfmul\b|\bfmulp\b|\bfnclex\b|\bfninit\b|\bfnop\b|\bfnsave\b|\bfnstcw\b|\bfnstenv\b|\bfnstsw\b|\bfpatan\b|\bfprem\b|\bfprem1\b|\bfptan\b|\bfrndint\b|\bfrstor\b|\bfsave\b|\bfscale\b|\bfsin\b|\bfsincos\b|\bfsqrt\b|\bfst\b|\bfstcw\b|\bfstenv\b|\bfstp\b|\bfstsw\b|\bfsub\b|\bfsubp\b|\bfsubr\b|\bfsubrp\b|\bftst\b|\bfucom\b|\bfucomi\b|\bfucomip\b|\bfucomp\b|\bfucompp\b|\bfwait\b|\bfxam\b|\bfxch\b|\bfxrstor\b|\bfxrstor64\b|\bfxsave\b|\bfxsave64\b|\bfxtract\b|\bfyl2x\b|\bfyl2xp1\b|\bhaddpd\b|\bhaddps\b|\bhsubpd\b|\bhsubps\b|\bidiv\b|\bimul\b|\bin\b|\binc\b|\binsb\b|\binsd\b|\binsertps\b|\binsertq\b|\binsw\b|\bint\b|\bint3\b|\binto\b|\bja\b|\bjae\b|\bjb\b|\bjbe\b|\bjc\b|\bje\b|\bjecxz\b|\bjg\b|\bjge\b|\bjl\b|\bjle\b|\bjmp\b|\bjna\b|\bjnae\b|\bjnb\b|\bjnbe\b|\bjnc\b|\bjne\b|\bjng\b|\bjnge\b|\bjnl\b|\bjnle\b|\bjno\b|\bjnp\b|\bjns\b|\bjnz\b|\bjo\b|\bjp\b|\bjpe\b|\bjpo\b|\bjs\b|\bjz\b|\bkaddb\b|\bkaddd\b|\bkaddq\b|\bkaddw\b|\bkandb\b|\bkandd\b|\bkandnb\b|\bkandnd\b|\bkandnq\b|\bkandnw\b|\bkandq\b|\bkandw\b|\bkmovb\b|\bkmovd\b|\bkmovq\b|\bkmovw\b|\bknotb\b|\bknotd\b|\bknotq\b|\bknotw\b|\bkorb\b|\bkord\b|\bkorq\b|\bkortestb\b|\bkortestd\b|\bkortestq\b|\bkortestw\b|\bkorw\b|\bkshiftlb\b|\bkshiftld\b|\bkshiftlq\b|\bkshiftlw\b|\bkshiftrb\b|\bkshiftrd\b|\bkshiftrq\b|\bkshiftrw\b|\bktestb\b|\bktestd\b|\bktestq\b|\bktestw\b|\bkunpckbw\b|\bkunpckdq\b|\bkunpckwd\b|\bkxnorb\b|\bkxnord\b|\bkxnorq\b|\bkxnorw\b|\bkxorb\b|\bkxord\b|\bkxorq\b|\bkxorw\b|\blahf\b|\blddqu\b|\bldmxcsr\b|\blea\b|\bleave\b|\blfence\b|\blodsb\b|\blodsd\b|\blodsq\b|\blodsw\b|\bloop\b|\bloope\b|\bloopne\b|\blzcnt\b|\bmaskmovdqu\b|\bmaskmovq\b|\bmaxpd\b|\bmaxps\b|\bmaxsd\b|\bmaxss\b|\bmfence\b|\bminpd\b|\bminps\b|\bminsd\b|\bminss\b|\bmov\b|\bmovapd\b|\bmovaps\b|\bmovbe\b|\bmovd\b|\bmovddup\b|\bmovdq2q\b|\bmovdqa\b|\bmovdqu\b|\bmovhlps\b|\bmovhpd\b|\bmovhps\b|\bmovlhps\b|\bmovlpd\b|\bmovlps\b|\bmovmskpd\b|\bmovmskps\b|\bmovntdq\b|\bmovntdqa\b|\bmovnti\b|\bmovntpd\b|\bmovntps\b|\bmovntq\b|\bmovntsd\b|\bmovntss\b|\bmovq\b|\bmovq2dq\b|\bmovsb\b|\bmovsd\b|\bmovshdup\b|\bmovsldup\b|\bmovsq\b|\bmovss\b|\bmovsw\b|\bmovsx\b|\bmovsxd\b|\bmovupd\b|\bmovups\b|\bmovzx\b|\bmpsadbw\b|\bmul\b|\bmulpd\b|\bmulps\b|\bmulsd\b|\bmulss\b|\bmulx\b|\bneg\b|\bnop\b|\bnot\b|\bor\b|\borpd\b|\borps\b|\bout\b|\boutsb\b|\boutsd\b|\boutsw\b|\bpabsb\b|\bpabsd\b|\bpabsw\b|\bpackssdw\b|\bpacksswb\b|\bpackusdw\b|\bpackuswb\b|\bpaddb\b|\bpaddd\b|\bpaddq\b|\bpaddsb\b|\bpaddsw\b|\bpaddusb\b|\bpaddusw\b|\bpaddw\b|\bpalignr\b|\bpand\b|\bpandn\b|\bpause\b|\bpavgb\b|\bpavgusb\b|\bpavgw\b|\bpblendvb\b|\bpblendw\b|\bpclmulqdq\b|\bpcmpeqb\b|\bpcmpeqd\b|\bpcmpeqq\b|\bpcmpeqw\b|\bpcmpestri\b|\bpcmpestrm\b|\bpcmpgtb\b|\bpcmpgtd\b|\bpcmpgtq\b|\bpcmpgtw\b|\bpcmpistri\b|\bpcmpistrm\b|\bpcommit\b|\bpdep\b|\bpext\b|\bpextrb\b|\bpextrd\b|\bpextrq\b|\bpextrw\b|\bpf2id\b|\bpf2iw\b|\bpfacc\b|\bpfadd\b|\bpfcmpeq\b|\bpfcmpge\b|\bpfcmpgt\b|\bpfmax\b|\bpfmin\b|\bpfmul\b|\bpfnacc\b|\bpfpnacc\b|\bpfrcp\b|\bpfrcpit1\b|\bpfrcpit2\b|\bpfrcpv\b|\bpfrsqit1\b|\bpfrsqrt\b|\bpfrsqrtv\b|\bpfsub\b|\bpfsubr\b|\bphaddd\b|\bphaddsw\b|\bphaddw\b|\bphminposuw\b|\bphsubd\b|\bphsubsw\b|\bphsubw\b|\bpi2fd\b|\bpi2fw\b|\bpinsrb\b|\bpinsrd\b|\bpinsrq\b|\bpinsrw\b|\bpmaddubsw\b|\bpmaddwd\b|\bpmaxsb\b|\bpmaxsd\b|\bpmaxsw\b|\bpmaxub\b|\bpmaxud\b|\bpmaxuw\b|\bpminsb\b|\bpminsd\b|\bpminsw\b|\bpminub\b|\bpminud\b|\bpminuw\b|\bpmovmskb\b|\bpmovsxbd\b|\bpmovsxbq\b|\bpmovsxbw\b|\bpmovsxdq\b|\bpmovsxwd\b|\bpmovsxwq\b|\bpmovzxbd\b|\bpmovzxbq\b|\bpmovzxbw\b|\bpmovzxdq\b|\bpmovzxwd\b|\bpmovzxwq\b|\bpmuldq\b|\bpmulhrsw\b|\bpmulhrw\b|\bpmulhuw\b|\bpmulhw\b|\bpmulld\b|\bpmullw\b|\bpmuludq\b|\bpop\b|\bpopa\b|\bpopad\b|\bpopcnt\b|\bpopf\b|\bpopfd\b|\bpopfq\b|\bpor\b|\bprefetch\b|\bprefetchnta\b|\bprefetcht0\b|\bprefetcht1\b|\bprefetcht2\b|\bprefetchw\b|\bprefetchwt1\b|\bpsadbw\b|\bpshufb\b|\bpshufd\b|\bpshufhw\b|\bpshuflw\b|\bpshufw\b|\bpsignb\b|\bpsignd\b|\bpsignw\b|\bpslld\b|\bpslldq\b|\bpsllq\b|\bpsllw\b|\bpsrad\b|\bpsraw\b|\bpsrld\b|\bpsrldq\b|\bpsrlq\b|\bpsrlw\b|\bpsubb\b|\bpsubd\b|\bpsubq\b|\bpsubsb\b|\bpsubsw\b|\bpsubusb\b|\bpsubusw\b|\bpsubw\b|\bpswapd\b|\bptest\b|\bpunpckhbw\b|\bpunpckhdq\b|\bpunpckhqdq\b|\bpunpckhwd\b|\bpunpcklbw\b|\bpunpckldq\b|\bpunpcklqdq\b|\bpunpcklwd\b|\bpush\b|\bpusha\b|\bpushad\b|\bpushf\b|\bpushfd\b|\bpushfq\b|\bpxor\b|\brcl\b|\brcpps\b|\brcpss\b|\brcr\b|\brdfsbase\b|\brdgsbase\b|\brdrand\b|\brdseed\b|\brdtsc\b|\brdtscp\b|\bret\b|\brol\b|\bror\b|\brorx\b|\broundpd\b|\broundps\b|\broundsd\b|\broundss\b|\brsqrtps\b|\brsqrtss\b|\bsahf\b|\bsal\b|\bsar\b|\bsarx\b|\bsbb\b|\bscasb\b|\bscasd\b|\bscasq\b|\bscasw\b|\bseta\b|\bsetae\b|\bsetb\b|\bsetbe\b|\bsetc\b|\bsete\b|\bsetg\b|\bsetge\b|\bsetl\b|\bsetle\b|\bsetna\b|\bsetnae\b|\bsetnb\b|\bsetnbe\b|\bsetnc\b|\bsetne\b|\bsetng\b|\bsetnge\b|\bsetnl\b|\bsetnle\b|\bsetno\b|\bsetnp\b|\bsetns\b|\bsetnz\b|\bseto\b|\bsetp\b|\bsetpe\b|\bsetpo\b|\bsets\b|\bsetz\b|\bsfence\b|\bsha1msg1\b|\bsha1msg2\b|\bsha1nexte\b|\bsha1rnds4\b|\bsha256msg1\b|\bsha256msg2\b|\bsha256rnds2\b|\bshl\b|\bshld\b|\bshlx\b|\bshr\b|\bshrd\b|\bshrx\b|\bshufpd\b|\bshufps\b|\bsqrtpd\b|\bsqrtps\b|\bsqrtsd\b|\bsqrtss\b|\bstac\b|\bstc\b|\bstd\b|\bsti\b|\bstmxcsr\b|\bstosb\b|\bstosd\b|\bstosq\b|\bstosw\b|\bsub\b|\bsubpd\b|\bsubps\b|\bsubsd\b|\bsubss\b|\bswapgs\b|\bsyscall\b|\bsysenter\b|\bsysexit\b|\bsysexit64\b|\bsysret\b|\bsysret64\b|\bt1mskc\b|\btest\b|\btzcnt\b|\btzmsk\b|\bucomisd\b|\bucomiss\b|\bud2\b|\bunpckhpd\b|\bunpckhps\b|\bunpcklpd\b|\bunpcklps\b|\bvaddpd\b|\bvaddps\b|\bvaddsd\b|\bvaddss\b|\bvaddsubpd\b|\bvaddsubps\b|\bvaesdec\b|\bvaesdeclast\b|\bvaesenc\b|\bvaesenclast\b|\bvaesimc\b|\bvaeskeygenassist\b|\bvalignd\b|\bvalignq\b|\bvandnpd\b|\bvandnps\b|\bvandpd\b|\bvandps\b|\bvblendmb\b|\bvblendmd\b|\bvblendmpd\b|\bvblendmps\b|\bvblendmq\b|\bvblendmw\b|\bvblendpd\b|\bvblendps\b|\bvblendvpd\b|\bvblendvps\b|\bvbroadcastf128\b|\bvbroadcastf32x2\b|\bvbroadcastf32x4\b|\bvbroadcastf32x8\b|\bvbroadcastf64x2\b|\bvbroadcastf64x4\b|\bvbroadcasti128\b|\bvbroadcasti32x2\b|\bvbroadcasti32x4\b|\bvbroadcasti32x8\b|\bvbroadcasti64x2\b|\bvbroadcasti64x4\b|\bvbroadcastsd\b|\bvbroadcastss\b|\bvcmppd\b|\bvcmpps\b|\bvcmpsd\b|\bvcmpss\b|\bvcomisd\b|\bvcomiss\b|\bvcompresspd\b|\bvcompressps\b|\bvcvtdq2pd\b|\bvcvtdq2ps\b|\bvcvtpd2dq\b|\bvcvtpd2ps\b|\bvcvtpd2qq\b|\bvcvtpd2udq\b|\bvcvtpd2uqq\b|\bvcvtph2ps\b|\bvcvtps2dq\b|\bvcvtps2pd\b|\bvcvtps2ph\b|\bvcvtps2qq\b|\bvcvtps2udq\b|\bvcvtps2uqq\b|\bvcvtqq2pd\b|\bvcvtqq2ps\b|\bvcvtsd2si\b|\bvcvtsd2ss\b|\bvcvtsd2usi\b|\bvcvtsi2sd\b|\bvcvtsi2ss\b|\bvcvtss2sd\b|\bvcvtss2si\b|\bvcvtss2usi\b|\bvcvttpd2dq\b|\bvcvttpd2qq\b|\bvcvttpd2udq\b|\bvcvttpd2uqq\b|\bvcvttps2dq\b|\bvcvttps2qq\b|\bvcvttps2udq\b|\bvcvttps2uqq\b|\bvcvttsd2si\b|\bvcvttsd2usi\b|\bvcvttss2si\b|\bvcvttss2usi\b|\bvcvtudq2pd\b|\bvcvtudq2ps\b|\bvcvtuqq2pd\b|\bvcvtuqq2ps\b|\bvcvtusi2sd\b|\bvcvtusi2ss\b|\bvdbpsadbw\b|\bvdivpd\b|\bvdivps\b|\bvdivsd\b|\bvdivss\b|\bvdppd\b|\bvdpps\b|\bvexp2pd\b|\bvexp2ps\b|\bvexpandpd\b|\bvexpandps\b|\bvextractf128\b|\bvextractf32x4\b|\bvextractf32x8\b|\bvextractf64x2\b|\bvextractf64x4\b|\bvextracti128\b|\bvextracti32x4\b|\bvextracti32x8\b|\bvextracti64x2\b|\bvextracti64x4\b|\bvextractps\b|\bvfixupimmpd\b|\bvfixupimmps\b|\bvfixupimmsd\b|\bvfixupimmss\b|\bvfmadd132pd\b|\bvfmadd132ps\b|\bvfmadd132sd\b|\bvfmadd132ss\b|\bvfmadd213pd\b|\bvfmadd213ps\b|\bvfmadd213sd\b|\bvfmadd213ss\b|\bvfmadd231pd\b|\bvfmadd231ps\b|\bvfmadd231sd\b|\bvfmadd231ss\b|\bvfmaddpd\b|\bvfmaddps\b|\bvfmaddsd\b|\bvfmaddss\b|\bvfmaddsub132pd\b|\bvfmaddsub132ps\b|\bvfmaddsub213pd\b|\bvfmaddsub213ps\b|\bvfmaddsub231pd\b|\bvfmaddsub231ps\b|\bvfmaddsubpd\b|\bvfmaddsubps\b|\bvfmsub132pd\b|\bvfmsub132ps\b|\bvfmsub132sd\b|\bvfmsub132ss\b|\bvfmsub213pd\b|\bvfmsub213ps\b|\bvfmsub213sd\b|\bvfmsub213ss\b|\bvfmsub231pd\b|\bvfmsub231ps\b|\bvfmsub231sd\b|\bvfmsub231ss\b|\bvfmsubadd132pd\b|\bvfmsubadd132ps\b|\bvfmsubadd213pd\b|\bvfmsubadd213ps\b|\bvfmsubadd231pd\b|\bvfmsubadd231ps\b|\bvfmsubaddpd\b|\bvfmsubaddps\b|\bvfmsubpd\b|\bvfmsubps\b|\bvfmsubsd\b|\bvfmsubss\b|\bvfnmadd132pd\b|\bvfnmadd132ps\b|\bvfnmadd132sd\b|\bvfnmadd132ss\b|\bvfnmadd213pd\b|\bvfnmadd213ps\b|\bvfnmadd213sd\b|\bvfnmadd213ss\b|\bvfnmadd231pd\b|\bvfnmadd231ps\b|\bvfnmadd231sd\b|\bvfnmadd231ss\b|\bvfnmaddpd\b|\bvfnmaddps\b|\bvfnmaddsd\b|\bvfnmaddss\b|\bvfnmsub132pd\b|\bvfnmsub132ps\b|\bvfnmsub132sd\b|\bvfnmsub132ss\b|\bvfnmsub213pd\b|\bvfnmsub213ps\b|\bvfnmsub213sd\b|\bvfnmsub213ss\b|\bvfnmsub231pd\b|\bvfnmsub231ps\b|\bvfnmsub231sd\b|\bvfnmsub231ss\b|\bvfnmsubpd\b|\bvfnmsubps\b|\bvfnmsubsd\b|\bvfnmsubss\b|\bvfpclasspd\b|\bvfpclassps\b|\bvfpclasssd\b|\bvfpclassss\b|\bvfrczpd\b|\bvfrczps\b|\bvfrczsd\b|\bvfrczss\b|\bvgatherdpd\b|\bvgatherdps\b|\bvgatherpf0dpd\b|\bvgatherpf0dps\b|\bvgatherpf0qpd\b|\bvgatherpf0qps\b|\bvgatherpf1dpd\b|\bvgatherpf1dps\b|\bvgatherpf1qpd\b|\bvgatherpf1qps\b|\bvgatherqpd\b|\bvgatherqps\b|\bvgetexppd\b|\bvgetexpps\b|\bvgetexpsd\b|\bvgetexpss\b|\bvgetmantpd\b|\bvgetmantps\b|\bvgetmantsd\b|\bvgetmantss\b|\bvhaddpd\b|\bvhaddps\b|\bvhsubpd\b|\bvhsubps\b|\bvinsertf128\b|\bvinsertf32x4\b|\bvinsertf32x8\b|\bvinsertf64x2\b|\bvinsertf64x4\b|\bvinserti128\b|\bvinserti32x4\b|\bvinserti32x8\b|\bvinserti64x2\b|\bvinserti64x4\b|\bvinsertps\b|\bvlddqu\b|\bvldmxcsr\b|\bvmaskmovdqu\b|\bvmaskmovpd\b|\bvmaskmovps\b|\bvmaxpd\b|\bvmaxps\b|\bvmaxsd\b|\bvmaxss\b|\bvminpd\b|\bvminps\b|\bvminsd\b|\bvminss\b|\bvmovapd\b|\bvmovaps\b|\bvmovd\b|\bvmovddup\b|\bvmovdqa\b|\bvmovdqa32\b|\bvmovdqa64\b|\bvmovdqu\b|\bvmovdqu16\b|\bvmovdqu32\b|\bvmovdqu64\b|\bvmovdqu8\b|\bvmovhlps\b|\bvmovhpd\b|\bvmovhps\b|\bvmovlhps\b|\bvmovlpd\b|\bvmovlps\b|\bvmovmskpd\b|\bvmovmskps\b|\bvmovntdq\b|\bvmovntdqa\b|\bvmovntpd\b|\bvmovntps\b|\bvmovq\b|\bvmovsd\b|\bvmovshdup\b|\bvmovsldup\b|\bvmovss\b|\bvmovupd\b|\bvmovups\b|\bvmpsadbw\b|\bvmulpd\b|\bvmulps\b|\bvmulsd\b|\bvmulss\b|\bvorpd\b|\bvorps\b|\bvpabsb\b|\bvpabsd\b|\bvpabsq\b|\bvpabsw\b|\bvpackssdw\b|\bvpacksswb\b|\bvpackusdw\b|\bvpackuswb\b|\bvpaddb\b|\bvpaddd\b|\bvpaddq\b|\bvpaddsb\b|\bvpaddsw\b|\bvpaddusb\b|\bvpaddusw\b|\bvpaddw\b|\bvpalignr\b|\bvpand\b|\bvpandd\b|\bvpandn\b|\bvpandnd\b|\bvpandnq\b|\bvpandq\b|\bvpavgb\b|\bvpavgw\b|\bvpblendd\b|\bvpblendvb\b|\bvpblendw\b|\bvpbroadcastb\b|\bvpbroadcastd\b|\bvpbroadcastmb2d\b|\bvpbroadcastmb2q\b|\bvpbroadcastq\b|\bvpbroadcastw\b|\bvpclmulqdq\b|\bvpcmov\b|\bvpcmpb\b|\bvpcmpd\b|\bvpcmpeqb\b|\bvpcmpeqd\b|\bvpcmpeqq\b|\bvpcmpeqw\b|\bvpcmpestri\b|\bvpcmpestrm\b|\bvpcmpgtb\b|\bvpcmpgtd\b|\bvpcmpgtq\b|\bvpcmpgtw\b|\bvpcmpistri\b|\bvpcmpistrm\b|\bvpcmpq\b|\bvpcmpub\b|\bvpcmpud\b|\bvpcmpuq\b|\bvpcmpuw\b|\bvpcmpw\b|\bvpcomb\b|\bvpcomd\b|\bvpcompressd\b|\bvpcompressq\b|\bvpcomq\b|\bvpcomub\b|\bvpcomud\b|\bvpcomuq\b|\bvpcomuw\b|\bvpcomw\b|\bvpconflictd\b|\bvpconflictq\b|\bvperm2f128\b|\bvperm2i128\b|\bvpermb\b|\bvpermd\b|\bvpermi2b\b|\bvpermi2d\b|\bvpermi2pd\b|\bvpermi2ps\b|\bvpermi2q\b|\bvpermi2w\b|\bvpermil2pd\b|\bvpermil2ps\b|\bvpermilpd\b|\bvpermilps\b|\bvpermpd\b|\bvpermps\b|\bvpermq\b|\bvpermt2b\b|\bvpermt2d\b|\bvpermt2pd\b|\bvpermt2ps\b|\bvpermt2q\b|\bvpermt2w\b|\bvpermw\b|\bvpexpandd\b|\bvpexpandq\b|\bvpextrb\b|\bvpextrd\b|\bvpextrq\b|\bvpextrw\b|\bvpgatherdd\b|\bvpgatherdq\b|\bvpgatherqd\b|\bvpgatherqq\b|\bvphaddbd\b|\bvphaddbq\b|\bvphaddbw\b|\bvphaddd\b|\bvphadddq\b|\bvphaddsw\b|\bvphaddubd\b|\bvphaddubq\b|\bvphaddubw\b|\bvphaddudq\b|\bvphadduwd\b|\bvphadduwq\b|\bvphaddw\b|\bvphaddwd\b|\bvphaddwq\b|\bvphminposuw\b|\bvphsubbw\b|\bvphsubd\b|\bvphsubdq\b|\bvphsubsw\b|\bvphsubw\b|\bvphsubwd\b|\bvpinsrb\b|\bvpinsrd\b|\bvpinsrq\b|\bvpinsrw\b|\bvplzcntd\b|\bvplzcntq\b|\bvpmacsdd\b|\bvpmacsdqh\b|\bvpmacsdql\b|\bvpmacssdd\b|\bvpmacssdqh\b|\bvpmacssdql\b|\bvpmacsswd\b|\bvpmacssww\b|\bvpmacswd\b|\bvpmacsww\b|\bvpmadcsswd\b|\bvpmadcswd\b|\bvpmadd52huq\b|\bvpmadd52luq\b|\bvpmaddubsw\b|\bvpmaddwd\b|\bvpmaskmovd\b|\bvpmaskmovq\b|\bvpmaxsb\b|\bvpmaxsd\b|\bvpmaxsq\b|\bvpmaxsw\b|\bvpmaxub\b|\bvpmaxud\b|\bvpmaxuq\b|\bvpmaxuw\b|\bvpminsb\b|\bvpminsd\b|\bvpminsq\b|\bvpminsw\b|\bvpminub\b|\bvpminud\b|\bvpminuq\b|\bvpminuw\b|\bvpmovb2m\b|\bvpmovd2m\b|\bvpmovdb\b|\bvpmovdw\b|\bvpmovm2b\b|\bvpmovm2d\b|\bvpmovm2q\b|\bvpmovm2w\b|\bvpmovmskb\b|\bvpmovq2m\b|\bvpmovqb\b|\bvpmovqd\b|\bvpmovqw\b|\bvpmovsdb\b|\bvpmovsdw\b|\bvpmovsqb\b|\bvpmovsqd\b|\bvpmovsqw\b|\bvpmovswb\b|\bvpmovsxbd\b|\bvpmovsxbq\b|\bvpmovsxbw\b|\bvpmovsxdq\b|\bvpmovsxwd\b|\bvpmovsxwq\b|\bvpmovusdb\b|\bvpmovusdw\b|\bvpmovusqb\b|\bvpmovusqd\b|\bvpmovusqw\b|\bvpmovuswb\b|\bvpmovw2m\b|\bvpmovwb\b|\bvpmovzxbd\b|\bvpmovzxbq\b|\bvpmovzxbw\b|\bvpmovzxdq\b|\bvpmovzxwd\b|\bvpmovzxwq\b|\bvpmuldq\b|\bvpmulhrsw\b|\bvpmulhuw\b|\bvpmulhw\b|\bvpmulld\b|\bvpmullq\b|\bvpmullw\b|\bvpmultishiftqb\b|\bvpmuludq\b|\bvpor\b|\bvpord\b|\bvporq\b|\bvpperm\b|\bvprold\b|\bvprolq\b|\bvprolvd\b|\bvprolvq\b|\bvprord\b|\bvprorq\b|\bvprorvd\b|\bvprorvq\b|\bvprotb\b|\bvprotd\b|\bvprotq\b|\bvprotw\b|\bvpsadbw\b|\bvpscatterdd\b|\bvpscatterdq\b|\bvpscatterqd\b|\bvpscatterqq\b|\bvpshab\b|\bvpshad\b|\bvpshaq\b|\bvpshaw\b|\bvpshlb\b|\bvpshld\b|\bvpshlq\b|\bvpshlw\b|\bvpshufb\b|\bvpshufd\b|\bvpshufhw\b|\bvpshuflw\b|\bvpsignb\b|\bvpsignd\b|\bvpsignw\b|\bvpslld\b|\bvpslldq\b|\bvpsllq\b|\bvpsllvd\b|\bvpsllvq\b|\bvpsllvw\b|\bvpsllw\b|\bvpsrad\b|\bvpsraq\b|\bvpsravd\b|\bvpsravq\b|\bvpsravw\b|\bvpsraw\b|\bvpsrld\b|\bvpsrldq\b|\bvpsrlq\b|\bvpsrlvd\b|\bvpsrlvq\b|\bvpsrlvw\b|\bvpsrlw\b|\bvpsubb\b|\bvpsubd\b|\bvpsubq\b|\bvpsubsb\b|\bvpsubsw\b|\bvpsubusb\b|\bvpsubusw\b|\bvpsubw\b|\bvpternlogd\b|\bvpternlogq\b|\bvptest\b|\bvptestmb\b|\bvptestmd\b|\bvptestmq\b|\bvptestmw\b|\bvptestnmb\b|\bvptestnmd\b|\bvptestnmq\b|\bvptestnmw\b|\bvpunpckhbw\b|\bvpunpckhdq\b|\bvpunpckhqdq\b|\bvpunpckhwd\b|\bvpunpcklbw\b|\bvpunpckldq\b|\bvpunpcklqdq\b|\bvpunpcklwd\b|\bvpxor\b|\bvpxord\b|\bvpxorq\b|\bvrangepd\b|\bvrangeps\b|\bvrangesd\b|\bvrangess\b|\bvrcp14pd\b|\bvrcp14ps\b|\bvrcp14sd\b|\bvrcp14ss\b|\bvrcp28pd\b|\bvrcp28ps\b|\bvrcp28sd\b|\bvrcp28ss\b|\bvrcpps\b|\bvrcpss\b|\bvreducepd\b|\bvreduceps\b|\bvreducesd\b|\bvreducess\b|\bvrndscalepd\b|\bvrndscaleps\b|\bvrndscalesd\b|\bvrndscaless\b|\bvroundpd\b|\bvroundps\b|\bvroundsd\b|\bvroundss\b|\bvrsqrt14pd\b|\bvrsqrt14ps\b|\bvrsqrt14sd\b|\bvrsqrt14ss\b|\bvrsqrt28pd\b|\bvrsqrt28ps\b|\bvrsqrt28sd\b|\bvrsqrt28ss\b|\bvrsqrtps\b|\bvrsqrtss\b|\bvscalefpd\b|\bvscalefps\b|\bvscalefsd\b|\bvscalefss\b|\bvscatterdpd\b|\bvscatterdps\b|\bvscatterpf0dpd\b|\bvscatterpf0dps\b|\bvscatterpf0qpd\b|\bvscatterpf0qps\b|\bvscatterpf1dpd\b|\bvscatterpf1dps\b|\bvscatterpf1qpd\b|\bvscatterpf1qps\b|\bvscatterqpd\b|\bvscatterqps\b|\bvshuff32x4\b|\bvshuff64x2\b|\bvshufi32x4\b|\bvshufi64x2\b|\bvshufpd\b|\bvshufps\b|\bvsqrtpd\b|\bvsqrtps\b|\bvsqrtsd\b|\bvsqrtss\b|\bvstmxcsr\b|\bvsubpd\b|\bvsubps\b|\bvsubsd\b|\bvsubss\b|\bvtestpd\b|\bvtestps\b|\bvucomisd\b|\bvucomiss\b|\bvunpckhpd\b|\bvunpckhps\b|\bvunpcklpd\b|\bvunpcklps\b|\bvxorpd\b|\bvxorps\b|\bvzeroall\b|\bvzeroupper\b|\bwait\b|\bwrfsbase\b|\bwrgsbase\b|\bxadd\b|\bxchg\b|\bxgetbv\b|\bxor\b|\bxorpd\b|\bxorps\b|\bxrstor\b|\bxrstor64\b|\bxrstors\b|\bxrstors64\b|\bxsave\b|\bxsave64\b|\bxsavec\b|\bxsavec64\b|\bxsaveopt\b|\bxsaveopt64\b|\bxsaves\b|\bxsaves64\b|\bxsetbv\b|\bhlt\b|\bmovabs\b|\bendbr32\b|\bendbr64\b",
);

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
const INDEX_REGISTER = /[er]?[ds]i|[ds]il/;
const STACK_REGISTER = /[er][sb]p/;
const AMD_REGISTERS = /r(8|9|10|11|12|13|14|15)[bwd]?/;
const SPECIAL_REGISTERS = /cr0|cr2|cr3|dr[0-3]|dr[67]|tr[3-7]/;
const SIMD_REGISTER = /[xyz]mm(0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15)/;
const SEGMENT_REGISTER = /[cdefgs]s/;

const CONTEXT_ITEM = /assumes|radix|listing|cpu|all/;
const DATA_TYPE =
  /byte|sbyte|word|sword|dword|sdword|fword|qword|sqword|tbyte|oword|real4|real8|real10|mmword|xmmword|ymmword/;

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
const LIST_OPTION =
  /.list|.nolist|.xlist|.listall|.listif|.lfcond|.nolistif|.sfcond|.tfcond|.listmacroall|.lall|.nolistmacro|.sall|.listmacro|.xall/;
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

const list = (listItem) => seq(listItem, repeat(seq(",", listItem)));
const listWithEol = (listItem, eol) =>
  seq(listItem, repeat(seq(",", optional(eol), listItem)));
const tokenFromRegex = (regex) => token(regex);

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

  extras: ($) => [/[ \t]+/, $.comment_line],

  word: ($) => $.identifier,

  conflicts: ($) => [
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
    source_file: ($) => $.module,

    module: ($) => seq($.directive_list, optional($.end_dir)),
    end_dir: ($) => seq("end", optional($.expression), $._eol),

    _eol: ($) => choice($.comment_line, /\n+/),
    comment_line: (_) => /;.*\n+/,

    _asm_instruction: ($) =>
      seq(
        field("mnemonic", token(prec(1, MNEMONIC))),
        field("args", optional($.expr_list)),
      ),
    instruction: ($) =>
      seq(optional($.instr_prefix), $._asm_instruction, $._eol), // official grammar error (possible): I belive there should be an eol (;;) here

    identifier: (_) => /[a-zA-Z@_$?][a-zA-Z0-9@_$?]*/,

    // tree-sitter is weird about regex vs. string keywords, the `word` token, and lexical precedence
    // const BYTE_REGISTER = /[abcd][lh]/;
    // const GP_REGISTER = /[er]?[abcd]x/;
    // const INDEX_REGISTER = /[er]?[ds]i|[ds]il/;
    // const STACK_REGISTER = /[er][sb]p/;
    // const AMD_REGISTERS = /r(8|9|10|11|12|13|14|15)[bwd]?/;
    // const SPECIAL_REGISTERS = /cr0|cr2|cr3|dr[0-3]|dr[67]|tr[3-7]/;
    // const SIMD_REGISTER = /[xyz]mm(0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15)/;
    // const SEGMENT_REGISTER = /[cdefgs]s/;

    register: ($) =>
      choice(
        "al",
        "ah",
        "ax",
        "eax",
        "rax",
        "bl",
        "bh",
        "bx",
        "ebx",
        "rbx",
        "cl",
        "ch",
        "cx",
        "ecx",
        "rcx",
        "dl",
        "dh",
        "dx",
        "edx",
        "rdx",
        "sil",
        "si",
        "esi",
        "rsi",
        "dil",
        "di",
        "edi",
        "rdi",
        "ebp",
        "rbp",
        "esp",
        "rsp",
        "r8b",
        "r9b",
        "r10b",
        "r11b",
        "r12b",
        "r13b",
        "r14b",
        "r15b",
        "r8w",
        "r9w",
        "r10w",
        "r11w",
        "r12w",
        "r13w",
        "r14w",
        "r15w",
        "r8d",
        "r9d",
        "r10d",
        "r11d",
        "r12d",
        "r13d",
        "r14d",
        "r15d",
        "r8",
        "r9",
        "r10",
        "r11",
        "r12",
        "r13",
        "r14",
        "r15",
        "cr0",
        "cr2",
        "cr3",
        "dr0",
        "dr1",
        "dr2",
        "dr3",
        "dr6",
        "dr7",
        "tr3",
        "tr4",
        "tr5",
        "tr6",
        "tr7",
        "xmm0",
        "xmm1",
        "xmm2",
        "xmm3",
        "xmm4",
        "xmm5",
        "xmm6",
        "xmm7",
        "xmm8",
        "xmm9",
        "xmm10",
        "xmm11",
        "xmm12",
        "xmm13",
        "xmm14",
        "xmm15",
        "ymm0",
        "ymm1",
        "ymm2",
        "ymm3",
        "ymm4",
        "ymm5",
        "ymm6",
        "ymm7",
        "ymm8",
        "ymm9",
        "ymm10",
        "ymm11",
        "ymm12",
        "ymm13",
        "ymm14",
        "ymm15",
        "zmm0",
        "zmm1",
        "zmm2",
        "zmm3",
        "zmm4",
        "zmm5",
        "zmm6",
        "zmm7",
        "zmm8",
        "zmm9",
        "zmm10",
        "zmm11",
        "zmm12",
        "zmm13",
        "zmm14",
        "zmm15",
        "cs",
        "ds",
        "es",
        "fs",
        "gs",
        "ss",
        seq("st", $.expression),
      ),

    // register: ($) =>
    //   choice(
    //     tokenFromRegex(BYTE_REGISTER),
    //     tokenFromRegex(GP_REGISTER),
    //     tokenFromRegex(INDEX_REGISTER),
    //     tokenFromRegex(STACK_REGISTER),
    //     tokenFromRegex(AMD_REGISTERS),
    //     tokenFromRegex(SPECIAL_REGISTERS),
    //     tokenFromRegex(SIMD_REGISTER),
    //     tokenFromRegex(SEGMENT_REGISTER),
    //     seq("st", $.expression),
    //   ),
    reg_list: ($) => repeat1($.register),

    // building blocks

    id_list: ($) => list($.identifier),
    stext: ($) => repeat1(ANY_CHAR_EXCEPT_QUOTE),
    string: ($) => seq($.quote, optional($.stext), $.quote),
    text_literal: ($) => seq("<", TEXT, ">", $._eol),
    exponent: ($) => seq("e", optional($.sign), DEC_NUMBER),
    float_number: ($) =>
      choice(
        seq(
          optional($.sign),
          DEC_NUMBER,
          ".",
          optional(DEC_NUMBER),
          optional($.exponent),
        ),
        seq(DIGITS, "r"),
      ),
    bcd_const: ($) => seq(optional($.sign), DEC_NUMBER),

    // expressions

    expression: ($) =>
      choice(
        $.binary_expression,
        $.prefix_expression,
        prec(PREC.e11, $.expression_terminal),
      ),

    prefix_expression: ($) => {
      const table = [
        [PREC.bitwise_not, "not"],
        [PREC.bit_section, choice("high", "low", "highword", "lowword")],
        [PREC.offset, choice("offset", "seg", "lroffset", "type", "this")],
      ];

      return choice(
        ...table.map(([precedence, prefix]) =>
          prec(
            precedence,
            seq(field("prefix", prefix), field("right", $.expression)),
          ),
        ),
      );
    },

    binary_expression: ($) => {
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

      return choice(
        ...table.map(([precedence, operator]) =>
          prec.left(
            precedence,
            seq(
              field("left", $.expression),
              field("operator", operator),
              field("right", $.expression),
            ),
          ),
        ),
      );
    },

    expression_terminal: ($) =>
      choice(
        seq("(", $.expression, ")"),
        seq("[", $.expression, "]"),
        seq("width", $.identifier),
        seq("mask", $.identifier),
        seq("size", $.expression),
        seq("sizeof", $.expression),
        seq("length", $.identifier),
        seq("lengthof", $.identifier),
        CONSTANT,
        $.string,
        $.type,
        "@f", // official grammar error: missing
        "@b", // official grammar error: missing
        "$",
        prec(4, seq("st", "(", $.expression, ")")),
        prec(3, "st"),
        $.register,
        prec(1, $.identifier),
      ),

    // uses expressions

    expr_list: ($) => list($.expression),
    _bit_field_size: ($) => alias($.expression, $.bit_field_size),
    bit_def: ($) =>
      seq(
        $.identifier,
        ":",
        $._bit_field_size,
        optional(seq("=", $.expression)),
      ),
    bit_def_list: ($) => listWithEol($.bit_def, $._eol),
    record_dir: ($) => seq($.identifier, "record", $.bit_def_list, $._eol),

    _comm_type: ($) => alias($.expression, $.comm_type),
    comm_decl: ($) =>
      seq(
        optional($.near_far),
        optional($.lang_type),
        $.identifier,
        ":",
        $._comm_type,
        optional(seq(":", $.expression)),
      ),
    comm_list: ($) => list($.comm_decl),
    comm_dir: ($) => seq("comm", $.comm_list, $._eol),

    init_value: ($) =>
      choice(
        // $.string, // handled by expression
        "?",
        seq($.expression, optional(seq("dup", "(", $.scalar_inst_list, ")"))),
        $.float_number,
        $.bcd_const,
      ),
    scalar_inst_list: ($) => listWithEol($.init_value, $._eol),

    field_init: ($) => choice($.init_value, $.struct_instance),
    field_init_list: ($) => listWithEol($.field_init, $._eol),

    record_field_list: ($) => listWithEol($.expression, $._eol),
    old_record_field_list: ($) => list($.expression),
    record_instance: ($) =>
      choice(
        seq("{", optional($._eol), $.record_field_list, optional($._eol), "}"),
        seq("<", $.old_record_field_list, ">"),
        seq($.expression, "dup", "(", $.record_instance, ")"),
      ),
    record_inst_list: ($) => listWithEol($.record_instance, $._eol),
    record_const: ($) =>
      choice(
        seq($.identifier, "{", $.old_record_field_list, "}"),
        seq($.identifier, "<", $.old_record_field_list, ">"),
      ),

    data_item: ($) =>
      choice(
        prec(
          1,
          seq(field("type", $._data_decl), field("value", $.scalar_inst_list)),
        ),
        prec(
          2,
          seq(field("type", $.identifier), field("value", $.struct_inst_list)),
        ),
        prec(
          3,
          seq(field("type", $.identifier), field("value", $.record_inst_list)),
        ),
      ),
    data_dir: ($) => seq(optional($.identifier), $.data_item, $._eol),

    _seg_dir: ($) =>
      choice(
        seq(".code", optional($.identifier)),
        ".data",
        ".data?",
        ".const",
        seq(".fardata", optional($.identifier)),
        seq(".fardata?", optional($.identifier)),
        seq(".stack", optional($.expression)),
      ),
    _simple_seg_dir: ($) => seq($._seg_dir, $._eol),

    text_item: ($) =>
      choice($.text_literal, $.identifier, seq("%", $.expression)),
    text_list: ($) => listWithEol($.text_item, $._eol),
    _text_len: ($) => alias($.expression, $.text_len),
    _text_start: ($) => alias($.expression, $.text_start),
    text_macro_dir: ($) =>
      choice(
        seq("catstr", optional($.text_list)),
        seq("textequ", optional($.text_list)),
        seq("sizestr", $.text_item),
        seq(
          "substr",
          $.text_item,
          ",",
          $._text_start,
          optional(seq(",", $._text_len)),
        ),
        seq(
          "instr",
          optional(seq($._text_start, ",")),
          $.text_item,
          ",",
          $.text_item,
        ),
      ),
    text_dir: ($) => seq($.identifier, $.text_macro_dir, $._eol),

    // official grammar error: missing a | to indicate choice
    until_dir: ($) =>
      choice(
        seq(".until", $.expression, $._eol),
        seq(".untilcxz", optional($.expression), $._eol),
      ),

    offset_dir_type: ($) =>
      choice(
        "even",
        seq("org", $.expression),
        seq("align", optional($.expression)),
      ),
    offset_dir: ($) => seq($.offset_dir_type, $._eol),

    struct_instance: ($) =>
      choice(
        seq("<", optional($.field_init_list), ">"),
        seq(
          "{",
          optional($._eol),
          optional($.field_init_list),
          optional($._eol),
          "}",
        ),
        seq($.expression, "dup", "(", $.struct_inst_list, ")"),
      ),
    struct_inst_list: ($) => listWithEol($.struct_instance, $._eol),
    struct_item: ($) =>
      choice($.data_dir, $._general_dir, $.offset_dir, $.nested_struct),
    nested_struct: ($) =>
      seq(
        $.struct_hdr,
        optional($.identifier),
        $._eol,
        $.struct_body,
        "ends",
        $._eol,
      ),
    struct_body: ($) => repeat1($.struct_item),
    _field_align: ($) => alias($.expression, $.field_align),
    struct_dir: ($) =>
      seq(
        $.identifier,
        $.struct_hdr,
        optional($._field_align),
        optional(seq(",", "nonunique")),
        $._eol,
        $.struct_body,
        $.identifier,
        "ends",
        $._eol,
      ),

    macro_arg: ($) =>
      choice(
        seq("%", $.expression),
        seq("%", $.identifier),
        seq("%", $.identifier, "(", $.macro_arg_list, ")"),
        $.string,
        SYNTACTICAL_TEXT,
        seq("<", SYNTACTICAL_TEXT, ">"),
      ),
    macro_arg_list: ($) => list($.macro_arg),

    // official grammar error: having this just be `directive` does not allow for instructions. inSegDirList does. see `macro_body`.
    macro_stmt: ($) =>
      choice(
        // $._directive,
        $.exitm_dir,
        seq(":", $.identifier),
        seq("goto", $.identifier),
      ),
    macro_stmt_list: ($) => repeat1(seq($.macro_stmt, $._eol)),
    macro_body: ($) =>
      seq(optional($.local_list), choice($.macro_stmt_list, $.in_seg_dir_list)),
    macro_dir: ($) =>
      seq(
        $.identifier,
        "macro",
        optional($.macro_parm_list),
        $._eol,
        $.macro_body,
        "endm",
        $._eol,
      ),
    macro_for: ($) =>
      seq(
        $.for_dir,
        $.for_parm,
        ",",
        "<",
        $.macro_arg_list,
        ">",
        $._eol,
        optional($.macro_body),
        "endm",
        $._eol,
      ),
    macro_forc: ($) =>
      seq(
        FORC_DIR,
        $.identifier,
        ",",
        $.text_literal,
        $._eol,
        $.macro_body,
        "endm",
        $._eol,
      ),
    macro_repeat: ($) =>
      seq($.repeat_dir, $.expression, $._eol, $.macro_body, "endm", $._eol),
    macro_while: ($) =>
      seq("while", $.expression, $._eol, $.macro_body, "endm", $._eol),
    macro_call: ($) =>
      choice(
        seq($.identifier, optional($.macro_arg_list), $._eol),
        seq($.identifier, "(", optional($.macro_arg_list), ")"),
      ),

    eq_dir: ($) => seq($.identifier, "=", $.expression, $._eol),
    equ_type: ($) => choice($.expression, $.text_literal),
    equ_dir: ($) => seq($.identifier, "equ", $.equ_type, $._eol),

    radix_dir: ($) => seq(".radix", $.expression, $._eol),

    title_dir: ($) => seq($.title_type, ARBITRARY_TEXT, $._eol),

    page_expr: ($) =>
      choice("+", seq($.expression, optional(seq(",", $.expression)))),
    page_dir: ($) => seq("page", optional($.page_expr), $._eol),

    // official grammar error: this isn't even listed
    alias_dir: ($) => seq("alias", $.text_literal, "=", $.text_literal),

    _general_dir: ($) =>
      choice(
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
        // $.macro_for,
        // $.macro_forc,
        // $.macro_repeat,
        // $.macro_while,
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
    _directive: ($) => choice($._general_dir),
    directive_list: ($) => repeat1($._directive),

    if_statement: ($) =>
      choice(
        seq("if", $.expression),
        seq("ife", $.expression),
        seq("ifb", $.text_item),
        seq("ifnb", $.text_item),
        seq("ifdef", $.identifier),
        seq("ifndef", $.identifier),
        seq("ifdif", $.text_item, $.text_item),
        seq("ifdifi", $.text_item, $.text_item),
        seq("ifidn", $.text_item, $.text_item),
        seq("ifidni", $.text_item, $.text_item),
        "if1",
        "if2",
      ),
    if_dir: ($) =>
      seq(
        $.if_statement,
        $._eol,
        $.directive_list,
        repeat($.elseif_block),
        optional(seq("else", $._eol, $.directive_list)),
        "endif",
        $._eol, // official grammar error: "endif" is nowhere to be found
      ),
    elseif_statement: ($) =>
      choice(
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
    elseif_block: ($) => seq($.elseif_statement, $._eol, $.directive_list),
    opt_text: ($) => seq(",", $.text_item),
    error_opt: ($) =>
      choice(
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
    error_dir: ($) => seq($.error_opt, $._eol),

    // idk

    cref_option: ($) =>
      choice(
        ".cref",
        seq(".xcref", optional($.id_list)),
        seq(".nocref", optional($.id_list)),
      ),
    cref_dir: ($) => seq($.cref_option, $._eol),
    _data_decl: ($) =>
      choice("db", "dw", "dd", "df", "dq", "dt", $._data_type, $.identifier),
    distance: ($) => choice($.near_far, "near16", "near32", "far16", "far32"),
    type: ($) => choice($.identifier, $.distance, $._data_type),
    qualified_type: ($) =>
      choice(
        $.type,
        seq(optional($.distance), "ptr", optional($.qualified_type)),
      ),

    proto_arg: ($) => seq(optional($.identifier), ":", $.qualified_type),
    proto_list: ($) => listWithEol($.proto_arg, $._eol),
    proto_arg_list: ($) =>
      seq(
        optional(seq(",", optional($._eol))),
        choice(
          seq(
            $.proto_list,
            optional(
              seq(",", optional($._eol), optional($.identifier), ":vararg"),
            ),
          ),
          seq(optional($.identifier), ":vararg"),
        ),
      ),
    proto_spec: ($) =>
      choice(
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
    proto_type_dir: ($) =>
      seq($.identifier, "proto", optional($.proto_spec), $._eol), // official grammar error (possibly): I believe there should be an eol (;;) here

    pub_def: ($) => seq(optional($.lang_type), $.identifier),
    pub_list: ($) => listWithEol($.pub_def, $._eol),
    public_dir: ($) => seq("public", $.pub_list, $._eol),

    purge_dir: ($) => seq("purge", $.id_list),

    parm_type: ($) => choice("req", seq("=", $.text_literal), "vararg"),
    macro_parm: ($) => seq($.identifier, optional(seq(":", $.parm_type))),
    macro_parm_list: ($) => listWithEol($.macro_parm, $._eol),
    parm: ($) =>
      choice(
        seq($.identifier, optional(seq(":", $.qualified_type))),
        seq(
          $.identifier,
          optional($.expression),
          optional(seq(":", $.qualified_type)),
        ),
      ),
    parm_list: ($) => listWithEol($.parm, $._eol),

    model_opt: ($) => choice($.lang_type, $.stack_option),
    model_opt_list: ($) => list($.model_opt),
    model_dir: ($) =>
      seq(".model", $.mem_option, optional(seq(",", $.model_opt_list)), $._eol),

    name_dir: ($) => seq("name", $.identifier, $._eol),

    p_options: ($) =>
      choice(
        seq($.distance, optional($.lang_type), optional($.o_visibility)),
        seq($.lang_type, optional($.o_visibility)),
        $.o_visibility,
      ),

    for_parm_type: ($) => choice("req", seq("=", $.text_literal)),
    for_parm: ($) => seq($.identifier, optional(seq(":", $.for_parm_type))),

    frame_expr: ($) =>
      choice(
        seq("seg", $.identifier),
        seq("dgroup", ":", $.identifier),
        seq(SEGMENT_REGISTER, ":", $.identifier),
        $.identifier,
      ),

    seg_id_list: ($) => list($.identifier),
    group_dir: ($) => seq($.identifier, "group", $.seg_id_list),

    file_spec: ($) => choice(FILE_CHAR_LIST, $.text_literal),
    include_dir: ($) => seq("include", $.file_spec, $._eol),
    include_lib_dir: ($) => seq("includelib", $.file_spec, $._eol),

    label_def: ($) =>
      choice(seq($.identifier, ":"), seq($.identifier, "::"), "@@:"),
    label_dir: ($) => seq($.identifier, "label", $.qualified_type, $._eol),

    qualifier: ($) =>
      choice($.qualified_type, seq("proto", optional($.proto_spec))),
    typedef_dir: ($) => seq($.identifier, "typedef", $.qualifier, $._eol), // official grammar error (possibly): I believe there should be an eol (;;) here

    // NOTE: I think the bnf grammar might be incorrect. There's no way to get from "extern" to "proto" in the grammar.
    // If instead of `qualified_type`, it was `qualifier`, then it would make sense.
    extern_type: ($) => choice("abs", $.qualifier),
    extern_def: ($) =>
      seq(
        optional($.lang_type),
        $.identifier,
        optional(seq("(", $.identifier, ")")),
        ":",
        $.extern_type,
      ),
    extern_list: ($) => listWithEol($.extern_def, $._eol),
    extern_dir: ($) => seq($.extern_key, $.extern_list, $._eol),

    assume_val: ($) => choice($.qualified_type, "nothing", "error"),
    assume_seg_val: ($) => choice($.frame_expr, "nothing", "error"),
    assume_seg_reg: ($) =>
      prec(1, seq(SEGMENT_REGISTER, ":", $.assume_seg_val)),
    assume_register: ($) => choice($.assume_seg_reg, $.assume_reg),
    assume_reg: ($) => seq($.register, ":", $.assume_val),
    assume_list: ($) => list($.assume_register),
    assume_dir: ($) =>
      choice(
        seq("assume", $.assume_list, $._eol),
        seq("assume nothing", $._eol),
      ),

    // official grammar error: it omits an `|` indicating a choice between "echo" and "%out"
    echo_dir: ($) =>
      choice(
        seq("echo", ARBITRARY_TEXT, $._eol),
        seq("%out", ARBITRARY_TEXT, $._eol),
      ),

    list_dir: ($) => seq($.list_option, $._eol),

    local_def: ($) => seq("local", $.id_list, $._eol),
    local_list: ($) => repeat1($.local_def),
    local_dir: ($) => seq("local", $.parm_list, $._eol),
    local_dir_list: ($) => repeat1($.local_dir),

    seg_attrib: ($) =>
      choice(
        "public",
        "stack",
        "common",
        "memory",
        seq("at", $.expression),
        "private",
      ),
    seg_option: ($) =>
      choice($.seg_align, $.seg_ro, $.seg_attrib, $.seg_size, $._class_name),
    seg_option_list: ($) => repeat1($.seg_option),
    segment_dir: ($) =>
      seq($.identifier, "segment", optional($.seg_option_list), $._eol),
    segment_def: ($) =>
      choice(
        seq($.segment_dir, optional($.in_seg_dir_list), $.ends_dir),
        seq(
          $._simple_seg_dir,
          optional($.in_seg_dir_list),
          optional($.ends_dir),
        ),
      ),

    context_dir: ($) =>
      choice(
        seq("pushcontext", $.context_item_list, $._eol),
        seq("popcontext", $.context_item_list, $._eol),
      ),

    endp_dir: ($) => seq($.identifier, "endp", $._eol),
    ends_dir: ($) => seq($.identifier, "ends", $._eol),
    exitm_dir: ($) => choice(seq(":", "exitm"), seq("exitm", $.text_item)),

    special_chars: (_) =>
      /:|\.|\[|\]|\(|\)|<|>|\{|\}|\+|-|\/|\*|&|%|!|'|\\|=|;|,|"|\s|\n/,

    startup_dir: ($) => seq(".startup", $._eol),

    uses_regs: ($) => seq("uses", $.reg_list),

    exit_dir: ($) => seq(".exit", $.expression, $._eol),

    invoke_arg: ($) =>
      choice(
        seq($.register, "::", $.register),
        $.expression,
        seq("addr", $.expression),
      ),
    invoke_list: ($) => listWithEol($.invoke_arg, $._eol),
    invoke_dir: ($) =>
      seq(
        "invoke",
        $.expression,
        optional(seq(",", optional($._eol), $.invoke_list)),
        $._eol,
      ),

    _in_segment_dir: ($) =>
      choice(
        $.instruction,
        $.data_dir,
        $.startup_dir,
        $.exit_dir,
        $.offset_dir,
        $.label_dir,
        $.invoke_dir,
        // $._general_dir,
        $.control_dir,
        seq(
          $.proc_dir,
          optional($.local_dir_list),
          optional($.in_seg_dir_list),
          $.endp_dir,
        ),
      ),
    // official grammar error: it is possible to have a label with no inSegmentDir
    _in_seg_dir: ($) =>
      choice(
        seq($.label_def, $._eol),
        seq($.label_def, $._in_segment_dir),
        seq($._in_segment_dir),
      ),
    in_seg_dir_list: ($) => repeat1($._in_seg_dir),

    block_statements: ($) =>
      choice(
        $.directive_list,
        seq(".continue", optional(seq(".if", $.expression))),
        seq(".break", optional(seq(".if", $.expression))),
      ),
    control_if: ($) =>
      seq(
        ".if",
        $.expression,
        $._eol,
        $.directive_list,
        repeat($.control_elseif),
        optional(seq(".else", $._eol, $.directive_list)),
        ".endif",
        $._eol,
      ),
    control_elseif: ($) =>
      seq(".elseif", $.expression, $._eol, $.directive_list),
    while_block: ($) =>
      seq(".while", $.expression, $._eol, $.block_statements, $._eol, ".endw"),
    repeat_block: ($) =>
      seq(".repeat", $._eol, $.block_statements, $._eol, $.until_dir, $._eol),
    control_block: ($) => choice($.while_block, $.repeat_block),
    control_dir: ($) => choice($.control_if, $.control_block),

    proc_parm_list: ($) =>
      seq(
        optional(seq(",", optional($._eol))),
        choice(
          seq(
            $.parm_list,
            optional(seq(",", optional($._eol), $.identifier, ":vararg")),
          ),
          seq($.identifier, ":vararg"),
        ),
      ),
    proc_dir: ($) =>
      seq(
        $.identifier,
        "proc",
        optional($.p_options),
        optional(seq("<", $.macro_arg_list, ">")),
        optional($.uses_regs),
        optional($.proc_parm_list),
        $._eol,
      ),

    // option

    option_item: ($) =>
      choice(
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
    option_list: ($) => listWithEol($.option_item, $._eol),
    option_dir: ($) => seq("option", $.option_list, $._eol),

    // aliases

    _class_name: ($) => alias($.string, $.class_name),

    // terminals

    context_item: (_) => choice("assumes", "radix", "listing", "cpu", "all"),
    context_item_list: ($) => list($.context_item),
    _data_type: (_) =>
      choice(
        "byte",
        "sbyte",
        "word",
        "sword",
        "dword",
        "sdword",
        "fword",
        "qword",
        "sqword",
        "tbyte",
        "oword",
        "real4",
        "real8",
        "real10",
        "mmword",
        "xmmword",
        "ymmword",
      ),

    sign: (_) => choice("+", "-"),
    binary_op: (_) => choice("==", "!=", ">=", "<=", ">", "<", "&"),
    add_op: (_) => choice("+", "-"),
    or_op: (_) => choice("or", "xor"),
    mul_op: (_) => choice("*", "/", "mod"),
    rel_op: (_) => choice("eq", "ne", "lt", "le", "gt", "ge"),
    shift_op: (_) => choice("shr", "shl"),
    quote: (_) => choice(`"`, "'"),
    bool: (_) => choice("true", "false"),

    processor: (_) =>
      choice(
        ".386",
        ".386p",
        ".486",
        ".486P",
        ".586",
        ".586P",
        ".686",
        ".686P",
      ),
    coprocessor: (_) => choice(".8087", ".287", ".387", ".NO87"),
    processor_dir: ($) =>
      choice(
        prec(2, seq($.processor, $._eol)),
        prec(1, seq($.coprocessor, $._eol)),
      ),

    struct_hdr: (_) => choice("struc", "struct", "union"),

    stack_option: (_) => choice("nearstack", "farstack"),
    offset_type: (_) => choice("group", "segment", "flat"),
    extern_key: (_) => choice("extrn", "extern", "externdef"),
    repeat_dir: (_) => choice("repeat", "rept"),
    for_dir: (_) => choice("for", "irp"),
    forc_dir: (_) => choice("forc", "irpc"),
    instr_prefix: (_) =>
      choice("rep", "repe", "repz", "repne", "repnz", "lock"),
    list_option: (_) =>
      choice(
        ".list",
        ".nolist",
        ".xlist",
        ".listall",
        ".listif",
        ".lfcond",
        ".nolistif",
        ".sfcond",
        ".tfcond",
        ".listmacroall",
        ".lall",
        ".nolistmacro",
        ".sall",
        ".listmacro",
        ".xall",
      ),
    mem_option: (_) =>
      choice("tiny", "small", "medium", "compact", "large", "huge", "flat"),
    near_far: (_) => choice("near", "far"),
    o_visibility: (_) => choice("public", "private", "export"),
    seg_align: (_) => choice("byte", "word", "dword", "para", "page"),
    seg_order_dir: (_) => choice(".alpha", ".seq", ".dosseg", "dosseg"),
    seg_ro: (_) => "readonly",
    seg_size: (_) => choice("use16", "use32", "flat"),
    title_type: (_) => choice("title", "subtitle", "subttl"),
    lang_type: (_) =>
      choice("c", "pascal", "fortran", "basic", "syscall", "stdcall"),
    map_type: (_) => choice("all", "none", "notpublic"),
    flag_name: (_) =>
      choice("zero?", "carry?", "overflow?", "sign?", "parity?"),

    // TODO: MASM's COMMENT directive is gross and probably requires an external scanner to properly parse it
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq(TEXT, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, TEXT, $._eol),
    // comment_dir: $ => seq("comment", DELIMITER, "\n", repeat(seq(TEXT, "\n")), repeat(NON_WHITESPACE_CHARACTER), DELIMITER, TEXT, $._eol),
    // comment_dir: _ => /comment\s+(\S)\n([^\n]*\n)*$1/,
  },
});
