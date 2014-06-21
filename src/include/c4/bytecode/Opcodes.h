//-*- C++ -*-
#ifndef __C4_BYTECODE_OPCODES_H__
#define __C4_BYTECODE_OPCODES_H__

#include <string>
#include <unordered_map>

namespace c4 {
/*
// Constants
00 (0x00) nop
01 (0x01) aconst_null
02 (0x02) iconst_m1
03 (0x03) iconst_0
04 (0x04) iconst_1
05 (0x05) iconst_2
06 (0x06) iconst_3
07 (0x07) iconst_4
08 (0x08) iconst_5
09 (0x09) lconst_0
10 (0x0a) lconst_1
11 (0x0b) fconst_0
12 (0x0c) fconst_1
13 (0x0d) fconst_2
14 (0x0e) dconst_0
15 (0x0f) dconst_1
16 (0x10) bipush
17 (0x11) sipush
18 (0x12) ldc
19 (0x13) ldc_w
20 (0x14) ldc2_w

// Loads
21 (0x15) iload
22 (0x16) lload
23 (0x17) fload
24 (0x18) dload
25 (0x19) aload
26 (0x1a) iload_0
27 (0x1b) iload_1
28 (0x1c) iload_2
29 (0x1d) iload_3
30 (0x1e) lload_0
31 (0x1f) lload_1
32 (0x20) lload_2
33 (0x21) lload_3
34 (0x22) fload_0
35 (0x23) fload_1
36 (0x24) fload_2
37 (0x25) fload_3
38 (0x26) dload_0
39 (0x27) dload_1
40 (0x28) dload_2
41 (0x29) dload_3
42 (0x2a) aload_0
43 (0x2b) aload_1
44 (0x2c) aload_2
45 (0x2d) aload_3
46 (0x2e) iaload
47 (0x2f) laload
48 (0x30) faload
49 (0x31) daload
50 (0x32) aaload
51 (0x33) baload
52 (0x34) caload
53 (0x35) saload

// Stores
54 (0x36) istore
55 (0x37) lstore
56 (0x38) fstore
57 (0x39) dstore
58 (0x3a) astore
59 (0x3b) istore_0
60 (0x3c) istore_1
61 (0x3d) istore_2
62 (0x3e) istore_3
63 (0x3f) lstore_0
64 (0x40) lstore_1
65 (0x41) lstore_2
66 (0x42) lstore_3
67 (0x43) fstore_0
68 (0x44) fstore_1
69 (0x45) fstore_2
70 (0x46) fstore_3
71 (0x47) dstore_0
72 (0x48) dstore_1
73 (0x49) dstore_2
74 (0x4a) dstore_3
75 (0x4b) astore_0
76 (0x4c) astore_1
77 (0x4d) astore_2
78 (0x4e) astore_3
79 (0x4f) iastore
80 (0x50) lastore
81 (0x51) fastore
82 (0x52) dastore
83 (0x53) aastore
84 (0x54) bastore
85 (0x55) castore
86 (0x56) sastore

// Stack
87 (0x57) pop
88 (0x58) pop2
89 (0x59) dup
90 (0x5a) dup_x1
91 (0x5b) dup_x2
92 (0x5c) dup2
93 (0x5d) dup2_x1
94 (0x5e) dup2_x2
95 (0x5f) swap

// Math
96 (0x60) iadd
97 (0x61) ladd
98 (0x62) fadd
99 (0x63) dadd
100 (0x64) isub
101 (0x65) lsub
102 (0x66) fsub
103 (0x67) dsub
104 (0x68) imul
105 (0x69) lmul
106 (0x6a) fmul
107 (0x6b) dmul
108 (0x6c) idiv
109 (0x6d) ldiv
110 (0x6e) fdiv
111 (0x6f) ddiv
112 (0x70) irem
113 (0x71) lrem
114 (0x72) frem
115 (0x73) drem
116 (0x74) ineg
117 (0x75) lneg
118 (0x76) fneg
119 (0x77) dneg
120 (0x78) ishl
121 (0x79) lshl
122 (0x7a) ishr
123 (0x7b) lshr
124 (0x7c) iushr
125 (0x7d) lushr
126 (0x7e) iand
127 (0x7f) land
128 (0x80) ior
129 (0x81) lor
130 (0x82) ixor
131 (0x83) lxor
132 (0x84) iinc

// Conversion
133 (0x85) i2l
134 (0x86) i2f
135 (0x87) i2d
136 (0x88) l2i
137 (0x89) l2f
138 (0x8a) l2d
139 (0x8b) f2i
140 (0x8c) f2l
141 (0x8d) f2d
142 (0x8e) d2i
143 (0x8f) d2l
144 (0x90) d2f
145 (0x91) i2b
146 (0x92) i2c
147 (0x93) i2s

// Comparisons
148 (0x94) lcmp
149 (0x95) fcmpl
150 (0x96) fcmpg
151 (0x97) dcmpl
152 (0x98) dcmpg
153 (0x99) ifeq
154 (0x9a) ifne
155 (0x9b) iflt
156 (0x9c) ifge
157 (0x9d) ifgt
158 (0x9e) ifle
159 (0x9f) if_icmpeq
160 (0xa0) if_icmpne
161 (0xa1) if_icmplt
162 (0xa2) if_icmpge
163 (0xa3) if_icmpgt
164 (0xa4) if_icmple
165 (0xa5) if_acmpeq
166 (0xa6) if_acmpne

// Control
167 (0xa7) goto
168 (0xa8) jsr
169 (0xa9) ret
170 (0xaa) tableswitch
171 (0xab) lookupswitch
172 (0xac) ireturn
173 (0xad) lreturn
174 (0xae) freturn
175 (0xaf) dreturn
176 (0xb0) areturn
177 (0xb1) return

// References
178 (0xb2) getstatic
179 (0xb3) putstatic
180 (0xb4) getfield
181 (0xb5) putfield
182 (0xb6) invokevirtual
183 (0xb7) invokespecial
184 (0xb8) invokestatic
185 (0xb9) invokeinterface
186 (0xba) invokedynamic
187 (0xbb) new
188 (0xbc) newarray
189 (0xbd) anewarray
190 (0xbe) arraylength
191 (0xbf) athrow
192 (0xc0) checkcast
193 (0xc1) instanceof
194 (0xc2) monitorenter
195 (0xc3) monitorexit

// Extended
196 (0xc4) wide
197 (0xc5) multianewarray
198 (0xc6) ifnull
199 (0xc7) ifnonnull
200 (0xc8) goto_w
201 (0xc9) jsr_w

// Reserved
202 (0xca) breakpoint
254 (0xfe) impdep1
255 (0xff) impdep2
*/

enum OperandType {
  OPERAND_NONE,
  OPERAND_1BYTE,
  OPERAND_2BYTES,
  OPERAND_IINC, // index const
  OPERAND_MULTIANEWARRAY, // indexbyte1 indexbyte2 dimensions
  OPERAND_4BYTES,
  OPERAND_INVOKE_DYNAMIC, // indexbyte1 indexbyte2 0 0
  OPERAND_INVOKE_INTERFACE, // indexbyte1 indexbyte2 count 0
  OPERAND_WIDE,
  OPERAND_LOOKUPSWITCH,
  OPERAND_TABLESWITCH,
};

typedef std::pair<std::string, OperandType> PairOpNameAndType;

class OpCodes {
public:

  std::unordered_map<int, PairOpNameAndType> info;

  OpCodes() {
    info = {
      { 0x00, std::make_pair("nop", OPERAND_NONE) },
      { 0x01, std::make_pair("aconst_null", OPERAND_NONE) },
      { 0x02, std::make_pair("iconst_m1", OPERAND_NONE) },
      { 0x03, std::make_pair("iconst_0", OPERAND_NONE) },
      { 0x04, std::make_pair("iconst_1", OPERAND_NONE) },
      { 0x05, std::make_pair("iconst_2", OPERAND_NONE) },
      { 0x06, std::make_pair("iconst_3", OPERAND_NONE) },
      { 0x07, std::make_pair("iconst_4", OPERAND_NONE) },
      { 0x08, std::make_pair("iconst_5", OPERAND_NONE) },
      { 0x09, std::make_pair("lconst_0", OPERAND_NONE) },
      { 0x0a, std::make_pair("lconst_1", OPERAND_NONE) },
      { 0x0b, std::make_pair("fconst_0", OPERAND_NONE) },
      { 0x0c, std::make_pair("fconst_1", OPERAND_NONE) },
      { 0x0d, std::make_pair("fconst_2", OPERAND_NONE) },
      { 0x0e, std::make_pair("dconst_0", OPERAND_NONE) },
      { 0x0f, std::make_pair("dconst_1", OPERAND_NONE) },
      { 0x10, std::make_pair("bipush", OPERAND_1BYTE) },
      { 0x11, std::make_pair("sipush", OPERAND_2BYTES) },
      { 0x12, std::make_pair("ldc", OPERAND_1BYTE) },
      { 0x13, std::make_pair("ldc_w", OPERAND_2BYTES) },
      { 0x14, std::make_pair("ldc2_w", OPERAND_2BYTES) },
      { 0x15, std::make_pair("iload", OPERAND_1BYTE) },
      { 0x16, std::make_pair("lload", OPERAND_1BYTE) },
      { 0x17, std::make_pair("fload", OPERAND_1BYTE) },
      { 0x18, std::make_pair("dload", OPERAND_1BYTE) },
      { 0x19, std::make_pair("aload", OPERAND_1BYTE) },
      { 0x1a, std::make_pair("iload_0", OPERAND_NONE) },
      { 0x1b, std::make_pair("iload_1", OPERAND_NONE) },
      { 0x1c, std::make_pair("iload_2", OPERAND_NONE) },
      { 0x1d, std::make_pair("iload_3", OPERAND_NONE) },
      { 0x1e, std::make_pair("lload_0", OPERAND_NONE) },
      { 0x1f, std::make_pair("lload_1", OPERAND_NONE) },
      { 0x20, std::make_pair("lload_2", OPERAND_NONE) },
      { 0x21, std::make_pair("lload_3", OPERAND_NONE) },
      { 0x22, std::make_pair("fload_0", OPERAND_NONE) },
      { 0x23, std::make_pair("fload_1", OPERAND_NONE) },
      { 0x24, std::make_pair("fload_2", OPERAND_NONE) },
      { 0x25, std::make_pair("fload_3", OPERAND_NONE) },
      { 0x26, std::make_pair("dload_0", OPERAND_NONE) },
      { 0x27, std::make_pair("dload_1", OPERAND_NONE) },
      { 0x28, std::make_pair("dload_2", OPERAND_NONE) },
      { 0x29, std::make_pair("dload_3", OPERAND_NONE) },
      { 0x2a, std::make_pair("aload_0", OPERAND_NONE) },
      { 0x2b, std::make_pair("aload_1", OPERAND_NONE) },
      { 0x2c, std::make_pair("aload_2", OPERAND_NONE) },
      { 0x2d, std::make_pair("aload_3", OPERAND_NONE) },
      { 0x2e, std::make_pair("iaload", OPERAND_NONE) },
      { 0x2f, std::make_pair("laload", OPERAND_NONE) },
      { 0x30, std::make_pair("faload", OPERAND_NONE) },
      { 0x31, std::make_pair("daload", OPERAND_NONE) },
      { 0x32, std::make_pair("aaload", OPERAND_NONE) },
      { 0x33, std::make_pair("baload", OPERAND_NONE) },
      { 0x34, std::make_pair("caload", OPERAND_NONE) },
      { 0x35, std::make_pair("saload", OPERAND_NONE) },
      { 0x36, std::make_pair("istore", OPERAND_1BYTE) },
      { 0x37, std::make_pair("lstore", OPERAND_1BYTE) },
      { 0x38, std::make_pair("fstore", OPERAND_1BYTE) },
      { 0x39, std::make_pair("dstore", OPERAND_1BYTE) },
      { 0x3a, std::make_pair("astore", OPERAND_NONE) },
      { 0x3b, std::make_pair("istore_0", OPERAND_NONE) },
      { 0x3c, std::make_pair("istore_1", OPERAND_NONE) },
      { 0x3d, std::make_pair("istore_2", OPERAND_NONE) },
      { 0x3e, std::make_pair("istore_3", OPERAND_NONE) },
      { 0x3f, std::make_pair("lstore_0", OPERAND_NONE) },
      { 0x40, std::make_pair("lstore_1", OPERAND_NONE) },
      { 0x41, std::make_pair("lstore_2", OPERAND_NONE) },
      { 0x42, std::make_pair("lstore_3", OPERAND_NONE) },
      { 0x43, std::make_pair("fstore_0", OPERAND_NONE) },
      { 0x44, std::make_pair("fstore_1", OPERAND_NONE) },
      { 0x45, std::make_pair("fstore_2", OPERAND_NONE) },
      { 0x46, std::make_pair("fstore_3", OPERAND_NONE) },
      { 0x47, std::make_pair("dstore_0", OPERAND_NONE) },
      { 0x48, std::make_pair("dstore_1", OPERAND_NONE) },
      { 0x49, std::make_pair("dstore_2", OPERAND_NONE) },
      { 0x4a, std::make_pair("dstore_3", OPERAND_NONE) },
      { 0x4b, std::make_pair("astore_0", OPERAND_NONE) },
      { 0x4c, std::make_pair("astore_1", OPERAND_NONE) },
      { 0x4d, std::make_pair("astore_2", OPERAND_NONE) },
      { 0x4e, std::make_pair("astore_3", OPERAND_NONE) },
      { 0x4f, std::make_pair("iastore", OPERAND_NONE) },
      { 0x50, std::make_pair("lastore", OPERAND_NONE) },
      { 0x51, std::make_pair("fastore", OPERAND_NONE) },
      { 0x52, std::make_pair("dastore", OPERAND_NONE) },
      { 0x53, std::make_pair("aastore", OPERAND_NONE) },
      { 0x54, std::make_pair("bastore", OPERAND_NONE) },
      { 0x55, std::make_pair("castore", OPERAND_NONE) },
      { 0x56, std::make_pair("sastore", OPERAND_1BYTE) },
      { 0x57, std::make_pair("pop", OPERAND_NONE) },
      { 0x58, std::make_pair("pop2", OPERAND_NONE) },
      { 0x59, std::make_pair("dup", OPERAND_NONE) },
      { 0x5a, std::make_pair("dup_x1", OPERAND_NONE) },
      { 0x5b, std::make_pair("dup_x2", OPERAND_NONE) },
      { 0x5c, std::make_pair("dup2", OPERAND_NONE) },
      { 0x5d, std::make_pair("dup2_x1", OPERAND_NONE) },
      { 0x5e, std::make_pair("dup2_x2", OPERAND_NONE) },
      { 0x5f, std::make_pair("swap", OPERAND_NONE) },
      { 0x60, std::make_pair("iadd", OPERAND_NONE) },
      { 0x61, std::make_pair("ladd", OPERAND_NONE) },
      { 0x62, std::make_pair("fadd", OPERAND_NONE) },
      { 0x63, std::make_pair("dadd", OPERAND_NONE) },
      { 0x64, std::make_pair("isub", OPERAND_NONE) },
      { 0x65, std::make_pair("lsub", OPERAND_NONE) },
      { 0x66, std::make_pair("fsub", OPERAND_NONE) },
      { 0x67, std::make_pair("dsub", OPERAND_NONE) },
      { 0x68, std::make_pair("imul", OPERAND_NONE) },
      { 0x69, std::make_pair("lmul", OPERAND_NONE) },
      { 0x6a, std::make_pair("fmul", OPERAND_NONE) },
      { 0x6b, std::make_pair("dmul", OPERAND_NONE) },
      { 0x6c, std::make_pair("idiv", OPERAND_NONE) },
      { 0x6d, std::make_pair("ldiv", OPERAND_NONE) },
      { 0x6e, std::make_pair("fdiv", OPERAND_NONE) },
      { 0x6f, std::make_pair("ddiv", OPERAND_NONE) },
      { 0x70, std::make_pair("irem", OPERAND_NONE) },
      { 0x71, std::make_pair("lrem", OPERAND_NONE) },
      { 0x72, std::make_pair("frem", OPERAND_NONE) },
      { 0x73, std::make_pair("drem", OPERAND_NONE) },
      { 0x74, std::make_pair("ineg", OPERAND_NONE) },
      { 0x75, std::make_pair("lneg", OPERAND_NONE) },
      { 0x76, std::make_pair("fneg", OPERAND_NONE) },
      { 0x77, std::make_pair("dneg", OPERAND_NONE) },
      { 0x78, std::make_pair("ishl", OPERAND_NONE) },
      { 0x79, std::make_pair("lshl", OPERAND_NONE) },
      { 0x7a, std::make_pair("ishr", OPERAND_NONE) },
      { 0x7b, std::make_pair("lshr", OPERAND_NONE) },
      { 0x7c, std::make_pair("iushr", OPERAND_NONE) },
      { 0x7d, std::make_pair("lushr", OPERAND_NONE) },
      { 0x7e, std::make_pair("iand", OPERAND_NONE) },
      { 0x7f, std::make_pair("land", OPERAND_NONE) },
      { 0x80, std::make_pair("ior", OPERAND_NONE) },
      { 0x81, std::make_pair("lor", OPERAND_NONE) },
      { 0x82, std::make_pair("ixor", OPERAND_NONE) },
      { 0x83, std::make_pair("lxor", OPERAND_NONE) },
      { 0x84, std::make_pair("iinc", OPERAND_IINC) },
      { 0x85, std::make_pair("i2l", OPERAND_NONE) },
      { 0x86, std::make_pair("i2f", OPERAND_NONE) },
      { 0x87, std::make_pair("i2d", OPERAND_NONE) },
      { 0x88, std::make_pair("l2i", OPERAND_NONE) },
      { 0x89, std::make_pair("l2f", OPERAND_NONE) },
      { 0x8a, std::make_pair("l2d", OPERAND_NONE) },
      { 0x8b, std::make_pair("f2i", OPERAND_NONE) },
      { 0x8c, std::make_pair("f2l", OPERAND_NONE) },
      { 0x8d, std::make_pair("f2d", OPERAND_NONE) },
      { 0x8e, std::make_pair("d2i", OPERAND_NONE) },
      { 0x8f, std::make_pair("d2l", OPERAND_NONE) },
      { 0x90, std::make_pair("d2f", OPERAND_NONE) },
      { 0x91, std::make_pair("i2b", OPERAND_NONE) },
      { 0x92, std::make_pair("i2c", OPERAND_NONE) },
      { 0x93, std::make_pair("i2s", OPERAND_NONE) },
      { 0x94, std::make_pair("lcmp", OPERAND_NONE) },
      { 0x95, std::make_pair("fcmpl", OPERAND_NONE) },
      { 0x96, std::make_pair("fcmpg", OPERAND_NONE) },
      { 0x97, std::make_pair("dcmpl", OPERAND_NONE) },
      { 0x98, std::make_pair("dcmpg", OPERAND_NONE) },
      { 0x99, std::make_pair("ifeq", OPERAND_2BYTES) },
      { 0x9a, std::make_pair("ifne", OPERAND_2BYTES) },
      { 0x9b, std::make_pair("iflt", OPERAND_2BYTES) },
      { 0x9c, std::make_pair("ifge", OPERAND_2BYTES) },
      { 0x9d, std::make_pair("ifgt", OPERAND_2BYTES) },
      { 0x9e, std::make_pair("ifle", OPERAND_2BYTES) },
      { 0x9f, std::make_pair("if_icmpeq", OPERAND_2BYTES) },
      { 0xa0, std::make_pair("if_icmpne", OPERAND_2BYTES) },
      { 0xa1, std::make_pair("if_icmplt", OPERAND_2BYTES) },
      { 0xa2, std::make_pair("if_icmpge", OPERAND_2BYTES) },
      { 0xa3, std::make_pair("if_icmpgt", OPERAND_2BYTES) },
      { 0xa4, std::make_pair("if_icmple", OPERAND_2BYTES) },
      { 0xa5, std::make_pair("if_acmpeq", OPERAND_2BYTES) },
      { 0xa6, std::make_pair("if_acmpne", OPERAND_2BYTES) },
      { 0xa7, std::make_pair("goto", OPERAND_2BYTES) },
      { 0xa8, std::make_pair("jsr", OPERAND_2BYTES) },
      { 0xa9, std::make_pair("ret", OPERAND_1BYTE) },
      { 0xaa, std::make_pair("tableswitch", OPERAND_TABLESWITCH) },
      { 0xab, std::make_pair("lookupswitch", OPERAND_LOOKUPSWITCH) },
      { 0xac, std::make_pair("ireturn", OPERAND_NONE) },
      { 0xad, std::make_pair("lreturn", OPERAND_NONE) },
      { 0xae, std::make_pair("freturn", OPERAND_NONE) },
      { 0xaf, std::make_pair("dreturn", OPERAND_NONE) },
      { 0xb0, std::make_pair("areturn", OPERAND_NONE) },
      { 0xb1, std::make_pair("return", OPERAND_NONE) },
      { 0xb2, std::make_pair("getstatic", OPERAND_2BYTES) },
      { 0xb3, std::make_pair("putstatic", OPERAND_2BYTES) },
      { 0xb4, std::make_pair("getfield", OPERAND_2BYTES) },
      { 0xb5, std::make_pair("putfield", OPERAND_2BYTES) },
      { 0xb6, std::make_pair("invokevirtual", OPERAND_2BYTES) },
      { 0xb7, std::make_pair("invokespecial", OPERAND_2BYTES) },
      { 0xb8, std::make_pair("invokestatic", OPERAND_2BYTES) },
      { 0xb9, std::make_pair("invokeinterface", OPERAND_INVOKE_INTERFACE) },
      { 0xba, std::make_pair("invokedynamic", OPERAND_INVOKE_DYNAMIC) },
      { 0xbb, std::make_pair("new", OPERAND_2BYTES) },
      { 0xbc, std::make_pair("newarray", OPERAND_1BYTE) },
      { 0xbd, std::make_pair("anewarray", OPERAND_2BYTES) },
      { 0xbe, std::make_pair("arraylength", OPERAND_NONE) },
      { 0xbf, std::make_pair("athrow", OPERAND_NONE) },
      { 0xc0, std::make_pair("checkcast", OPERAND_2BYTES) },
      { 0xc1, std::make_pair("instanceof", OPERAND_2BYTES) },
      { 0xc2, std::make_pair("monitorenter", OPERAND_NONE) },
      { 0xc3, std::make_pair("monitorexit", OPERAND_NONE) },
      { 0xc4, std::make_pair("wide", OPERAND_WIDE) },
      { 0xc5, std::make_pair("multianewarray", OPERAND_MULTIANEWARRAY) },
      { 0xc6, std::make_pair("ifnull", OPERAND_2BYTES) },
      { 0xc7, std::make_pair("ifnonnull", OPERAND_2BYTES) },
      { 0xc8, std::make_pair("goto_w", OPERAND_4BYTES) },
      { 0xc9, std::make_pair("jsr_w", OPERAND_4BYTES) },
      { 0xca, std::make_pair("breakpoint", OPERAND_NONE) },
      { 0xfe, std::make_pair("impdep1", OPERAND_NONE) },
      { 0xff, std::make_pair("impdep2", OPERAND_NONE) },
    };
  }
};
} // namespace
#endif
