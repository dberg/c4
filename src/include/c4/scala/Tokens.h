//-*- C++ -*-
#ifndef __C4_SCALA_TOKENS_H__
#define __C4_SCALA_TOKENS_H__

namespace c4s {

typedef int Token;

//-----------------------------------------------------------------------------
// Common tokens
//-----------------------------------------------------------------------------
// special tokens
Token EMPTY = -3;
Token UNDEF = -2;
Token ERROR = -1;
Token EOF = 0;

// literals
Token CHARLIT = 1;
Token INTLIT = 2;
Token LONGLIT = 3;
Token FLOATLIT = 4;
Token DOUBLELIT = 5;
Token STRINGLIT = 6;

// keywords
Token NEW = 20;
Token THIS = 21;
Token SUPER = 23;

Token NULL = 24;
Token TRUE = 25;
Token FALSE = 26;

// J: INSTANCEOF = 27
// J: CONST = 28

// modifiers
// S: IMPLICIT = 40
// S: OVERRIDE = 41
// J: PUBLIC = 42
Token PROTECTED = 43;
Token PRIVATE = 44;
// S: SEALED = 45
Token ABSTRACT = 46;
// J: DEFAULT = 47
// J: STATIC = 48
Token FINAL = 49;
// J: TRANSIENT = 50
// J: VOLATILE = 51
// J: SYNCHRONIZED = 52
// J: NATIVE = 53
// J: STRICTFP = 54
// S: LAZY = 55
// J: THROWS = 56
// S: MACRO = 57

// templates
Token PACKAGE = 60;
Token IMPORT = 61;
Token CLASS = 62;
// S: CASECLASS = 63
// S: OBJECT = 64
// S: CASEOBJECT = 65
// S: TRAIT, J: INTERFACE = 66
// J: ENUM = 67
Token EXTENDS = 68;
// S: WITH, J: IMPLEMENTS = 69
// S: TYPE = 70
// S: FORSOME = 71
// S: DEF = 72
// S: VAL = 73
// S: VAR = 74

// control structures
Token IF = 80;
// S: THEN = 81
Token ELSE = 82;
Token WHILE = 83;
Token DO = 84;
Token FOR = 85;
// S: YIELD = 86
// J: BREAK = 87
// J: CONTINUE = 88
// J: GOTO = 89
Token THROW = 90;
Token TRY = 91;
Token CATCH = 92;
Token FINALLY = 93;
// J: SWITCH = 94
// S: MATCH = 95
Token CASE = 96;
Token RETURN = 97;
// J: ASSERT = 98

// parenthesis
Token LPAREN = 100;
Token RPAREN = 101;
Token LBRACKET = 102;
Token RBRACKET = 103;
Token LBRACE = 104;
Token RBRACE = 105;

// special symbols
Token COMMA = 120;
Token SEMI = 121;
Token DOT = 122;
Token COLON = 123;
Token EQUALS = 124;
Token AT = 125;
// S: <special symbols> = 130 - 139
// J: <special symbols> = 140 - 179
// J: <primitive types> = 180 - 189

//-----------------------------------------------------------------------------
// Scala Tokens
//-----------------------------------------------------------------------------
// a part of an interpolated string
Token STRINGPART = 7;
Token SYMBOLLIT = 8;
// the lead identifier of an interpolated string
Token INTERPOLATIONID = 9;

// identifiers
Token IDENTIFIER = 10;
Token BACKQUOTED_IDENT = 11;

// modifiers
Token IMPLICIT = 40;
Token OVERRIDE = 41;
Token SEALED = 45;
Token LAZY = 55;
Token MACRO = 57;

// templates
Token CASECLASS = 63;
Token OBJECT = 64;
Token CASEOBJECT = 65;
Token TRAIT = 66;
Token WITH = 69;
Token TYPE = 70;
Token FORSOME = 71;
Token DEF = 72;
Token VAL = 73;
Token VAR = 74;

// control structures
Token THEN = 81;
Token YIELD = 86;
Token MATCH = 95;

// special symbols
Token HASH = 130;
Token USCORE = 131;
Token ARROW = 132;
Token LARROW = 133;
Token SUBTYPE = 134;
Token SUPERTYPE = 135;
Token VIEWBOUND = 136;
Token NEWLINE = 137;
Token NEWLINES = 138;
Token XMLSTART = 139;

// for emacs only
Token COMMENT = 200;
Token WHITESPACE = 201;
Token IGNORE = 202;
Token ESCAPE = 203;

} // namespace

#endif
