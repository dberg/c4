//-*- C++ -*-
#ifndef __C4_SCALA_TOKENS_H__
#define __C4_SCALA_TOKENS_H__

namespace c4s {

//-----------------------------------------------------------------------------
// Common tokens
//-----------------------------------------------------------------------------
enum class Token {
  // special tokens
  T_EMPTY = -3,
  T_UNDEF = -2,
  T_ERROR = -1,
  T_EOF = 0,

  // literals
  T_CHARLIT = 1,
  T_INTLIT = 2,
  T_LONGLIT = 3,
  T_FLOATLIT = 4,
  T_DOUBLELIT = 5,
  T_STRINGLIT = 6,

  // keywords
  T_NEW = 20,
  T_THIS = 21,
  T_SUPER = 23,

  T_NULL = 24,
  T_TRUE = 25,
  T_FALSE = 26,

  // J: INSTANCEOF = 27
  // J: CONST = 28

  // modifiers
  // S: IMPLICIT = 40
  // S: OVERRIDE = 41
  // J: PUBLIC = 42
  T_PROTECTED = 43,
  T_PRIVATE = 44,
  // S: SEALED = 45
  T_ABSTRACT = 46,
  // J: DEFAULT = 47
  // J: STATIC = 48
  T_FINAL = 49,
  // J: TRANSIENT = 50
  // J: VOLATILE = 51
  // J: SYNCHRONIZED = 52
  // J: NATIVE = 53
  // J: STRICTFP = 54
  // S: LAZY = 55
  // J: THROWS = 56
  // S: MACRO = 57

  // templates
  T_PACKAGE = 60,
  T_IMPORT = 61,
  T_CLASS = 62,
  // S: CASECLASS = 63
  // S: OBJECT = 64
  // S: CASEOBJECT = 65
  // S: TRAIT, J: INTERFACE = 66
  // J: ENUM = 67
  T_EXTENDS = 68,
  // S: WITH, J: IMPLEMENTS = 69
  // S: TYPE = 70
  // S: FORSOME = 71
  // S: DEF = 72
  // S: VAL = 73
  // S: VAR = 74

  // control structures
  T_IF = 80,
  // S: THEN = 81
  T_ELSE = 82,
  T_WHILE = 83,
  T_DO = 84,
  T_FOR = 85,
  // S: YIELD = 86
  // J: BREAK = 87
  // J: CONTINUE = 88
  // J: GOTO = 89
  T_THROW = 90,
  T_TRY = 91,
  T_CATCH = 92,
  T_FINALLY = 93,
  // J: SWITCH = 94
  // S: MATCH = 95
  T_CASE = 96,
  T_RETURN = 97,
  // J: ASSERT = 98

  // parenthesis
  T_LPAREN = 100,
  T_RPAREN = 101,
  T_LBRACKET = 102,
  T_RBRACKET = 103,
  T_LBRACE = 104,
  T_RBRACE = 105,

  // special symbols
  T_COMMA = 120,
  T_SEMI = 121,
  T_DOT = 122,
  T_COLON = 123,
  T_EQUALS = 124,
  T_AT = 125,
  // S: <special symbols> = 130 - 139
  // J: <special symbols> = 140 - 179
  // J: <primitive types> = 180 - 189

//-----------------------------------------------------------------------------
// Scala Tokens
//-----------------------------------------------------------------------------
  // a part of an interpolated string
  T_STRINGPART = 7,
  T_SYMBOLLIT = 8,
  // the lead identifier of an interpolated string
  T_INTERPOLATIONID = 9,

  // identifiers
  T_IDENTIFIER = 10,
  T_BACKQUOTED_IDENT = 11,

  // modifiers
  T_IMPLICIT = 40,
  T_OVERRIDE = 41,
  T_SEALED = 45,
  T_LAZY = 55,
  T_MACRO = 57,

  // templates
  T_CASECLASS = 63,
  T_OBJECT = 64,
  T_CASEOBJECT = 65,
  T_TRAIT = 66,
  T_WITH = 69,
  T_TYPE = 70,
  T_FORSOME = 71,
  T_DEF = 72,
  T_VAL = 73,
  T_VAR = 74,

  // control structures
  T_THEN = 81,
  T_YIELD = 86,
  T_MATCH = 95,

  // special symbols
  T_HASH = 130,
  T_USCORE = 131,
  T_ARROW = 132,
  T_LARROW = 133,
  T_SUBTYPE = 134,
  T_SUPERTYPE = 135,
  T_VIEWBOUND = 136,
  T_NEWLINE = 137,
  T_NEWLINES = 138,
  T_XMLSTART = 139,

  // for emacs only
  T_COMMENT = 200,
  T_WHITESPACE = 201,
  T_IGNORE = 202,
  T_ESCAPE = 203
}; // enum class

} // namespace

#endif
