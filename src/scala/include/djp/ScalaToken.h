//-*- C++ -*-
#ifndef __SCALA_TOKEN_H__
#define __SCALA_TOKEN_H__

namespace djp {

enum class STok {
  END_OF_FILE = -1,
  ERROR = -2,

  // Reserved words
  ABSTRACT = -3,
  CASE = -4,
  CATCH = -5,
  CLASS = -6,
  DEF = -7,
  DO = -8,
  ELSE = -9,
  EXTENDS = -10,
  FALSE = -11,
  FINAL = -12,
  FINALLY = -13,
  FOR = -14,
  FORSOME = -15,
  IF = -16,
  IMPLICIT = -17,
  IMPORT = -18,
  LAZY = -19,
  MATCH = -20,
  NEW = -21,
  NULL_KEY = -22, // null
  OBJECT = -23,
  OVERRIDE = -24,
  PACKAGE = -25,
  PRIVATE = -26,
  PROTECTED = -27,
  RETURN = -28,
  SEALED = -29,
  SUPER = -30,
  THIS = -31,
  THROW = -32,
  TRAIT = -33,
  TRY = -34,
  TRUE = -35,
  TYPE = -36,
  VAL = -37,
  VAR = -38,
  WHILE = -39,
  WITH = -40,
  YIELD = -41,
  UNDERSCORE = -42, // _
  COLON = -43, // :
  EQUALS = -44, // =
  EQUALS_GT = -45, // =>
  LT_DASH = -46, // <-
  LT_COLON = -47, // <:
  LT_PERCENTAGE = -48, // <%
  GT_COLON = -49, // >:
  HASH = -50, // #
  AT = -51, // @
};

} // namespace

#endif
