//-*- C++ -*-
#ifndef __SCALA_TOKEN_H__
#define __SCALA_TOKEN_H__
#include <unordered_map>
#include <string>

namespace djp {

enum class STok : int {
  ERROR = -1,
  END_OF_FILE = -2,

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

  // Lexer grammar
  ID = -52,
  ESCAPE_SEQUENCE,

  // Literals
  STRING_LITERAL,

  // Symbol Terminals
  COMMA = -53, // ,
  SEMICOLON = -54, // ;
  LCURLYB = -55, // {
  RCURLYB = -56, // }
  LPAREN = -57, // (
  RPAREN = -58, // )
  LBRACKET = -59, // [
  RBRACKET = -60, // ]
};

class ScalaTokenUtil {

  std::unordered_map<std::string, STok> ReservedWords = {
    { "abstract", STok::ABSTRACT },
    { "case", STok::CASE },
    { "catch", STok::CATCH },
    { "class", STok::CLASS },
    { "def", STok::DEF },
    { "do", STok::DO },
    { "else", STok::ELSE },
    { "extends", STok::EXTENDS },
    { "false", STok::FALSE },
    { "final", STok::FINAL },
    { "finally", STok::FINALLY },
    { "for", STok::FOR },
    { "forsome", STok::FORSOME },
    { "if", STok::IF },
    { "implicit", STok::IMPLICIT },
    { "import", STok::IMPORT },
    { "lazy", STok::LAZY },
    { "match", STok::MATCH },
    { "new", STok::NEW },
    { "null", STok::NULL_KEY },
    { "object", STok::OBJECT },
    { "override", STok::OVERRIDE },
    { "package", STok::PACKAGE },
    { "private", STok::PRIVATE },
    { "protected", STok::PROTECTED },
    { "return", STok::RETURN },
    { "sealed", STok::SEALED },
    { "super", STok::SUPER },
    { "this", STok::THIS },
    { "throw", STok::THROW },
    { "trait", STok::TRAIT },
    { "try", STok::TRY },
    { "true", STok::TRUE },
    { "type", STok::TYPE },
    { "val", STok::VAL },
    { "var", STok::VAR },
    { "while", STok::WHILE },
    { "with", STok::WITH },
    { "yield", STok::YIELD },
    { "_", STok::UNDERSCORE },
    { ":", STok::COLON },
    { "=", STok::EQUALS },
    { "=>", STok::EQUALS_GT },
    { "<-", STok::LT_DASH },
    { "<:", STok::LT_COLON },
    { "<%", STok::LT_PERCENTAGE },
    { ">:", STok::GT_COLON },
    { "#", STok::HASH },
    { "@", STok::AT },
  };

public:
  STok getReservedWordToken(std::string id) {
    auto it = ReservedWords.find(id);
    if (it == ReservedWords.end()) {
      return STok::ERROR;
    }
    return it->second;
  }
};

} // namespace

#endif
