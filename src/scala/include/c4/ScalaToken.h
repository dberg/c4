//-*- C++ -*-
#ifndef __SCALA_TOKEN_H__
#define __SCALA_TOKEN_H__
#include <unordered_map>
#include <string>

namespace c4s {

enum class STok : int {
  ERROR,
  END_OF_FILE,

  // Reserved words
  ABSTRACT,
  CASE,
  CATCH,
  CLASS,
  DEF,
  DO,
  ELSE,
  EXTENDS,
  FALSE,
  FINAL,
  FINALLY,
  FOR,
  FORSOME,
  IF,
  IMPLICIT,
  IMPORT,
  LAZY,
  MATCH,
  NEW,
  NULL_KEY, // null
  OBJECT,
  OVERRIDE,
  PACKAGE,
  PRIVATE,
  PROTECTED,
  RETURN,
  SEALED,
  SUPER,
  THIS,
  THROW,
  TRAIT,
  TRY,
  TRUE,
  TYPE,
  VAL,
  VAR,
  WHILE,
  WITH,
  YIELD,
  UNDERSCORE, // _
  COLON, // :
  EQUALS, // =
  EQUALS_GT, // =>
  LT_DASH, // <-
  LT_COLON, // <:
  LT_PERCENTAGE, // <%
  GT_COLON, // >:
  HASH, // #
  AT, // @

  // Lexer grammar
  ID,
  ESCAPE_SEQUENCE,

  // Literals
  STRING_LITERAL,
  DECIMAL_NUMERAL,
  DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX,
  DECIMAL_FLOATING_POINT_LITERAL,
  HEX_NUMERAL,
  HEX_NUMERAL_WITH_INT_TYPE_SUFFIX,
  HEXADECIMAL_FLOATING_POINT_LITERAL,
  OCTAL_NUMERAL,
  OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX,

  // Symbol Terminals
  COMMA, // ,
  PERIOD, // .
  SEMICOLON, // ;
  LCURLYB, // {
  RCURLYB, // }
  LPAREN, // (
  RPAREN, // )
  LBRACKET, // [
  RBRACKET, // ]

  // Operators
  MINUS,
  PLUS,
  DIV,
  MUL,
  SLASH_EQUALS,

  COMMENT,
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
