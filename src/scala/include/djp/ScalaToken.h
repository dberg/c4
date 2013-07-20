//-*- C++ -*-
#ifndef __SCALA_TOKEN_H__
#define __SCALA_TOKEN_H__
#include <unordered_map>
#include <string>

// To use STok as a map key we have to specialize the std namespace with a hash
// function for STok.
namespace djp { enum class STok: int; } // forward decl
namespace std {
  template <>
  struct hash<djp::STok> {
    size_t operator()(const djp::STok &tok) const {
      return hash<int>()(static_cast<int>(tok));
    }
  };
}

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

  // Grammar
  ID = -52,
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

  std::unordered_map<STok, int> TokensLength = {
    { STok::ABSTRACT, 8 },
    { STok::CASE, 4 },
    { STok::CATCH, 5 },
    { STok::CLASS, 5 },
    { STok::DEF, 3 },
    { STok::DO, 2 },
    { STok::ELSE, 4 },
    { STok::EXTENDS, 7 },
    { STok::FALSE, 5 },
    { STok::FINAL, 5 },
    { STok::FINALLY, 7 },
    { STok::FOR, 3 },
    { STok::FORSOME, 7 },
    { STok::IF, 2 },
    { STok::IMPLICIT, 8 },
    { STok::IMPORT, 6 },
    { STok::LAZY, 4 },
    { STok::MATCH, 5 },
    { STok::NEW, 3 },
    { STok::NULL_KEY, 4 },
    { STok::OBJECT, 6 },
    { STok::OVERRIDE, 8 },
    { STok::PACKAGE, 7 },
    { STok::PRIVATE, 7 },
    { STok::PROTECTED, 9 },
    { STok::RETURN, 6 },
    { STok::SEALED, 6 },
    { STok::SUPER, 5 },
    { STok::THIS, 4 },
    { STok::THROW, 5 },
    { STok::TRAIT, 5 },
    { STok::TRY, 3 },
    { STok::TRUE, 4 },
    { STok::TYPE, 4 },
    { STok::VAL, 3 },
    { STok::VAR, 3 },
    { STok::WHILE, 5 },
    { STok::WITH, 4 },
    { STok::YIELD, 5 },
    { STok::UNDERSCORE, 1 },
    { STok::COLON, 1 },
    { STok::EQUALS, 1 },
    { STok::EQUALS_GT, 2 },
    { STok::LT_DASH, 2 },
    { STok::LT_COLON, 2 },
    { STok::LT_PERCENTAGE, 2 },
    { STok::GT_COLON, 2 },
    { STok::HASH, 1 },
    { STok::AT, 1 },
  };

public:
  STok getReservedWordToken(std::string id) {
    auto it = ReservedWords.find(id);
    if (it == ReservedWords.end()) {
      return STok::ERROR;
    }
    return it->second;
  }

  int getTokenLength(STok token) {
    auto it = TokensLength.find(token);
    if (it == TokensLength.end()) {
      return 0;
    }
    return it->second;
  }

};

} // namespace

#endif
