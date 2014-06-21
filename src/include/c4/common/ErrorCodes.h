//-*- C++ -*-
#ifndef __C4_COMMON_ERROR_CODES__
#define __C4_COMMON_ERROR_CODES__

#include <unordered_map>
#include <memory>
#include <string>

namespace c4 {

struct Error {
  int type;
  unsigned ini;
  unsigned end;
  Error(int type, unsigned ini, unsigned end)
    : type(type), ini(ini), end(end) {}
};

typedef std::shared_ptr<struct Error> spError;

/**
 * Error codes for the class Diagnosis shared by Java and Scala.
 */
enum ErrorCode {
  ERR_EXP_ARGUMENTS,
  ERR_EXP_ARRAY,
  ERR_EXP_CATCH,
  ERR_EXP_CLASS,
  ERR_EXP_CLASS_OR_ARRAY_CREATOR_REST,
  ERR_EXP_COMMA,
  ERR_EXP_ELEMENT_VALUE,
  ERR_EXP_EXPRESSION_IN_BRACKETS,
  ERR_EXP_IDENTIFIER,
  ERR_EXP_IMPLICIT,
  ERR_EXP_INTERFACE,
  ERR_EXP_LCURLY_BRACKET,
  ERR_EXP_LBRACKET,
  ERR_EXP_LPAREN,
  ERR_EXP_NL,
  ERR_EXP_OP_COLON,
  ERR_EXP_OP_EQUALS,
  ERR_EXP_OP_LT,
  ERR_EXP_OP_GT,
  ERR_EXP_PERIOD,
  ERR_EXP_QID,
  ERR_EXP_RBRACKET,
  ERR_EXP_RCURLY_BRACKET,
  ERR_EXP_REFTYPE,
  ERR_EXP_RPAREN,
  ERR_EXP_SEMICOLON,
  ERR_EXP_STAR,
  ERR_EXP_STRING_LITERAL,
  ERR_EXP_SUPER,
  ERR_EXP_TRAIT,
  ERR_EXP_TYPE,
  ERR_EXP_UNDERSCORE,
  ERR_EXP_VOID,
  ERR_EXP_WHILE,
  ERR_NVAL_ANNOT_ELEM,
  ERR_NVAL_ARRAY,
  ERR_NVAL_EXPLICIT_GENERIC_INVOCATION_SUFFIX,
  ERR_NVAL_HEX,
  ERR_NVAL_IDENTIFIER_SUFFIX,
  ERR_NVAL_SELECTOR,
  ERR_NVAL_TYPE_ARGUMENT,
  ERR_VAR_MODIFIER_FINAL,
};

class ErrorUtil {
  std::unordered_map<int, std::string> msgs = {
    { ERR_EXP_ARGUMENTS, "Expected Arguments" },
    { ERR_EXP_ARRAY, "Expected Array" },
    { ERR_EXP_CATCH, "Expected 'catch' expression" },
    { ERR_EXP_CLASS, "Expected keyword class" },
    { ERR_EXP_CLASS_OR_ARRAY_CREATOR_REST,
      "Expected ClassCreatorRest or ArrayCreatorRest" },
    { ERR_EXP_COMMA, "Expected comma" },
    { ERR_EXP_ELEMENT_VALUE, "Expected element value" },
    { ERR_EXP_EXPRESSION_IN_BRACKETS, "Expected [Expression]" },
    { ERR_EXP_IDENTIFIER, "Expected identifier" },
    { ERR_EXP_IMPLICIT, "Expected implicit keyword" },
    { ERR_EXP_INTERFACE, "Expected interface" },
    { ERR_EXP_LCURLY_BRACKET, "Expected opening curly brackets" },
    { ERR_EXP_LBRACKET, "Expected '['" },
    { ERR_EXP_LPAREN, "Expected opening parenthesis" },
    { ERR_EXP_NL, "Expected new line" },
    { ERR_EXP_OP_COLON, "Expected colon" },
    { ERR_EXP_OP_EQUALS, "Expected operator =" },
    { ERR_EXP_OP_LT, "Expected operator <" },
    { ERR_EXP_OP_GT, "Expected operator >" },
    { ERR_EXP_PERIOD, "Expected period" },
    { ERR_EXP_QID, "Expected qualified id" },
    { ERR_EXP_RBRACKET, "Expected ']'" },
    { ERR_EXP_RCURLY_BRACKET, "Expected closing curly brackets" },
    { ERR_EXP_REFTYPE, "Expected reference type" },
    { ERR_EXP_RPAREN, "Expected closing parenthesis" },
    { ERR_EXP_SEMICOLON, "Expected semi-colon" },
    { ERR_EXP_STAR, "Expected *" },
    { ERR_EXP_STRING_LITERAL, "Expected string literal" },
    { ERR_EXP_SUPER, "Expected keyword super" },
    { ERR_EXP_TYPE, "Expected Type" },
    { ERR_EXP_TRAIT, "Expected keyword trait" },
    { ERR_EXP_UNDERSCORE, "Expected keyword underscore" },
    { ERR_EXP_VOID, "Expected keyword void" },
    { ERR_EXP_WHILE, "Expected keyword while" },
    { ERR_NVAL_ANNOT_ELEM, "Invalid annotation element" },
    { ERR_NVAL_ARRAY, "Invalid array notation" },
    { ERR_NVAL_EXPLICIT_GENERIC_INVOCATION_SUFFIX,
      "Invalid ExplicitGenericInvocationSuffix" },
    { ERR_NVAL_HEX, "Invalid Hexadecimal" },
    { ERR_NVAL_IDENTIFIER_SUFFIX, "Invalid IdentifierSuffix" },
    { ERR_NVAL_SELECTOR, "Invalid Selector" },
    { ERR_NVAL_TYPE_ARGUMENT, "Invalid Type Argument" },
    { ERR_VAR_MODIFIER_FINAL, "Duplicate 'final' keyword" },
  };

public:
  const std::string getMessage(int error) {
    auto it = msgs.find(error);
    if (it == msgs.end()) {
      return "";
    }
    return it->second;
  }
};


} // namespace

#endif
