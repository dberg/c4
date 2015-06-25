//-*- C++ -*-
#ifndef __C4_COMMON_ERROR_CODES__
#define __C4_COMMON_ERROR_CODES__

#include <unordered_map>
#include <memory>
#include <string>

using std::u32string;

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
 * Error codes for the class Diagnosis.
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
  std::unordered_map<int, u32string> msgs = {
    { ERR_EXP_ARGUMENTS, U"Expected Arguments" },
    { ERR_EXP_ARRAY, U"Expected Array" },
    { ERR_EXP_CATCH, U"Expected 'catch' expression" },
    { ERR_EXP_CLASS, U"Expected keyword class" },
    { ERR_EXP_CLASS_OR_ARRAY_CREATOR_REST,
      U"Expected ClassCreatorRest or ArrayCreatorRest" },
    { ERR_EXP_COMMA, U"Expected comma" },
    { ERR_EXP_ELEMENT_VALUE, U"Expected element value" },
    { ERR_EXP_EXPRESSION_IN_BRACKETS, U"Expected [Expression]" },
    { ERR_EXP_IDENTIFIER, U"Expected identifier" },
    { ERR_EXP_IMPLICIT, U"Expected implicit keyword" },
    { ERR_EXP_INTERFACE, U"Expected interface" },
    { ERR_EXP_LCURLY_BRACKET, U"Expected opening curly brackets" },
    { ERR_EXP_LBRACKET, U"Expected '['" },
    { ERR_EXP_LPAREN, U"Expected opening parenthesis" },
    { ERR_EXP_NL, U"Expected new line" },
    { ERR_EXP_OP_COLON, U"Expected colon" },
    { ERR_EXP_OP_EQUALS, U"Expected operator =" },
    { ERR_EXP_OP_LT, U"Expected operator <" },
    { ERR_EXP_OP_GT, U"Expected operator >" },
    { ERR_EXP_PERIOD, U"Expected period" },
    { ERR_EXP_QID, U"Expected qualified id" },
    { ERR_EXP_RBRACKET, U"Expected ']'" },
    { ERR_EXP_RCURLY_BRACKET, U"Missing closing curly brackets" },
    { ERR_EXP_REFTYPE, U"Expected reference type" },
    { ERR_EXP_RPAREN, U"Expected closing parenthesis" },
    { ERR_EXP_SEMICOLON, U"Expected semi-colon" },
    { ERR_EXP_STAR, U"Expected *" },
    { ERR_EXP_STRING_LITERAL, U"Expected string literal" },
    { ERR_EXP_SUPER, U"Expected keyword super" },
    { ERR_EXP_TYPE, U"Expected Type" },
    { ERR_EXP_TRAIT, U"Expected keyword trait" },
    { ERR_EXP_UNDERSCORE, U"Expected keyword underscore" },
    { ERR_EXP_VOID, U"Expected keyword void" },
    { ERR_EXP_WHILE, U"Expected keyword while" },
    { ERR_NVAL_ANNOT_ELEM, U"Invalid annotation element" },
    { ERR_NVAL_ARRAY, U"Invalid array notation" },
    { ERR_NVAL_EXPLICIT_GENERIC_INVOCATION_SUFFIX,
      U"Invalid ExplicitGenericInvocationSuffix" },
    { ERR_NVAL_HEX, U"Invalid Hexadecimal" },
    { ERR_NVAL_IDENTIFIER_SUFFIX, U"Invalid IdentifierSuffix" },
    { ERR_NVAL_SELECTOR, U"Invalid Selector" },
    { ERR_NVAL_TYPE_ARGUMENT, U"Invalid Type Argument" },
    { ERR_VAR_MODIFIER_FINAL, U"Duplicate 'final' keyword" },
  };

public:
  const u32string getMessage(int error) {
    auto it = msgs.find(error);
    if (it == msgs.end()) {
      return U"";
    }
    return it->second;
  }
};


} // namespace

#endif
