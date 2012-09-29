//-*- C++ -*-
#ifndef __ERROR_CODES__
#define __ERROR_CODES__
#include <map>
#include <boost/shared_ptr.hpp>

namespace djp {

struct Error {
  int ini, end, type;
  Error(int ini, int end, int type) : ini(ini), end(end), type(type) {}
};

typedef boost::shared_ptr<struct Error> spError;

enum ErrorCode {
  ERR_EXP_ARGUMENTS,
  ERR_EXP_ARRAY,
  ERR_EXP_QID,
  ERR_EXP_IDENTIFIER,
  ERR_EXP_ELEMENT_VALUE,
  ERR_EXP_EXPRESSION_IN_BRACKETS,
  ERR_EXP_RBRACKET,
  ERR_EXP_LPAREN,
  ERR_EXP_RPAREN,
  ERR_EXP_LCURLY_BRACKET,
  ERR_EXP_RCURLY_BRACKET,
  ERR_EXP_TYPE,
  ERR_EXP_OP_LT,
  ERR_EXP_OP_GT,
  ERR_EXP_REFTYPE,
  ERR_EXP_CLASS_OR_ARRAY_CREATOR_REST,
  ERR_NVAL_ANNOT_ELEM,
  ERR_NVAL_ARRAY,
  ERR_NVAL_EXPLICIT_GENERIC_INVOCATION_SUFFIX,
  ERR_NVAL_HEX,
  ERR_NVAL_TYPE_ARGUMENT,
};

class ErrorUtil {
  typedef std::map<int, std::string> ErrorMap;
  ErrorMap msgs;

public:
  ErrorUtil() {
    msgs[ERR_EXP_ARGUMENTS] = "Expected Arguments";
    msgs[ERR_EXP_ARRAY] = "Expected Array";
    msgs[ERR_EXP_QID] = "Expected qualified id";
    msgs[ERR_EXP_ELEMENT_VALUE] = "Expected element value";
    msgs[ERR_EXP_EXPRESSION_IN_BRACKETS] = "Expected [Expression]";
    msgs[ERR_EXP_IDENTIFIER] = "Expected identifier";
    msgs[ERR_EXP_RBRACKET] = "Expected ']'";
    msgs[ERR_EXP_LPAREN] = "Expected opening parenthesis";
    msgs[ERR_EXP_RPAREN] = "Expected closing parenthesis";
    msgs[ERR_EXP_LCURLY_BRACKET] = "Expected opening curly brackets";
    msgs[ERR_EXP_RCURLY_BRACKET] = "Expected closing curly brackets";
    msgs[ERR_EXP_TYPE] = "Expected Type";
    msgs[ERR_EXP_OP_LT] = "Expected operator <";
    msgs[ERR_EXP_OP_GT] = "Expected operator >";
    msgs[ERR_EXP_REFTYPE] = "Expected reference type";
    msgs[ERR_EXP_CLASS_OR_ARRAY_CREATOR_REST] =
      "Expected ClassCreatorRest or ArrayCreatorRest";
    msgs[ERR_NVAL_ANNOT_ELEM] = "Invalid annotation element";
    msgs[ERR_NVAL_ARRAY] = "Invalid array notation";
    msgs[ERR_NVAL_EXPLICIT_GENERIC_INVOCATION_SUFFIX] =
      "Invalid ExplicitGenericInvocationSuffix";
    msgs[ERR_NVAL_HEX] = "Invalid Hexadecimal";
    msgs[ERR_NVAL_TYPE_ARGUMENT] = "Invalid Type Argument";
  }

  const std::string getMessage(int error) {
    ErrorMap::iterator it = msgs.find(error);
    if (it == msgs.end()) {
      return "";
    }
    return it->second;
  }
};


} // namespace

#endif
