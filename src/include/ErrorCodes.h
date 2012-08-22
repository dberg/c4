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
  ERR_EXP_QID,
  ERR_EXP_IDENTIFIER,
  ERR_EXP_ELEMENT_VALUE,
  ERR_EXP_RBRACKET,
  ERR_EXP_LPAREN,
  ERR_EXP_RPAREN,
  ERR_EXP_TYPE,
  ERR_NVAL_ANNOT_ELEM,
  ERR_NVAL_ARRAY,
};

class ErrorUtil {
  typedef std::map<int, std::string> ErrorMap;
  ErrorMap msgs;

public:
  ErrorUtil() {
    msgs[ERR_EXP_QID] = "Expected qualified id";
    msgs[ERR_EXP_ELEMENT_VALUE] = "Expected element value";
    msgs[ERR_EXP_IDENTIFIER] = "Expected identifier";
    msgs[ERR_EXP_RBRACKET] = "Expected ']'";
    msgs[ERR_EXP_LPAREN] = "Expected opening parenthesis";
    msgs[ERR_EXP_RPAREN] = "Expected closing parenthesis";
    msgs[ERR_EXP_TYPE] = "Expected Type";
    msgs[ERR_NVAL_ANNOT_ELEM] = "Invalid annotation element";
    msgs[ERR_NVAL_ARRAY] = "Invalid array notation";
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
