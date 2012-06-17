//-*- C++ -*-
#ifndef __ERROR_CODES__
#define __ERROR_CODES__

namespace djp {

enum ErrorCode {
  ERR_EXP_QID,
  ERR_EXP_IDENTIFIER,
  ERR_EXP_LPAREN,
  ERR_EXP_RPAREN,
  ERR_NVAL_ANNOT_ELEM,
};

class ErrorUtil {
  typedef std::map<int, std::string> ErrorMap;
  ErrorMap msgs;

public:
  ErrorUtil() {
    msgs[ERR_EXP_QID] = "Expected qualified id";
    msgs[ERR_EXP_IDENTIFIER] = "Expected identifier";
    msgs[ERR_EXP_LPAREN] = "Expected opening parenthesis";
    msgs[ERR_EXP_RPAREN] = "Expected closing parenthesis";
    msgs[ERR_NVAL_ANNOT_ELEM] = "Invalid annotation element";
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
