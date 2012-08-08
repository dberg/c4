//-*- C++ -*-
#ifndef __LEXER_H__
#define __LEXER_H__
#include <cctype>
#include <string>
#include <sstream>
#include "Token.h"

namespace djp {

// Current State of the Lexer. We use this to perform lookaheads and restore
// or consult the previous state if necessary.
struct State {
  int cursor, line, token;
  std::string tokenStr;
};

class Lexer {

  const std::string buffer;
  unsigned int cursor;
  unsigned int line;

  int curToken;
  std::string curTokenStr;

  const char getChar();
  const char ungetChar(int count);

  TokenUtil tokenUtil;

  bool lookaheadInterface(int point);

  int getToken();
  int getAnnotationToken();
  int getEqualsOrEqualsEqualsToken();
  int getMinusOrMinusMinusToken();
  int getNumberToken(char c);
  int getPeriodOrEllipsisToken();
  int getPlusOrPlusPlusToken();
  int getTokenIdentifier(char c);

  bool isJavaLetter(char c);
  bool isJavaLetterOrDigit(char c);

public:
  Lexer(const std::string &_buffer)
    : buffer(_buffer), cursor(0), line(0), curToken(0) {}

  void getNextToken();

  int getCurToken() { return curToken; }
  unsigned int getCursor() { return cursor; }
  unsigned int getLine() { return line; }
  const std::string getCurTokenStr() { return curTokenStr; }
  unsigned int getCurTokenIni() { return cursor - curTokenStr.length(); }

  void saveState(State &state);
  void restoreState(State &state);
};
} // namespace

#endif
