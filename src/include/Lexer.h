//-*- C++ -*-
#ifndef __LEXER_H__
#define __LEXER_H__
#include <cctype>
#include <string>
#include <sstream>
#include <boost/shared_ptr.hpp>
#include "Diagnosis.h"
#include "LiteralSupport.h"
#include "SourceCodeStream.h"
#include "Token.h"

namespace djp {

// Current State of the Lexer. We use this to perform lookaheads and restore
// or consult the previous state if necessary.
struct State {
  int cursor, line, token;
  std::string tokenStr;
};

// Helpers
bool isBinaryDigit(char c);
bool isDecimalDigit(char c);
bool isHexDigit(char c);
bool isOctalDigit(char c);
bool isJavaLetter(char c);
bool isJavaLetterOrDigit(char c);

class Lexer;
typedef boost::shared_ptr<Lexer> spLexer;

class Lexer {

  int curToken;
  std::string curTokenStr;
  spDiagnosis diag;
  spSourceCodeStream src;
  spLiteralSupport litSupport;
  TokenUtil tokenUtil;

  int getToken();
  int getAnnotationToken();
  int getCharacterLiteral();
  int getEqualsOrEqualsEqualsToken();
  int getEscapeSequence(std::stringstream &ss);
  int getMinusOrMinusMinusToken();
  int getNumberToken(char c);
  int getPeriodOrEllipsisToken(std::stringstream &ss);
  int getPeriodStartingToken();
  int getPlusOrPlusPlusToken();
  int getStringLiteral();
  int getTokenIdentifier(char c);

public:
  Lexer(spSourceCodeStream &src, spDiagnosis &diag)
    : curToken(0), curTokenStr(""), diag(diag), src(src),
      litSupport(spLiteralSupport(new LiteralSupport(src, diag))) {}

  void getNextToken();
  int getCurToken() { return curToken; }
  unsigned int getCursor() { return src->getCursor(); }
  const std::string getCurTokenStr() { return curTokenStr; }
  unsigned int getCurTokenIni() {
    return src->getCursor() - curTokenStr.length();
  }

  void saveState(State &state);
  void restoreState(State &state);
};
} // namespace

#endif
