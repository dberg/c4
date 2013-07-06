//-*- C++ -*-
#ifndef __LEXER_H__
#define __LEXER_H__
#include <cctype>
#include <string>
#include <sstream>
#include <memory>
#include "djp/SourceCodeStream.h"
#include "djp/Diagnosis.h"
#include "AST.h"
#include "Indentation.h"
#include "LiteralSupport.h"
#include "ParserState.h"
#include "Token.h"

namespace djp {

// Helpers
bool isBinaryDigit(char c);
bool isDecimalDigit(char c);
bool isHexDigit(char c);
bool isOctalDigit(char c);
bool isJavaLetter(char c);
bool isJavaLetterOrDigit(char c);

class Lexer;
typedef std::shared_ptr<Lexer> spLexer;

class Lexer {

  int curToken;
  std::string curTokenStr;
  bool isPrevTokenSwitchLabelColon;
  int curIndentationLevel;
  spDiagnosis diag;
  spSourceCodeStream src;
  spLiteralSupport litSupport;
  TokenUtil tokenUtil;
  std::vector<spComment> comments;
  LineIndentationMap &indentMap;

  int getToken();
  int getAmpersandToken();
  int getAnnotationToken();
  int getCarretToken();
  int getCharacterLiteral();
  int getCommentOrDivToken();
  int getEqualsToken();
  int getEscapeSequence(std::stringstream &ss);
  int getExclamationToken();
  int getGreaterThenToken();
  int getLessThenToken();
  int getMinusToken();
  int getNumberToken(char c);
  int getMulToken();
  int getPeriodOrEllipsisToken(std::stringstream &ss);
  int getPeriodStartingToken();
  int getPipeToken();
  int getPlusToken();
  int getRemToken();
  int getStringLiteral();
  int getTokenIdentifier(char c);
  void processIndentation(unsigned prevLine, unsigned curLine,
    int prevToken, int curToken);
  bool isLineWrap(int prevToken);

public:
  Lexer(spSourceCodeStream &src, spDiagnosis &diag,
    LineIndentationMap &indentMap)
    : curToken(0), curTokenStr(""), isPrevTokenSwitchLabelColon(false),
      curIndentationLevel(0), diag(diag), src(src),
      litSupport(spLiteralSupport(new LiteralSupport(src, diag))),
      indentMap(indentMap) {}

  void getNextToken();
  int getCurToken() { return curToken; }
  unsigned int getCursor() { return src->getCursor(); }
  const std::string getCurTokenStr() { return curTokenStr; }
  unsigned int getCurTokenIni() {
    return src->getCursor() - curTokenStr.length();
  }

  std::vector<spComment>& getComments() { return comments; }

  void saveState(State &state);
  void restoreState(State &state);

  void adjustClosingCurlyBracketIndentation();
  void increaseIndentLevel() { ++curIndentationLevel; }
  void decreaseIndentLevel() { --curIndentationLevel; }

  void setPrevTokenSwitchLabelColon(bool isColonToken) {
    isPrevTokenSwitchLabelColon = isColonToken;
  }
};
} // namespace

#endif
