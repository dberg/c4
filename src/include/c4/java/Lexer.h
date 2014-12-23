//-*- C++ -*-
#ifndef __C4_JAVA_LEXER_H__
#define __C4_JAVA_LEXER_H__

#include <cctype>
#include <string>
#include <sstream>
#include <memory>

#include "c4/java/AST.h"
#include "c4/java/Diagnosis.h"
#include "c4/java/Indentation.h"
#include "c4/java/LiteralSupport.h"
#include "c4/java/ParserState.h"
#include "c4/java/SourceCodeStream.h"
#include "c4/java/Token.h"

using std::map;
using std::shared_ptr;
using std::string;
using std::stringstream;
using std::vector;

namespace c4j {

int toJavaTok(c4::LiteralToken litTok);
bool isJavaLetter(char c);
bool isJavaLetterOrDigit(char c);

class Lexer;
typedef shared_ptr<Lexer> spLexer;

class Lexer {

  int curToken;
  string curTokenStr;
  bool isPrevTokenSwitchLabelColon;
  int curIndentationLevel;
  c4::spDiagnosis diag;
  c4::spSourceCodeStream src;
  c4::spLiteralSupport litSupport;
  TokenUtil tokenUtil;
  vector<spComment> comments;
  LineIndentationMap &indentMap;

  int getToken();
  int getAmpersandToken();
  int getAnnotationToken();
  int getCarretToken();
  int getCharacterLiteral();
  int getCommentOrDivToken();
  int getEqualsToken();
  int getEscapeSequence(stringstream &ss);
  int getExclamationToken();
  int getGreaterThenToken();
  int getLessThenToken();
  int getMinusToken();
  int getNumberToken(char c);
  int getMulToken();
  int getPeriodOrEllipsisToken(stringstream &ss);
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
  Lexer(c4::spSourceCodeStream &src, c4::spDiagnosis &diag,
    LineIndentationMap &indentMap)
    : curToken(0), curTokenStr(""), isPrevTokenSwitchLabelColon(false),
      curIndentationLevel(0), diag(diag), src(src),
      litSupport(c4::spLiteralSupport(new c4::LiteralSupport(src, diag))),
      indentMap(indentMap) {}

  void getNextToken();
  int getCurToken() { return curToken; }
  unsigned int getCursor() { return src->getCursor(); }
  const string getCurTokenStr() { return curTokenStr; }
  unsigned int getCurTokenIni() {
    return src->getCursor() - curTokenStr.length();
  }

  vector<spComment>& getComments() { return comments; }

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
