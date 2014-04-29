//-*- C++ -*-
#ifndef __SCALA_LEXER_H__
#define __SCALA_LEXER_H__
#include <cctype>
#include <memory>
#include <sstream>
#include "c4/common/Diagnosis.h"
#include "c4/common/LiteralSupport.h"
#include "c4/common/SourceCodeStream.h"
#include "c4/scala/AST.h"
#include "c4/scala/Indentation.h"
#include "c4/scala/ParserState.h"
#include "c4/scala/Token.h"

namespace c4s {

class Lexer;
typedef std::shared_ptr<Lexer> spLexer;

STok toScalaTok(c4::LiteralToken litTok);

// Helper functions
bool is_idrest(char c);

class Lexer {

  c4::spSourceCodeStream src;
  c4::spDiagnosis diag;

  // current token information
  STok curTok;
  std::string curTokStr;
  std::stringstream curTokStream;
  int curIndentationLevel;
  bool seenLinebreak;

  TokenUtil tokUtil;
  std::vector<spComment> comments;
  LineIndentationMap &indentMap;
  c4::spLiteralSupport litSupport;

  void clearCurTokenStr();

  STok getCommentOrDivToken();
  STok getEscapeSequence();
  STok getEqualsToken(char c);
  STok getLowerToken(char c);
  STok getUpperToken(char c);
  STok getNumberToken(char c);
  STok getStringLiteral();
  STok getStringLiteralMultiLine();
  STok getToken();

  void processIndentation(unsigned prevLine, unsigned curLine,
    STok prevToken, STok curToken);

  void increaseIndentLevel() { ++curIndentationLevel; }
  void decreaseIndentLevel() { --curIndentationLevel; }

  bool isLineWrap(STok prevToken);

public:

  Lexer(c4::spSourceCodeStream &src, c4::spDiagnosis &diag,
    LineIndentationMap &indentMap)
    : src(src), diag(diag), curIndentationLevel(0), indentMap(indentMap),
      litSupport(c4::spLiteralSupport(new c4::LiteralSupport(src, diag))) {}

  void getNextToken();
  STok getCurToken() { return curTok; }
  int getCurTokenIni();
  int getCurTokenEnd();
  std::string getCurTokenStr() { return curTokStr; }
  spTokenNode getCurTokenNode();

  void saveState(State &state);
  void restoreState(State &state);

  bool getSeenLineBreak() { return seenLinebreak; }

  std::vector<spComment>& getComments() { return comments; }
};

} // namespace
#endif
