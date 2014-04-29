//-*- C++ -*-
#ifndef __SCALA_LEXER_H__
#define __SCALA_LEXER_H__
#include <cctype>
#include <memory>
#include <sstream>
#include "c4/common/Diagnosis.h"
#include "c4/common/LiteralSupport.h"
#include "c4/common/SourceCodeStream.h"
#include "c4/scala/ScalaIndentation.h"
#include "c4/scala/AST.h"
#include "c4/scala/ScalaParserState.h"
#include "c4/scala/ScalaToken.h"

namespace c4s {

class ScalaLexer;
typedef std::shared_ptr<ScalaLexer> spScalaLexer;

STok toScalaTok(c4::LiteralToken litTok);

// Helper functions
bool is_idrest(char c);

class ScalaLexer {

  c4::spSourceCodeStream src;
  c4::spDiagnosis diag;

  // current token information
  STok curTok;
  std::string curTokStr;
  std::stringstream curTokStream;
  int curIndentationLevel;
  bool seenLinebreak;

  ScalaTokenUtil tokUtil;
  std::vector<spComment> comments;
  ScalaLineIndentationMap &indentMap;
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

  ScalaLexer(c4::spSourceCodeStream &src, c4::spDiagnosis &diag,
    ScalaLineIndentationMap &indentMap)
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
