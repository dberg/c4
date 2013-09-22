//-*- C++ -*-
#ifndef __SCALA_LEXER_H__
#define __SCALA_LEXER_H__
#include <cctype>
#include <memory>
#include <sstream>
#include "c4/Diagnosis.h"
#include "c4/ScalaIndentation.h"
#include "c4/LiteralSupport.h"
#include "c4/SourceCodeStream.h"
#include "ScalaAST.h"
#include "ScalaParserState.h"
#include "ScalaToken.h"
using namespace c4::scala;

namespace c4 {

class ScalaLexer;
typedef std::shared_ptr<ScalaLexer> spScalaLexer;

// Helper functions
bool is_idrest(char c);

class ScalaLexer {

  spSourceCodeStream src;
  spDiagnosis diag;

  // current token information
  STok curTok;
  std::string curTokStr;
  std::stringstream curTokStream;
  int curIndentationLevel;
  bool seenLinebreak;

  ScalaTokenUtil tokUtil;

  ScalaLineIndentationMap &indentMap;

  STok getEscapeSequence();
  STok getLowerToken(char c);
  STok getUpperToken(char c);
  STok getStringLiteral();
  STok getStringLiteralMultiLine();
  STok getToken();

  void processIndentation(unsigned prevLine, unsigned curLine,
    STok prevToken, STok curToken);

  void increaseIndentLevel() { ++curIndentationLevel; }
  void decreaseIndentLevel() { --curIndentationLevel; }

  bool isLineWrap(STok prevToken);

public:

  ScalaLexer(spSourceCodeStream &src, spDiagnosis &diag,
    ScalaLineIndentationMap &indentMap)
    : src(src), diag(diag), curIndentationLevel(0), indentMap(indentMap) {}

  void getNextToken();
  STok getCurToken() { return curTok; }
  int getCurTokenIni();
  int getCurTokenEnd();
  std::string getCurTokenStr() { return curTokStr; }
  spTokenNode getCurTokenNode();

  void saveState(State &state);
  void restoreState(State &state);

  bool getSeenLineBreak() { return seenLinebreak; }
};

} // namespace
#endif
