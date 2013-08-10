//-*- C++ -*-
#ifndef __SCALA_LEXER_H__
#define __SCALA_LEXER_H__
#include <cctype>
#include <memory>
#include <sstream>
#include "djp/Diagnosis.h"
#include "djp/Indentation.h"
#include "djp/LiteralSupport.h"
#include "djp/SourceCodeStream.h"
#include "ScalaAST.h"
#include "ScalaParserState.h"
#include "ScalaToken.h"
using namespace djp::scala;

namespace djp {

class ScalaLexer;
typedef std::shared_ptr<ScalaLexer> spScalaLexer;

// Helper functions
bool is_idrest(char c);

class ScalaLexer {

  spSourceCodeStream src;
  spDiagnosis diag;

  STok curTok;
  std::string curTokStr;
  std::stringstream curTokStream;

  ScalaTokenUtil tokUtil;

  LineIndentationMap &indentMap;

  STok getEscapeSequence();
  STok getLowerToken(char c);
  STok getUpperToken(char c);
  STok getStringLiteral();
  STok getStringLiteralMultiLine();
  STok getToken();

public:
  ScalaLexer(spSourceCodeStream &src, spDiagnosis &diag,
    LineIndentationMap &indentMap)
    : src(src), diag(diag), indentMap(indentMap) {}

  void getNextToken();
  STok getCurToken() { return curTok; }
  int getCurTokenIni();
  int getCurTokenEnd();
  std::string getCurTokenStr() { return curTokStr; }
  spTokenNode getCurTokenNode();

  void saveState(State &state);
  void restoreState(State &state);
};

} // namespace
#endif
