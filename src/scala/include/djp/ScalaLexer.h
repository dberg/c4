//-*- C++ -*-
#ifndef __SCALA_LEXER_H__
#define __SCALA_LEXER_H__
#include <cctype>
#include <memory>
#include <sstream>
#include "djp/SourceCodeStream.h"
#include "djp/Diagnosis.h"
#include "ScalaToken.h"

namespace djp {

class ScalaLexer;
typedef std::shared_ptr<ScalaLexer> spScalaLexer;

class ScalaLexer {

  spSourceCodeStream src;
  spDiagnosis diag;

  STok curTok;
  std::string curTokStr;
  std::stringstream curTokStream;

  ScalaTokenUtil tokUtil;

  STok getLowerToken(char c);
  STok getToken();

public:
  ScalaLexer(spSourceCodeStream &src, spDiagnosis diag)
    : src(src), diag(diag) {}

  void getNextToken();
  STok getCurToken() { return curTok; }

};

} // namespace
#endif
