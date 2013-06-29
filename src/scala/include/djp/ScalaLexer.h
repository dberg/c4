//-*- C++ -*-
#ifndef __SCALA_LEXER_H__
#define __SCALA_LEXER_H__
#include <memory>
#include "djp/SourceCodeStream.h"
#include "ScalaToken.h"

namespace djp {

class ScalaLexer;
typedef std::shared_ptr<ScalaLexer> spScalaLexer;

class ScalaLexer {

  spSourceCodeStream src;

  STok curToken;
  std::string curTokenStr;

  STok getToken();

public:
  ScalaLexer(spSourceCodeStream &src)
    : src(src) {}

  void getNextToken();
  STok getCurToken() { return curToken; }

};

} // namespace
#endif
