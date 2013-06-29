//-*- C++ -*-
#ifndef __SCALA_LEXER_H__
#define __SCALA_LEXER_H__
#include <memory>
#include "djp/SourceCodeStream.h"

namespace djp {

class ScalaLexer;
typedef std::shared_ptr<ScalaLexer> spScalaLexer;

class ScalaLexer {

  spSourceCodeStream src;

  int curToken;
  std::string curTokenStr;

public:
  ScalaLexer(spSourceCodeStream &src) : src(src) {}

};

} // namespace
#endif
