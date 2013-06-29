//-*- C++ -*-
#ifndef __SCALA_PARSER_H__
#define __SCALA_PARSER_H__
#include <string>
#include "djp/SourceCodeStream.h"
#include "ScalaLexer.h"

namespace djp {

class ScalaParser {
  const std::string filename;
  spSourceCodeStream src;
  spScalaLexer lexer;

public:
  ScalaParser(const std::string filename, const std::string &buffer)
    : filename(filename), src(spSourceCodeStream(new SourceCodeStream(buffer)))
  {
    lexer = spScalaLexer(new ScalaLexer(src));
  }
};

} // namespace

#endif
