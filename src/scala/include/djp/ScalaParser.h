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

  void buildParseTree();

  void parseCompilationUnit();

public:
  ScalaParser(const std::string filename, const std::string &buffer)
    : filename(filename), src(spSourceCodeStream(new SourceCodeStream(buffer))),
      error(0)
  {
    lexer = spScalaLexer(new ScalaLexer(src));
  }

  int error;
  std::string error_msg;

  void parse();
};

} // namespace

#endif
