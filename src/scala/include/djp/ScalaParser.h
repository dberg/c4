//-*- C++ -*-
#ifndef __SCALA_PARSER_H__
#define __SCALA_PARSER_H__
#include <string>
#include "djp/SourceCodeStream.h"
#include "djp/Diagnosis.h"
#include "ScalaAST.h"
#include "ScalaLexer.h"
#include "ScalaToken.h"
using namespace djp::scala;

namespace djp {

class ScalaParser {
  const std::string filename;
  spSourceCodeStream src;
  spScalaLexer lexer;

  void buildParseTree();
  void parseCompilationUnit();
  void parseObjectDef(spObjectDef &objectDef);
  void parseTmplDef(spTmplDef &tmplDef);
  void parseTopStat(spTopStat &topStat);
  void parseTopStatSeq(spTopStatSeq &topStatSeq);

public:
  ScalaParser(const std::string filename, const std::string &buffer)
    : filename(filename), src(spSourceCodeStream(new SourceCodeStream(buffer))),
      compUnit(spCompilationUnit(new CompilationUnit)), error(0)
  {
    lexer = spScalaLexer(new ScalaLexer(src, diag));
  }

  spDiagnosis diag;
  spCompilationUnit compUnit;
  int error;
  std::string error_msg;

  void parse();
};

} // namespace

#endif
