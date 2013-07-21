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

  spLexId parseLexId();

  void buildParseTree();
  void parseAnnotType(spAnnotType &annotType);
  void parseClassParents(spClassParents &classParents);
  void parseClassTemplate(spClassTemplate &classTmpl);
  void parseClassTemplateOpt(spClassTemplateOpt &classTmplOpt);
  void parseCompilationUnit();
  void parseConstr(spConstr &constr);
  void parseObjectDef(spObjectDef &objectDef);
  void parseSimpleType(spSimpleType &simpleType);
  void parseStableId(spStableId &stableId);
  void parseTemplateBody(spTemplateBody &tmplBody);
  void parseTmplDef(spTmplDef &tmplDef);
  void parseTopStat(spTopStat &topStat);
  void parseTopStatSeq(spTopStatSeq &topStatSeq);

  int addErr(int err);

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
