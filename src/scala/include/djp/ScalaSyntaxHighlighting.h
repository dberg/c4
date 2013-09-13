//-*- C++ -*-
#ifndef __SCALA_SYNTAX_HIGHLIGHTING_H__
#define __SCALA_SYNTAX_HIGHLIGHTING_H__
#include <sstream>
#include "djp/ScalaAST.h"
using namespace djp::scala;

namespace djp {

class ScalaSyntaxHighlighting {
  spCompilationUnit compUnit;

  void setKeyword(spTokenNode tok);

  void setAnnotType(spAnnotType &annotType);
  void setClassParents(spClassParents &classParents);
  void setClassTemplate(spClassTemplate &classTmpl);
  void setClassTemplateOpt(spClassTemplateOpt &classTmplOpt);
  void setConstr(spConstr &constr);
  void setLexId(spLexId &id);
  void setObjectDef(spObjectDef &objectDef);
  void setPackaging(spPackaging &packing);
  void setSimpleType(spSimpleType &simpleType);
  void setTmplDef(spTmplDef &tmplDef);
  void setTopStat(spTopStat &topStat);
  void setTopStatSeq(spTopStatSeq &topStatSeq);

  // Emacs output
  std::stringstream sh;
public:

  ScalaSyntaxHighlighting(spCompilationUnit &compUnit) : compUnit(compUnit) {}
  void build();
  std::string get();
};

} // namespace

#endif
