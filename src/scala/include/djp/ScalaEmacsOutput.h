//-*- C++ -*-
#ifndef __SCALA_EMACS_OUTPUT_H__
#define __SCALA_EMACS_OUTPUT_H__
#include "djp/Diagnosis.h"
#include "djp/ScalaIndentation.h"
#include "djp/ScalaAST.h"
#include "djp/ScalaSyntaxHighlighting.h"
using namespace djp::scala;

namespace djp {

class ScalaEmacsOutput {
  spCompilationUnit compUnit;
  spDiagnosis diag;
  ScalaLineIndentationMap &indentMap;

public:
  std::string sh; // syntax highlighting

  ScalaEmacsOutput(spCompilationUnit &compUnit, spDiagnosis &diag,
    ScalaLineIndentationMap &indentMap)
    : compUnit(compUnit), diag(diag), indentMap(indentMap) {}

  void build();
};

} // namespace

#endif