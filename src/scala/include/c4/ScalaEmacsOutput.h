//-*- C++ -*-
#ifndef __SCALA_EMACS_OUTPUT_H__
#define __SCALA_EMACS_OUTPUT_H__
#include <memory>
#include "c4/Diagnosis.h"
#include "c4/ScalaIndentation.h"
#include "c4/ScalaAST.h"
#include "c4/ScalaSyntaxHighlighting.h"
using namespace c4::scala;

namespace c4 {

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
