//-*- C++ -*-
#ifndef __SCALA_EMACS_OUTPUT_H__
#define __SCALA_EMACS_OUTPUT_H__
#include <memory>
#include "c4/Diagnosis.h"
#include "c4/ScalaIndentation.h"
#include "c4/ScalaAST.h"
#include "c4/ScalaSyntaxHighlighting.h"

namespace c4s {

class ScalaEmacsOutput {
  spCompilationUnit &compUnit;
  c4::spDiagnosis &diag;
  ScalaLineIndentationMap &indentMap;

  c4::ErrorUtil errUtil;

  void setErrors(const std::vector<c4::spError> &diagErrors);
  void setIndentation();

public:
  std::string sh; // syntax highlighting
  std::string errors;
  std::string indentation;

  ScalaEmacsOutput(spCompilationUnit &compUnit, c4::spDiagnosis &diag,
    ScalaLineIndentationMap &indentMap)
    : compUnit(compUnit), diag(diag), indentMap(indentMap) {}

  void build();
};

} // namespace

#endif
