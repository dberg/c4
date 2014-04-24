//-*- C++ -*-
#ifndef __SCALA_EMACS_OUTPUT_H__
#define __SCALA_EMACS_OUTPUT_H__
#include <memory>
#include <vector>
#include "c4/common/Diagnosis.h"
#include "c4/scala/ScalaIndentation.h"
#include "c4/scala/ScalaAST.h"
#include "c4/scala/ScalaParser.h"
#include "c4/scala/ScalaSyntaxHighlighting.h"

namespace c4s {

class ScalaEmacsOutput {
  spCompilationUnit &compUnit;
  c4::spDiagnosis &diag;
  ScalaLineIndentationMap &indentMap;
  std::vector<spComment> &comments;

  c4::ErrorUtil errUtil;

  void setErrors(const std::vector<c4::spError> &diagErrors);
  void setIndentation();

public:
  std::string sh; // syntax highlighting
  std::string errors;
  std::string indentation;

  // parser.compUnit, parser.comments, parser.diag, parser.indentMap;

  ScalaEmacsOutput(ScalaParser &parser)
    : compUnit(parser.compUnit), diag(parser.diag), indentMap(parser.indentMap),
      comments(parser.comments) {}

  void build();
};

} // namespace

#endif
