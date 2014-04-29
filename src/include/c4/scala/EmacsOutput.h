//-*- C++ -*-
#ifndef __SCALA_EMACS_OUTPUT_H__
#define __SCALA_EMACS_OUTPUT_H__
#include <memory>
#include <vector>
#include "c4/common/Diagnosis.h"
#include "c4/scala/AST.h"
#include "c4/scala/Indentation.h"
#include "c4/scala/Parser.h"
#include "c4/scala/SyntaxHighlighting.h"

namespace c4s {

class EmacsOutput {
  spCompilationUnit &compUnit;
  c4::spDiagnosis &diag;
  LineIndentationMap &indentMap;
  std::vector<spComment> &comments;

  c4::ErrorUtil errUtil;

  void setErrors(const std::vector<c4::spError> &diagErrors);
  void setIndentation();

public:
  std::string sh; // syntax highlighting
  std::string errors;
  std::string indentation;

  // parser.compUnit, parser.comments, parser.diag, parser.indentMap;

  EmacsOutput(Parser &parser)
    : compUnit(parser.compUnit), diag(parser.diag), indentMap(parser.indentMap),
      comments(parser.comments) {}

  void build();
};

} // namespace

#endif
