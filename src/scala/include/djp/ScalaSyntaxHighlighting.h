//-*- C++ -*-
#ifndef __SCALA_SYNTAX_HIGHLIGHTING_H__
#define __SCALA_SYNTAX_HIGHLIGHTING_H__
#include "djp/ScalaAST.h"
using namespace djp::scala;

namespace djp {

class ScalaSyntaxHighlighting {
  spCompilationUnit compUnit;

public:
  ScalaSyntaxHighlighting(spCompilationUnit &compUnit) : compUnit(compUnit) {}
  void build();
};

} // namespace

#endif