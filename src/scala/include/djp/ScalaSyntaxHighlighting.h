//-*- C++ -*-
#ifndef __SCALA_SYNTAX_HIGHLIGHTING_H__
#define __SCALA_SYNTAX_HIGHLIGHTING_H__
#include <sstream>
#include "djp/ScalaAST.h"
using namespace djp::scala;

namespace djp {

class ScalaSyntaxHighlighting {
  spCompilationUnit compUnit;

  void setTopStat(spTopStat &topStat);
  void setTopStatSeq(spTopStatSeq &topStatSeq);

public:
  // Emacs output
  std::stringstream sh;

  ScalaSyntaxHighlighting(spCompilationUnit &compUnit) : compUnit(compUnit) {}
  void build();
};

} // namespace

#endif