//-*- C++ -*-
#ifndef __C4_SCALA_SYNTAX_ANALYZER_H__
#define __C4_SCALA_SYNTAX_ANALYZER_H__

#include "c4/scala/TypeDefs.h"
#include "c4/scala/SubComponent.h"

namespace c4s {

class SyntaxAnalyzer : public SubComponent {
public:
  SyntaxAnalyzer(Global *global);
  virtual spPhase newPhase();
};

} // namespace

#endif
