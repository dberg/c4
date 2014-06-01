//-*- C++ -*-
#ifndef __C4S_SYNTAX_ANALYZER_H__
#define __C4S_SYNTAX_ANALYZER_H__

#include <string>

#include "c4/scala/TypeDefs.h"
#include "c4/scala/Global.h"
#include "c4/scala/Phase.h"
#include "c4/scala/SubComponent.h"
#include "c4/scala/SyntaxAnalyzer.h"

namespace c4s {

class SyntaxAnalyzer : public SubComponent {

protected:
  std::string phaseName;

public:
  SyntaxAnalyzer(spGlobal global): SubComponent(global), phaseName("parser") {}
  virtual spPhase newPhase(spPhase prev);
};

} // namespace

#endif
