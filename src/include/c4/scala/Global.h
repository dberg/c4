//-*- C++ -*-
#ifndef __C4_SCALA_GLOBAL_H__
#define __C4_SCALA_GLOBAL_H__

#include <memory>
#include <vector>

#include "c4/scala/TypeDefs.h"
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/Phase.h"
#include "c4/scala/SubComponent.h"
#include "c4/scala/SyntaxAnalyzer.h"

namespace c4s {

class Global {

protected:

  /** The current phase being run */
  spPhase globalPhase;

  /** The phases to be applied in order */
  std::vector<spPhase> phases;

  /** Each Component is a phase factory */
  // TODO: std::vector<spSubComponent> phaseDescriptors;

public:

  /** Compilation units to be compiled */
  std::vector<spCompilationUnit> units;

  Global();

  void compile(std::vector<spCompilationUnit> units);
};

} // namespace

#endif
