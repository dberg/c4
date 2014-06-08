//-*- C++ -*-
#ifndef __C4S_GLOBAL_H__
#define __C4S_GLOBAL_H__

#include <memory>
#include <vector>

#include "c4/scala/TypeDefs.h"
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/Phase.h"
#include "c4/scala/SubComponent.h"

namespace c4s {

class Global {

protected:

  /** The current phase being run */
  spPhase globalPhase;

  /** The phases to be applied in order */
  std::vector<spPhase> phases;

  /** Each Component is a phase factory */
  std::vector<spSubComponent> phaseDescriptors;

public:

  /** Compilation units to be compiled */
  std::vector<spCompilationUnit> units;

  Global(): globalPhase(spNoPhase(new NoPhase)) {
    // TODO: initialize phaseDescriptors
  }

  void compile(std::vector<spCompilationUnit> &units);
};

} // namespace

#endif
