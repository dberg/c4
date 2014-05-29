//-*- C++ -*-
#ifndef __C4S_GLOBAL_H__
#define __C4S_GLOBAL_H__

#include <memory>
#include <vector>

#include "c4/common/CompilationUnit.h"
#include "c4/scala/TypeDefs.h"
#include "c4/scala/Phase.h"
#include "c4/scala/SubComponent.h"

namespace c4s {

class Run;
typedef std::shared_ptr<Run> spRun;

class Global {

private:

  spPhase globalPhase;

  /** Each Component is a phase factory */
  std::vector<spSubComponent> phaseDescriptors;

public:

  Global(): globalPhase(spNoPhase(new NoPhase)) {
    // TODO: initialize phaseDescriptors
  }

};

/**
 * Single execution of the compiler on a set of units.
 */
class Run {

private:
  spGlobal global;
  spPhase firstPhase;

public:

  Run(spGlobal global): global(global) {}

  void compileUnits(std::vector<c4::spCompilationUnit> &units);

private:

  /**
   * Add unit to be compiled in this run.
   * Update 'unitbuf' and 'compiledFiles'.
   */
  void addUnit(c4::spCompilationUnit &unit);

};

} // namespace

#endif
