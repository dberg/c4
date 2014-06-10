//-*- C++ -*-
#ifndef __C4S_PHASE_H__
#define __C4S_PHASE_H__

#include <memory>
#include "c4/scala/TypeDefs.h"
#include "c4/scala/Global.h"
#include "c4/scala/CompilationUnits.h"

namespace c4s {

/** Phase abstract class. */
class Phase {

public:
  virtual void run() = 0;
};

class GlobalPhase : public Phase {

public:
  spGlobal global;

  GlobalPhase(spGlobal &global) : global(global) {}

  virtual void run();

  virtual void apply(spCompilationUnit &unit) = 0;
};

class StdPhase : public GlobalPhase {
public:
  StdPhase(spGlobal &global) : GlobalPhase(global) {}
};

class ParserPhase : public StdPhase {
public:
  ParserPhase(spGlobal &global) : StdPhase(global) {}
  virtual void apply(spCompilationUnit &unit);
};

} // namespace

#endif
