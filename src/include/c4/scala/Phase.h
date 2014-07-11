//-*- C++ -*-
#ifndef __C4_SCALA_PHASE_H__
#define __C4_SCALA_PHASE_H__

#include <memory>

#include "c4/scala/CompilationUnits.h"
#include "c4/scala/Global.h"
#include "c4/scala/Parsers.h"
#include "c4/scala/TypeDefs.h"

namespace c4s {

/** Phase abstract class. */
class Phase {

public:
  virtual void run() = 0;
};

class GlobalPhase : public Phase {

protected:
  Global *global;

public:
  GlobalPhase(Global *global) : global(global) {}

  virtual void run();

  virtual void apply(spCompilationUnit unit) = 0;
};

class StdPhase : public GlobalPhase {
public:
  StdPhase(Global *global) : GlobalPhase(global) {}
};

class ParserPhase : public StdPhase {
public:
  ParserPhase(Global *global) : StdPhase(global) {}
  virtual void apply(spCompilationUnit unit);
};

} // namespace

#endif
