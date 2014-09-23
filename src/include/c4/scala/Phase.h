//-*- C++ -*-
#ifndef __C4_SCALA_PHASE_H__
#define __C4_SCALA_PHASE_H__

#include "c4/scala/CompilationUnits.h"
#include "c4/scala/TypeDefs.h"

namespace c4s {

/** Phase abstract class. */
class Phase {

public:
  virtual ~Phase();
  virtual void run() = 0;
};

class GlobalPhase : public Phase {

protected:
  Global *global;

public:
  GlobalPhase(Global *global);
  virtual ~GlobalPhase();

  virtual void run();

  virtual void apply(CompilationUnit *unit) = 0;
};

class StdPhase : public GlobalPhase {
public:
  StdPhase(Global *global);
  virtual ~StdPhase();
};

class ParserPhase : public StdPhase {
public:
  ParserPhase(Global *global);
  virtual ~ParserPhase();
  virtual void apply(CompilationUnit *unit);
};

} // namespace

#endif
