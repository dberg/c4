//-*- C++ -*-
#ifndef __C4S_PHASE_H__
#define __C4S_PHASE_H__

#include <memory>
#include "c4/common/CompilationUnit.h"
#include "c4/scala/TypeDefs.h"

namespace c4s {

/**
 * Phase abstract class.
 */
class Phase {

private:
  spPhase prev;

public:
  Phase(spPhase prev): prev(prev) {}
  virtual void run() = 0;
  virtual bool hasNext() { /** TODO **/ return false; }
};

/**
 * Initial Phase marker.
 */
class NoPhase : public Phase {

public:
  NoPhase(): Phase(nullptr) {}
  virtual void run();
};

class GlobalPhase : public Phase {
private:
  virtual void applyPhase(c4::spCompilationUnit &unit);

public:
  GlobalPhase(spPhase prev): Phase(prev) {}
  virtual void run();
  virtual void apply(c4::spCompilationUnit &unit) = 0;
};

class ParserPhase : public GlobalPhase {
public:
  ParserPhase(spPhase prev): GlobalPhase(prev) {}
  virtual void apply(c4::spCompilationUnit &unit);
};

} // namespace

#endif
