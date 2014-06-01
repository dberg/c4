//-*- C++ -*-
#ifndef __C4S_PHASE_H__
#define __C4S_PHASE_H__

#include <memory>
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
public:
  GlobalPhase(spPhase prev): Phase(prev) {}
  virtual void run();
};

class ParserPhase : public GlobalPhase {
public:
  ParserPhase(spPhase prev): GlobalPhase(prev) {}
};

} // namespace

#endif
