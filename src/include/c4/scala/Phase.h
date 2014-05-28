//-*- C++ -*-
#ifndef __C4S_PHASE_H__
#define __C4S_PHASE_H__

#include <memory>

namespace c4s {

class Phase;
typedef std::shared_ptr<Phase> spPhase;

class NoPhase;
typedef std::shared_ptr<NoPhase> spNoPhase;

class Phase {

private:
  spPhase prev;

public:
  Phase(spPhase prev): prev(prev) {}
  virtual void run() = 0;
};

class NoPhase : public Phase {

public:
  NoPhase(): Phase(nullptr) {}
  virtual void run();
};

} // namespace

#endif
