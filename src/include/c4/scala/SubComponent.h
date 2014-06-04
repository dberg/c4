//-*- C++ -*-
#ifndef __C4S_SUB_COMPONENT__H__
#define __C4S_SUB_COMPONENT__H__

#include <string>

#include "c4/scala/TypeDefs.h"
#include "c4/scala/Global.h"

namespace c4s {

/**
 * Base abstract class of SubComponents.
 */
class SubComponent {

private:
  Global *global;

protected:
  std::string phaseName;

public:
  SubComponent(Global *global): global(global) {}
  virtual Phase* newPhase(Phase* prev) = 0;
};

} // namespace

#endif
