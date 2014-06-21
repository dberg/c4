//-*- C++ -*-
#ifndef __C4_SCALA_SUB_COMPONENT__H__
#define __C4_SCALA_SUB_COMPONENT__H__

#include <string>

#include "c4/scala/TypeDefs.h"
#include "c4/scala/Global.h"

namespace c4s {

/**
 * Base abstract class of SubComponents.
 */
class SubComponent {

private:
  spGlobal global;

protected:
  std::string phaseName;

public:
  SubComponent(spGlobal &global): global(global) {}
  virtual spPhase newPhase() = 0;
};

} // namespace

#endif
