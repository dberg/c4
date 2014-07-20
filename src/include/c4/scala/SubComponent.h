//-*- C++ -*-
#ifndef __C4_SCALA_SUB_COMPONENT__H__
#define __C4_SCALA_SUB_COMPONENT__H__

#include <string>
#include "c4/scala/TypeDefs.h"

namespace c4s {

/** Base abstract class of SubComponents. */
class SubComponent {

protected:
  Global *global;
  std::string phaseName;

public:
  SubComponent(Global *global, std::string phaseName);
  virtual spPhase newPhase() = 0;
};

} // namespace

#endif
