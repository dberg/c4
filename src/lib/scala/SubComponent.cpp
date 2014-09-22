#include "c4/scala/SubComponent.h"

namespace c4s {

/** Constructor */
SubComponent::SubComponent(Global *global, std::string phaseName)
  : global(global), phaseName(phaseName) {}

/** Destructor */
SubComponent::~SubComponent() {}

} // namespace
