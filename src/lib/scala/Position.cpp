#include "c4/scala/Position.h"

namespace c4s {

/** Constructor */
Position::Position() {}

bool Position::isOpaqueRange() {
  // TODOO:
  //isRange() && !isTransparent;
  return false;
}

/** Constructor */
UndefinedPosition::UndefinedPosition() {}

/** Constructor */
NoPosition::NoPosition(): UndefinedPosition() {}

} // namespace
