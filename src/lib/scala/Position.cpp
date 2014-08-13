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

static spPosition NoPosition = spPosition(new UndefinedPosition());

} // namespace
