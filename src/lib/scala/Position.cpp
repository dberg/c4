#include "c4/scala/Position.h"

namespace c4s {

/** Constructor */
Position::Position() {}

bool Position::isOpaqueRange() {
  // TODOO:
  //isRange() && !isTransparent;
  return false;
}

spPosition Position::validate(spPosition pos) {
  // TODO:
  //if (pos.isRange)
  //  assert(pos.start <= pos.end, s"bad position: ${pos.show}")
  return pos;
}

spPosition Position::offset(spSourceFile source, int point) {
  return validate(spPosition(new OffsetPosition(source, point)));
}

/** Constructor */
DefinedPosition::DefinedPosition(): Position() {}

/** Constructor */
OffsetPosition::OffsetPosition(spSourceFile sourceIn, int pointIn)
  : DefinedPosition(), sourceIn(sourceIn), pointIn(pointIn) {}

/** Constructor */
UndefinedPosition::UndefinedPosition() {}

static spPosition NoPosition = spPosition(new UndefinedPosition());

} // namespace
