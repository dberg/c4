#include <cstdlib>
#include <iostream>
#include "c4/scala/Position.h"

namespace c4s {

/** Constructor */
Position::Position() {}

bool Position::isOpaqueRange() {
  // TODOO:
  //isRange() && !isTransparent;
  return false;
}

int Position::point() {
  std::cerr << "UnsupportedOperation: Position point" << std::endl;
  std::abort();
}

int Position::start() {
  std::cerr << "UnsupportedOperation: Position start" << std::endl;
  std::abort();
}

Position* Position::validate(Position* pos) {
  // TODO:
  //if (pos.isRange)
  //  assert(pos.start <= pos.end, s"bad position: ${pos.show}")
  return pos;
}

Position* Position::offset(SourceFile *source, int point) {
  return validate(new OffsetPosition(source, point));
}

/** Constructor */
DefinedPosition::DefinedPosition(): Position() {}

/** Constructor */
OffsetPosition::OffsetPosition(SourceFile *sourceIn, int pointIn)
  : DefinedPosition(), sourceIn(sourceIn), pointIn(pointIn) {}

int OffsetPosition::point() {
  return pointIn;
}

int OffsetPosition::start() {
  return point();
}

/** Constructor */
UndefinedPosition::UndefinedPosition() {}

const Position* NoPosition = new UndefinedPosition();

} // namespace
