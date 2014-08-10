#include "c4/scala/Positions.h"
#include "c4/scala/Position.h"

namespace c4s {

/** Constructor */
Positions::Positions() {}

// TODO:
// Global.scala -> override val useOffsetPositions = !currentSettings.Yrangepos
// ScaladocGlobal.scala -> override val useOffsetPositions = false
// Also, interactive classes disable it.
bool Positions::useOffsetPositions() {
  return true;
}

/**
 * Position a tree.
 * This means: Set position of a node and position all its unpositioned children.
 */
spTree Positions::atPos(spPosition pos, spTree t) {
  // TODO:
  if (useOffsetPositions() || !pos->isOpaqueRange()) {
    // TODO:
  } else {
    // TODO:
  }
}

spPosition Positions::rangePos(
  spSourceFile source, int start, int point, int end)
{
  if (useOffsetPositions()) {
    //return Position.offset(source, point);
  } else {
    //return Position.range(source, start, point, end);
  }
}

} // namespace
