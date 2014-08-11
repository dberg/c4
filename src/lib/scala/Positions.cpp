#include "c4/scala/Positions.h"
#include "c4/scala/Position.h"

namespace c4s {

/** Constructor */
PosAssigner::PosAssigner(): Traverser() {}

/** Constructor */
DefaultPosAssigner::DefaultPosAssigner(): PosAssigner() {}

void DefaultPosAssigner::traverse(spTree tree) {
  // TODO:
  //if (t.canHaveAttrs() && t->pos == NoPosition) {
  //
  //}
}

/** Constructor */
Positions::Positions(): posAssigner(spPosAssigner(new DefaultPosAssigner)) {}

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
spTree Positions::atPos(spPosition pos, spTree tree) {
  if (useOffsetPositions() || !pos->isOpaqueRange()) {
    posAssigner->pos = pos;
    posAssigner->traverse(tree);
    return tree;
  } else {
    // TODO:
    return tree;
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
