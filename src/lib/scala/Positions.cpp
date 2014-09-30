#include "c4/scala/Positions.h"
#include "c4/scala/Position.h"

namespace c4s {

/** Constructor */
PosAssigner::PosAssigner(): Traverser() {}

/** Destructor */
PosAssigner::~PosAssigner() {}

/** Constructor */
DefaultPosAssigner::DefaultPosAssigner(): PosAssigner() {}

/** Destructor */
DefaultPosAssigner::~DefaultPosAssigner() {}

void DefaultPosAssigner::traverse(Tree* tree) {
  if (tree->canHaveAttrs() && tree->pos() == NoPosition) {
    // TODO:
    //tree.setPos(pos);
    //super.traverse(tree);
  }
}

/** Constructor */
Positions::Positions(): posAssigner(new DefaultPosAssigner) {}

/** Destructor */
Positions::~Positions() {
  delete posAssigner;
}

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
Tree* Positions::atPos(Position* pos, Tree* tree) {
  if (useOffsetPositions() || !pos->isOpaqueRange()) {
    posAssigner->pos = pos;
    posAssigner->traverse(tree);
    return tree;
  } else {
    // TODO:
    return tree;
  }
}

Position* Positions::rangePos(
  SourceFile *source, int start, int point, int end)
{
  if (useOffsetPositions()) {
    //return Position.offset(source, point);
  } else {
    //return Position.range(source, start, point, end);
  }
}

} // namespace
