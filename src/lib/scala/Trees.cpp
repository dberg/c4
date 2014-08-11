#include "c4/scala/Trees.h"

namespace c4s {

/** Constructor */
Tree::Tree() {}

/**
 * TODO:
 * Can this tree carry attributes (i.e. symbols, types or positions)?
 * Typically the answer is yes, except for the `EmptyTree` null object and
 * two special singletons: `noSelfType` and `pendingSuperCall`.
 */
bool Tree::canHaveAttrs() {
  return true;
}

Ident::Ident(spName name): name(name) {}

/** Constructor */
Traverser::Traverser() {}

void Traverser::traverse(spTree tree) {
  // TODO:
  //itraverse(this, tree)
}

} // namespace
