#include "c4/scala/Trees.h"
#include "c4/scala/FlagSets.h"

namespace c4s {

/** Constructor */
Tree::Tree() {}

/**
 * TODO:
 * Can this tree carry attributes (i.e. symbols, types or positions)?
 * Typically the answer is yes, except for the `EmptyTree` null object and
 * two special singletons: `noSelfType` and `pendingSuperCall`.
 */
bool Tree::canHaveAttrs() { return true; }

bool Tree::isEmpty() { return false; }

/** Position of the tree. */
spPosition Tree::pos() {
  // TODO:
  //return rawatt->pos;
}

std::string Tree::toString() {
  // TODO:
  //global->treeToString(this)
  return "TODO";
}

/** Constructor */
PackageDef::PackageDef(spTree pid, std::vector<spTree> stats)
  : Tree(), pid(pid), stats(stats) {}

/** Constructor */
Select::Select(spTree qualifier, spName name)
  : Tree(), qualifier(qualifier), name(name) {}

/** Constructor */
CannotHaveAttrs::CannotHaveAttrs(): Tree() {}

bool CannotHaveAttrs::canHaveAttrs() { return false; }

/** Constructor */
TermTree::TermTree(): Tree() {}

/** Constructor */
EmptyTree::EmptyTree(): TermTree(), CannotHaveAttrs() {}

bool EmptyTree::isEmpty() { return true; }

const spTree EMPTY_TREE = spTree(new EmptyTree());

Ident::Ident(spName name): name(name) {}

/** Constructor */
Traverser::Traverser() {}

void Traverser::traverse(spTree tree) {
  // TODO:
  //itraverse(this, tree)
}

/** Constructor */
//Modifiers::Modifiers()
//  : flags(NoFlags), spName(tpnme.EMPTY), annotations(std::vector<spTree>()) {}

Modifiers::Modifiers(
  long flags, spName privateWithin, std::vector<spTree> annotations)
  : flags(flags), privateWithin(privateWithin), annotations(annotations) {}

} // namespace
