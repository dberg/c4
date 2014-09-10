#include "c4/scala/Trees.h"
#include "c4/scala/FlagSets.h"
#include "c4/scala/Global.h"
#include "c4/scala/Printers.h"
#include "c4/scala/Symbols.h"

namespace c4s {

/** Constructor */
Tree::Tree(Global *global): global(global) {}

/** Does this tree represent a definition? (of a method, of a class, etc) */
bool Tree::isDef() { return false; }

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

spSymbol Tree::symbol() {
  // TODO:
  return std::make_shared<Symbol>();
}

std::string Tree::toString() {
  return global->printers->treeToString(this);
}

/** Constructor */
PackageDef::PackageDef(Global *global, spTree pid, std::vector<spTree> stats)
  : Tree(global), pid(pid), stats(stats) {}

/** Constructor */
Select::Select(Global *global, spTree qualifier, spName name)
  : Tree(global), qualifier(qualifier), name(name) {}

/** Constructor */
CannotHaveAttrs::CannotHaveAttrs(Global *global): Tree(global) {}

bool CannotHaveAttrs::canHaveAttrs() { return false; }

/** Constructor */
TermTree::TermTree(Global *global): Tree(global) {}

/** Constructor */
EmptyTree::EmptyTree(Global *global)
  : Tree(global), TermTree(global), CannotHaveAttrs(global) {}

bool EmptyTree::isEmpty() { return true; }

const spTree EMPTY_TREE = spTree(new EmptyTree(nullptr));

Ident::Ident(Global *global, spName name): Tree(global), name(name) {}

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
