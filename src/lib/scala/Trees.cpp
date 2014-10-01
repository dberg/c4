#include "c4/scala/Trees.h"
#include "c4/scala/FlagSets.h"
#include "c4/scala/Global.h"
#include "c4/scala/Printers.h"
#include "c4/scala/Symbols.h"

namespace c4s {

/** Constructor */
Tree::Tree(Global* global): global(global) {}

/** Destructor */
Tree::~Tree() {}

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
Position* Tree::pos() {
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
PackageDef::PackageDef(Global* global, Tree* pid, std::vector<Tree*> stats)
  : Tree(global), pid(pid), stats(stats) {}

PackageDef::~PackageDef() {
  // TODO:
}

/** Constructor */
Select::Select(Global* global, Tree* qualifier, Name* name)
  : Tree(global), qualifier(qualifier), name(name) {}

/** Destructor */
Select::~Select() {
  // TODO:
}

/** Constructor */
CannotHaveAttrs::CannotHaveAttrs(Global* global): Tree(global) {}

/** Destructor */
CannotHaveAttrs::~CannotHaveAttrs() {
  // TODO:
}

bool CannotHaveAttrs::canHaveAttrs() { return false; }

/** Constructor */
TermTree::TermTree(Global *global): Tree(global) {}

/** Destructor */
TermTree::~TermTree() {
  // TODO:
}

/** Constructor */
EmptyTree::EmptyTree(Global *global)
  : Tree(global), TermTree(global), CannotHaveAttrs(global) {}

/** Destructor */
EmptyTree::~EmptyTree() {
  // TODO:
}

bool EmptyTree::isEmpty() { return true; }

const Tree* EMPTY_TREE = new EmptyTree(nullptr);

/** Constructor */
Ident::Ident(Global* global, Name* name): Tree(global), name(name) {}

/** Destructor */
Ident::~Ident() {
  // TODO:
}

/** Constructor */
Traverser::Traverser() {}

/** Destructor */
Traverser::~Traverser() {}

void Traverser::traverse(Tree* tree) {
  // TODO:
  //itraverse(this, tree)
}

/** Constructor */
//Modifiers::Modifiers()
//  : flags(NoFlags), new Name(tpnme.EMPTY), annotations(std::vector<spTree>()) {}

Modifiers::Modifiers(
  long flags, Name* privateWithin, std::vector<Tree*> annotations)
  : flags(flags), privateWithin(privateWithin), annotations(annotations) {}

} // namespace
