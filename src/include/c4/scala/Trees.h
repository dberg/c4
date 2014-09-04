//-*- C++ -*-
#ifndef __C4_SCALA_TREES_H__
#define __C4_SCALA_TREES_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Tree {
public:
  Tree();
  virtual bool canHaveAttrs();
  virtual bool isEmpty();
  spPosition pos();
  virtual std::string toString();
};

class PackageDef : public Tree {
public:
  spTree pid;
  std::vector<spTree> stats;
  PackageDef(spTree pid, std::vector<spTree> stats);
};

class Select: public Tree {
public:
  spTree qualifier;
  spName name;
  Select(spTree qualifier, spName name);
};

class CannotHaveAttrs : virtual public Tree {
public:
  CannotHaveAttrs();
  virtual bool canHaveAttrs();
};

class TermTree : virtual public Tree {
public:
  TermTree();
};

class EmptyTree : public TermTree, public CannotHaveAttrs {
public:
  EmptyTree();
  virtual bool isEmpty();
};

extern const spTree EMPTY_TREE;

/**
 * A reference to identifier `name`.
 * TODO: Ident inherits RefTree and IdentApi
 *       type RefTree >: Null <: RefTreeApi with SymTree with NameTree
 *       type NameTree >: Null <: NameTreeApi with Tree
 */
class Ident : public Tree {
public:
  spName name;
  Ident(spName name);
};

//-----------------------------------------------------------------------------
// Traversing and transforming
//-----------------------------------------------------------------------------

/**
 * A class that implements a default tree traversal strategy: breadth-first
 * component-wise.
 */
class Traverser {
public:
  Traverser();

  /** Traverses a single tree. */
  virtual void traverse(spTree tree);
};

class Modifiers {
public:
  long flags;
  spName privateWithin;
  std::vector<spTree> annotations;
  //Modifiers();
  Modifiers(long flags, spName privateWithin,
            std::vector<spTree> annotations);
};

} // namespace

#endif
