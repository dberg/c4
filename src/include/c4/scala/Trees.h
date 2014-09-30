//-*- C++ -*-
#ifndef __C4_SCALA_TREES_H__
#define __C4_SCALA_TREES_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Tree {
protected:
  Global *global;

public:
  Tree(Global *global);
  virtual bool isDef();
  virtual bool canHaveAttrs();
  virtual bool isEmpty();
  Position* pos();
  virtual spSymbol symbol();
  virtual std::string toString();
};

class PackageDef : public Tree {
public:
  spTree pid;
  std::vector<spTree> stats;
  PackageDef(Global *global, spTree pid, std::vector<spTree> stats);
};

class Select: public Tree {
public:
  spTree qualifier;
  Name *name;
  Select(Global *global, spTree qualifier, Name *name);
};

class CannotHaveAttrs : virtual public Tree {
public:
  CannotHaveAttrs(Global *global);
  virtual bool canHaveAttrs();
};

class TermTree : virtual public Tree {
public:
  TermTree(Global *global);
};

class EmptyTree : public TermTree, public CannotHaveAttrs {
public:
  EmptyTree(Global *global);
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
  Name *name;
  Ident(Global *global, Name *name);
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
  virtual ~Traverser();

  /** Traverses a single tree. */
  virtual void traverse(spTree tree);
};

class Modifiers {
public:
  long flags;
  Name *privateWithin;
  std::vector<spTree> annotations;
  //Modifiers();
  Modifiers(long flags, Name *privateWithin,
            std::vector<spTree> annotations);
};

} // namespace

#endif
