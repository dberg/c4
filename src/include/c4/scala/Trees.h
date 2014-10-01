//-*- C++ -*-
#ifndef __C4_SCALA_TREES_H__
#define __C4_SCALA_TREES_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Tree {
protected:
  Global* global;

public:
  Tree(Global* global);
  virtual ~Tree();

  virtual bool isDef();
  virtual bool canHaveAttrs();
  virtual bool isEmpty();
  Position* pos();
  virtual Symbol* symbol();
  virtual std::string toString();
};

class PackageDef : public Tree {
public:
  Tree* pid;
  std::vector<Tree*> stats;
  PackageDef(Global* global, Tree* pid, std::vector<Tree*> stats);
  virtual ~PackageDef();
};

class Select: public Tree {
public:
  Tree* qualifier;
  Name* name;

  Select(Global* global, Tree* qualifier, Name* name);
  virtual ~Select();
};

class CannotHaveAttrs : virtual public Tree {
public:
  CannotHaveAttrs(Global* global);
  virtual ~CannotHaveAttrs();

  virtual bool canHaveAttrs();
};

class TermTree : virtual public Tree {
public:
  TermTree(Global* global);
  virtual ~TermTree();
};

class EmptyTree : public TermTree, public CannotHaveAttrs {
public:
  EmptyTree(Global* global);
  virtual ~EmptyTree();

  virtual bool isEmpty();
};

extern const Tree* EMPTY_TREE;

/**
 * A reference to identifier `name`.
 * TODO: Ident inherits RefTree and IdentApi
 *       type RefTree >: Null <: RefTreeApi with SymTree with NameTree
 *       type NameTree >: Null <: NameTreeApi with Tree
 */
class Ident : public Tree {
public:
  Name* name;

  Ident(Global* global, Name* name);
  virtual ~Ident();
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
  virtual void traverse(Tree* tree);
};

class Modifiers {
public:
  long flags;
  Name* privateWithin;
  std::vector<Tree*> annotations;
  //Modifiers();
  Modifiers(long flags, Name* privateWithin,
            std::vector<Tree*> annotations);
};

} // namespace

#endif
