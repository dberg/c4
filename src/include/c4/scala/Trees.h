//-*- C++ -*-
#ifndef __C4_SCALA_TREES_H__
#define __C4_SCALA_TREES_H__

#include "c4/scala/TypeDefs.h"

namespace c4s {

class Tree {
public:
  Tree();
  virtual bool canHaveAttrs();
  spPosition pos();
};

/** TODO:
 * Tree, NameTreeApi
 * NameTree
 */
class NameTree : virtual public Tree {};

/** TODO:
 * Tree, SymTreeApi
 * SymTree
 */
class SymTree : virtual public Tree {};

/** TODO:
 * SymTree, NameTree, DefTreeApi
 * DefTree
 */
class DefTree : public SymTree, public NameTree {};

/** TODO:
 * DefTree, MemberDefApi
 * MemberDef
 */
class MemberDef : public DefTree {};

/** TODO:
 *  MemberDef, PackageDefApi
 *  case class PackageDef
 *
 *  PackageDefExtractor
 *  object PackageDef
 */
class PackageDef : public MemberDef {};

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

} // namespace

#endif
