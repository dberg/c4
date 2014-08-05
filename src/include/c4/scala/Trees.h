//-*- C++ -*-
#ifndef __C4_SCALA_TREES_H__
#define __C4_SCALA_TREES_H__

#include "c4/scala/TypeDefs.h"

namespace c4s {

class Tree {};

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

} // namespace

#endif
