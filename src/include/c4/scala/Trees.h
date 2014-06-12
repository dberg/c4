//-*- C++ -*-
#ifndef __C4_SCALA_TREE_H__
#define __C4_SCALA_TREE_H__

#include <memory>

#include "c4/scala/TreesTypeDefs.h"

namespace c4s {

class Tree {};

/** TODO:
 * Tree, NameTreeApi
 * NameTree
 */
class NameTree : public Tree {};

/** TODO:
 * Tree, SymTreeApi
 * SymTree
 */
class SymTree : public Tree {};

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

} // namespace

#endif
