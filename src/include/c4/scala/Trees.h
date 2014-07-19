//-*- C++ -*-
#ifndef __C4_SCALA_TREES_H__
#define __C4_SCALA_TREES_H__

#include <memory>

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

} // namespace

#endif
