//-*- C++ -*-
#ifndef __C4_SCALA_TREES_TYPE_DEFS_H__
#define __C4_SCALA_TREES_TYPE_DEFS_H__

#include <memory>

namespace c4s {

class Tree;
typedef std::shared_ptr<Tree> spTree;

class NameTree;
typedef std::shared_ptr<NameTree> spNameTree;

class SymTree;
typedef std::shared_ptr<SymTree> spSymTree;

class DefTree;
typedef std::shared_ptr<DefTree> spDefTree;

class MemberDef;
typedef std::shared_ptr<MemberDef> spMemberDef;

class PackageDef;
typedef std::shared_ptr<PackageDef> spPackageDef;

} // namespace

#endif