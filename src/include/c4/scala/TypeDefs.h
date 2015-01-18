//-*- C++ -*-
#ifndef __C4_SCALA_TYPE_DEFS_H__
#define __C4_SCALA_TYPE_DEFS_H__

#include <memory>

using std::shared_ptr;

namespace c4s {

// Global.h
class Global;
typedef shared_ptr<Global> spGlobal;

// SourceFile.h
class SourceFile;

} // namespace

#endif
