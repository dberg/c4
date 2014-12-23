//-*- C++ -*-
#ifndef __C4_SCALA_TYPE_DEFS_H__
#define __C4_SCALA_TYPE_DEFS_H__

#include <memory>

#include "c4/common/TypeDefs.h"

using std::shared_ptr;

namespace c4s {

typedef c4::Char Char;

// Global.h
class Global;
typedef shared_ptr<Global> spGlobal;

// SourceFile.h
class SourceFile;

} // namespace

#endif
