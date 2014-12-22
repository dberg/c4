//-*- C++ -*-
#ifndef __C4_SCALA_COMPILATION_UNITS_H__
#define __C4_SCALA_COMPILATION_UNITS_H__

#include <memory>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class CompilationUnit {
public:
  SourceFile* source;
  CompilationUnit(SourceFile* source);
};

} // namespace

#endif
