//-*- C++ -*-
#ifndef __C4_SCALA_COMPILATION_UNITS_H__
#define __C4_SCALA_COMPILATION_UNITS_H__

#include <memory>

#include "c4/scala/TypeDefs.h"
#include "c4/scala/SourceFile.h"
#include "c4/scala/Trees.h"

namespace c4s {

class CompilationUnit {
public:
  spSourceFile source;
  spTree body;
  CompilationUnit(spSourceFile source): source(source), body(nullptr) {}
};

} // namespace

#endif
