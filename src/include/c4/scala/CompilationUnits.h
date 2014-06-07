//-*- C++ -*-
#ifndef __SCALA_COMPILATION_UNITS_H__
#define __SCALA_COMPILATION_UNITS_H__

#include <memory>

#include "c4/scala/Trees.h"

namespace c4s {

class CompilationUnit;
typedef std::shared_ptr<CompilationUnit> spCompilationUnit;

class CompilationUnit {

public:

  spTree body;

  CompilationUnit(): body(nullptr) {}

};

} // namespace

#endif
