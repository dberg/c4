//-*- C++ -*-
#ifndef __C4_SCALA_GLOBAL_H__
#define __C4_SCALA_GLOBAL_H__

#include <vector>
#include "c4/scala/CompilationUnits.h"

using std::vector;

namespace c4s {

class Global {

public:

  /** Compilation units to be compiled */
  vector<CompilationUnit*> units;

  Global();

  void compile(vector<CompilationUnit*> units);
};

} // namespace

#endif
