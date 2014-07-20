#include "c4/scala/CompilationUnits.h"

namespace c4s {

CompilationUnit::CompilationUnit(spSourceFile source)
  : source(source), body(nullptr) {}

} // namespace
