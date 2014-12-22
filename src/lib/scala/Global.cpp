#include "c4/scala/Global.h"

namespace c4s {

/** Constructor */
Global::Global() {}

void Global::compile(vector<CompilationUnit*> units) {
  this->units = units;
}

} // namespace
