#include "c4/scala/CompilationUnits.h"
#include "c4/scala/Trees.h"

namespace c4s {

/** Constructor */
CompilationUnit::CompilationUnit(SourceFile* source)
  : source(source), body(nullptr) {}

/** Destructor */
CompilationUnit::~CompilationUnit() {
  if (body) {
    delete body;
  }
}

} // namespace
