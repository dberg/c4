#include "c4/common/Project.h"

namespace c4 {

/**
 * Match the unit to a compiler using the filename extension.
 */
void Project::compile(spCompilationUnit unit) {
  // Target specific project
  if (endsWith(unit->filename, ".java")) {
    projJava->compile(unit);
  } else if (endsWith(unit->filename, ".scala")) {
    projScala->compile(unit);
  } else if (endsWith(unit->filename, ".class")) {
    // TODO:
  }
}

} // namespace
