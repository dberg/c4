#include "c4/common/Project.h"

namespace c4 {

/**
 * Match the unit to a compiler using the filename extension.
 */
void Project::compile(spCompilation comp) {
  // Target specific project
  if (endsWith(comp->filename, ".java")) {
    projJava->compile(comp);
  } else if (endsWith(comp->filename, ".scala")) {
    projScala->compile(comp);
  } else if (endsWith(comp->filename, ".class")) {
    // TODO:
  }
}

} // namespace
