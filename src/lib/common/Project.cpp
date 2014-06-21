#include "c4/common/Project.h"

namespace c4 {

/**
 * Match the unit to a compiler using the filename extension.
 */
void Project::compile(spCompilationRequest compReq) {
  // Target specific project
  if (endsWith(compReq->filename, ".java")) {
    projJava->compile(compReq);
  } else if (endsWith(compReq->filename, ".scala")) {
    projScala->compile(compReq);
  } else if (endsWith(compReq->filename, ".class")) {
    // TODO:
  }
}

} // namespace
