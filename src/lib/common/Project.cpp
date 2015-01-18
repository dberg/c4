#include "c4/common/Project.h"

namespace c4 {

/** Constructor */
Project::Project(u32string id) : id(id),
  projJava(unique_ptr<ProjectJava>(new ProjectJava)),
  projScala(unique_ptr<ProjectScala>(new ProjectScala))
  {}

/**
 * Match the unit to a compiler using the filename extension.
 */
void Project::compile(spCompilation comp) {
  // Target specific project
  if (endsWith<u32string>(comp->filename, U".java")) {
    projJava->compile(comp);
  } else if (endsWith<u32string>(comp->filename, U".scala")) {
    projScala->compile(comp);
  } else if (endsWith<u32string>(comp->filename, U".class")) {
    // TODO:
  }
}

} // namespace
