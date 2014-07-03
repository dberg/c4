#include "c4/common/ProjectScala.h"

namespace c4 {

void ProjectScala::compile(spCompilation comp) {
  // TODO: we should only instantiate one global object and keep feeding it
  // compilation units as they arrive from the client.
  c4s::spGlobal global = c4s::spGlobal(new c4s::Global);

  c4s::spSourceFile source = c4s::spSourceFile(new c4s::ClientSourceFile(
    comp->filename,
    comp->buffer
  ));

  c4s::spCompilationUnit unit = c4s::spCompilationUnit(
    new c4s::CompilationUnit(source));

  std::vector<c4s::spCompilationUnit> units;
  units.push_back(unit);

  // TODO:
  //global->compile(units);
}

} // namespace
