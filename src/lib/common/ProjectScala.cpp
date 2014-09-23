#include "c4/common/ProjectScala.h"

namespace c4 {

void ProjectScala::compile(spCompilation comp) {

  c4s::SourceFile *source = new c4s::ClientSourceFile(
    comp->filename,
    comp->buffer
  );

  c4s::spCompilationUnit unit = c4s::spCompilationUnit(
    new c4s::CompilationUnit(source));

  std::vector<c4s::spCompilationUnit> units;
  units.push_back(unit);

  // TODO: we should only instantiate one global object and keep feeding it
  // compilation units as they arrive from the client.
  c4s::Global *global = new c4s::Global;
  global->compile(units);

  delete source;
  delete global;
}

} // namespace
