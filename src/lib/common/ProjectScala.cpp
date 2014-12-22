#include "c4/common/ProjectScala.h"

namespace c4 {

void ProjectScala::compile(spCompilation comp) {

  c4s::SourceFile *source = new c4s::SourceFile(
    comp->filename,
    comp->buffer
  );

  c4s::CompilationUnit *unit = new c4s::CompilationUnit(source);

  std::vector<c4s::CompilationUnit *> units;
  units.push_back(unit);

  // TODO: we should only instantiate one global object per project and keep
  // feeding it compilation units as they arrive from the client.
  c4s::Global *global = new c4s::Global;
  global->compile(units);

  delete global;
  for (auto u: units) { delete u; }
  delete source;
}

} // namespace
