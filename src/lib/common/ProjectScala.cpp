#include "c4/common/ProjectScala.h"

namespace c4 {

void ProjectScala::compile(spCompilation comp) {
  // TODO: we should probably only instantiate one global object and keep
  // feeding it compilation units as they arrive from the client.
  c4s::spGlobal global = c4s::spGlobal(new c4s::Global);
}

} // namespace
