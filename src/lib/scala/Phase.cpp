#include "c4/scala/Phase.h"

namespace c4s {

void GlobalPhase::run() {
  auto units = global->units;
  for (auto &unit : units) {
    apply(unit);
  }
}

void ParserPhase::apply(spCompilationUnit unit) {
  if (!unit->body) {
    // TODO:
    // unit->body = (new UnitParser(unit))->parse();
  }
}

} // namespace
