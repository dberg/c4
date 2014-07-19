#include "c4/scala/Phase.h"

namespace c4s {

void GlobalPhase::run() {
  for (auto &unit : global->units) {
    apply(unit);
  }
}

void ParserPhase::apply(spCompilationUnit unit) {
  if (!unit->body) {
    spUnitParser parser = spUnitParser(new UnitParser(global, unit));
    unit->body = parser->parse();
  }
}

} // namespace
