#include "c4/scala/Phase.h"
#include "c4/scala/Global.h"
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/Parsers.h"

namespace c4s {

/** Constructor */
GlobalPhase::GlobalPhase(Global *global) : global(global) {}

void GlobalPhase::run() {
  for (auto &unit : global->units) {
    apply(unit);
  }
}

/** Constructor */
StdPhase::StdPhase(Global *global) : GlobalPhase(global) {}

/** Constructor */
ParserPhase::ParserPhase(Global *global) : StdPhase(global) {}

void ParserPhase::apply(spCompilationUnit unit) {
  if (!unit->body) {
    spUnitParser parser = spUnitParser(new UnitParser(global, unit));
    unit->body = parser->parse();
  }
}

} // namespace
