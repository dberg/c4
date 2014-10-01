#include "c4/scala/Phase.h"
#include "c4/scala/Global.h"
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/Parsers.h"

namespace c4s {

/** Constructor */
Phase::Phase() {}

/** Destructor */
Phase::~Phase() {}

/** Constructor */
GlobalPhase::GlobalPhase(Global *global) : global(global) {}

/** Destructor */
GlobalPhase::~GlobalPhase() {}

void GlobalPhase::run() {
  for (auto &unit : global->units) {
    apply(unit);
  }
}

/** Constructor */
StdPhase::StdPhase(Global* global) : GlobalPhase(global) {}

/** Destructor */
StdPhase::~StdPhase() {}

/** Constructor */
ParserPhase::ParserPhase(Global* global) : StdPhase(global) {}

/** Destructor */
ParserPhase::~ParserPhase() {}

void ParserPhase::apply(CompilationUnit* unit) {
  if (!unit->body) {
    UnitParser* parser = new UnitParser(global, unit);
    unit->body = parser->parse();
    delete parser;
  }
}

} // namespace
