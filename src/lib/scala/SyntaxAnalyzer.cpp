#include "c4/scala/SyntaxAnalyzer.h"
#include "c4/scala/Phase.h"

namespace c4s {

/** Constructor */
SyntaxAnalyzer::SyntaxAnalyzer(Global *global)
  : SubComponent(global, "parser") {}

spPhase SyntaxAnalyzer::newPhase() {
  return spPhase(new ParserPhase(global));
}

} // namespace
