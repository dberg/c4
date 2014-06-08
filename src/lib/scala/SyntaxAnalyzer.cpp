#include "c4/scala/SyntaxAnalyzer.h"

namespace c4s {

spPhase SyntaxAnalyzer::newPhase() {
  return spPhase(new ParserPhase(global));
}

} // namespace
