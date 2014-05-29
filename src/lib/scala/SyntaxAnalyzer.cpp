#include "c4/scala/SyntaxAnalyzer.h"

namespace c4s {

spPhase SyntaxAnalyzer::newPhase(spPhase prev) {
  // TODO:
  return spNoPhase(new NoPhase);
}

} // namespace
