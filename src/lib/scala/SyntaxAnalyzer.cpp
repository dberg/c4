#include "c4/scala/SyntaxAnalyzer.h"

namespace c4s {

spPhase SyntaxAnalyzer::newPhase(spPhase prev) {
  return spPhase(new ParserPhase(prev));
}

} // namespace
