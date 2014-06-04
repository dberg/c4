#include "c4/scala/SyntaxAnalyzer.h"

namespace c4s {

StdPhase* SyntaxAnalyzer::newPhase(Phase *prev) {
  return new ParserPhase(prev);
}

} // namespace
