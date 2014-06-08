#include "c4/scala/Phase.h"

namespace c4s {

void GlobalPhase::run() {
//  auto units = getGlobal()->units;
//  for (auto &unit : units) {
//    applyPhase(unit);
//  }
}

/**
 * @private
 */
void GlobalPhase::applyPhase(spCompilationUnit &unit) {
  apply(unit);
}

void ParserPhase::apply(spCompilationUnit &unit) {
  // TODO:
}

} // namespace
