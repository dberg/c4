#include "c4/scala/Phase.h"

namespace c4s {

void NoPhase::run() {
  // TODO:
}

void GlobalPhase::run() {
  // TODO:
  // for (auto &unit : globalRun->units) {
  //   applyPhase(unit);
  // }
}

/**
 * @private
 */
void GlobalPhase::applyPhase(spCompilationUnit &unit) {
  // TODO:
}

void ParserPhase::apply(spCompilationUnit &unit) {
  // TODO:
}

} // namespace
