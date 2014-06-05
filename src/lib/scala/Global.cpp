#include "c4/scala/Global.h"

namespace c4s {

void Run::compileUnits(std::vector<c4::spCompilationUnit> &units) {
  this->units = units;
  for (auto &phase : phases) {
    globalPhase = phase;
    globalPhase->run();
  }
}

} // namespace
