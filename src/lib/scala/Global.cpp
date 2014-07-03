#include "c4/scala/Global.h"

namespace c4s {

void Global::compile(std::vector<spCompilationUnit> units) {
  this->units = units;
  for (auto &phase : phases) {
    globalPhase = phase;
    globalPhase->run();
  }
}

} // namespace
