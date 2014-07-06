#include "c4/scala/Global.h"

namespace c4s {

Global::Global(): globalPhase(nullptr) {
  spSyntaxAnalyzer synAnalyzer = spSyntaxAnalyzer(new SyntaxAnalyzer(this));
  phases.push_back(synAnalyzer->newPhase());
}

void Global::compile(std::vector<spCompilationUnit> units) {
  this->units = units;
  for (auto &phase : phases) {
    globalPhase = phase;
    globalPhase->run();
  }
}

} // namespace
