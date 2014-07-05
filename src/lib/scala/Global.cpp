#include "c4/scala/Global.h"

namespace c4s {

Global::Global(): globalPhase(nullptr) {
  spGlobal global = spGlobal(this);
  spSyntaxAnalyzer synAnalyzer = spSyntaxAnalyzer(new SyntaxAnalyzer(global));
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
