#include "c4/scala/Global.h"

#include "c4/scala/Names.h"
#include "c4/scala/Phase.h"
#include "c4/scala/StdNames.h"
#include "c4/scala/SyntaxAnalyzer.h"

namespace c4s {

Global::Global():
  globalPhase(nullptr),
  names(spNames(new Names)),
  stdNames(spStdNames(new StdNames(this)))
{
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
