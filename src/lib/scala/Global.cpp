#include "c4/scala/Global.h"

#include "c4/scala/Names.h"
#include "c4/scala/Phase.h"
#include "c4/scala/StdNames.h"
#include "c4/scala/SyntaxAnalyzer.h"
#include "c4/scala/NameTransformer.h"

namespace c4s {

Global::Global():
  globalPhase(nullptr),
  names(spNames(new Names(this))),
  stdNames(spStdNames(new StdNames(this))),
  nameTransformer(spNameTransformer(new NameTransformer()))
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
