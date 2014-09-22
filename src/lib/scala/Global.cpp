#include "c4/scala/Global.h"

#include "c4/scala/Names.h"
#include "c4/scala/Phase.h"
#include "c4/scala/StdNames.h"
#include "c4/scala/SyntaxAnalyzer.h"
#include "c4/scala/NameTransformer.h"
#include "c4/scala/Positions.h"
#include "c4/scala/Printers.h"

namespace c4s {

/** Constructor */
Global::Global():
  globalPhase(nullptr),
  names(new Names(this)),
  stdNames(spStdNames(new StdNames(this))),
  nameTransformer(spNameTransformer(new NameTransformer())),
  positions(new Positions()),
  printers(new Printers())
{
  spSyntaxAnalyzer synAnalyzer = spSyntaxAnalyzer(new SyntaxAnalyzer(this));
  phases.push_back(synAnalyzer->newPhase());
}

/** Destructor */
Global::~Global() {
  delete printers;
  delete positions;
  delete names;
}

void Global::compile(std::vector<spCompilationUnit> units) {
  this->units = units;
  for (auto &phase : phases) {
    globalPhase = phase;
    globalPhase->run();
  }
}

} // namespace
