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
  stdNames(new StdNames(this)),
  nameTransformer(new NameTransformer()),
  positions(new Positions()),
  printers(new Printers())
{
  SyntaxAnalyzer *synAnalyzer = new SyntaxAnalyzer(this);
  phases.push_back(synAnalyzer->newPhase());
  delete synAnalyzer;
}

/** Destructor */
Global::~Global() {
  delete printers;
  delete positions;
  delete nameTransformer;
  delete stdNames;
  delete names;

  for (Phase *phase : phases) {
    delete phase;
  }
}

void Global::compile(std::vector<CompilationUnit *> units) {
  this->units = units;
  for (Phase *phase : phases) {
    globalPhase = phase;
    globalPhase->run();
  }
}

} // namespace
