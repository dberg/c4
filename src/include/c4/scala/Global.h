//-*- C++ -*-
#ifndef __C4_SCALA_GLOBAL_H__
#define __C4_SCALA_GLOBAL_H__

#include <vector>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Names;

class Global {

protected:

  /** The current phase being run */
  spPhase globalPhase;

  /** The phases to be applied in order */
  std::vector<spPhase> phases;

  /** Each Component is a phase factory */
  // TODO: std::vector<spSubComponent> phaseDescriptors;

public:

  spNames names;
  spStdNames stdNames;
  spNameTransformer nameTransformer;
  spPositions positions;
  Printers *printers;

  /** Compilation units to be compiled */
  std::vector<spCompilationUnit> units;

  Global();
  ~Global();

  void compile(std::vector<spCompilationUnit> units);
};

} // namespace

#endif
