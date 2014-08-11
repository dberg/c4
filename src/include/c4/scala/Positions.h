//-*- C++ -*-
#ifndef __C4_SCALA_POSITIONS_H__
#define __C4_SCALA_POSITIONS_H__

#include "c4/scala/TypeDefs.h"
#include "c4/scala/Trees.h"

namespace c4s {

class PosAssigner : public Traverser {
public:
  spPosition pos;
  PosAssigner();
};

class DefaultPosAssigner : public PosAssigner {
public:
  DefaultPosAssigner();
  virtual void traverse(spTree tree);
};

class Positions {
protected:
  spPosAssigner posAssigner;

public:
  Positions();
  bool useOffsetPositions();
  spTree atPos(spPosition pos, spTree tree);
  spPosition rangePos(spSourceFile source, int start, int point, int end);
};

}; // namespace

#endif
