//-*- C++ -*-
#ifndef __C4_SCALA_POSITIONS_H__
#define __C4_SCALA_POSITIONS_H__

#include "c4/scala/TypeDefs.h"
#include "c4/scala/Trees.h"

namespace c4s {

class PosAssigner : public Traverser {
public:
  Position* pos;
  PosAssigner();
  virtual ~PosAssigner();
};

class DefaultPosAssigner : public PosAssigner {
public:
  DefaultPosAssigner();
  virtual ~DefaultPosAssigner();
  virtual void traverse(spTree tree);
};

class Positions {
protected:
  PosAssigner* posAssigner;

public:
  Positions();
  ~Positions();
  bool useOffsetPositions();
  spTree atPos(Position* pos, spTree tree);
  Position* rangePos(SourceFile *source, int start, int point, int end);
};

}; // namespace

#endif
