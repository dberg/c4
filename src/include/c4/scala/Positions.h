//-*- C++ -*-
#ifndef __C4_SCALA_POSITIONS_H__
#define __C4_SCALA_POSITIONS_H__

#include "c4/scala/TypeDefs.h"

namespace c4s {

class Positions {
public:
  Positions();
  spTree atPos(spPosition pos);
  spPosition rangePos(spSourceFile source, int start, int point, int end);
};

}; // namespace

#endif
