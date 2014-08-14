//-*- C++ -*-
#ifndef __C4_SCALA_POSITION_H__
#define __C4_SCALA_POSITION_H__

#include "c4/scala/TypeDefs.h"

namespace c4s {

class Position {
private:
  static spPosition validate(spPosition pos);

public:
  Position();
  bool isOpaqueRange();

  static spPosition offset(spSourceFile source, int point);
};

class DefinedPosition : public Position {
public:
  DefinedPosition();
};

class OffsetPosition : public DefinedPosition {
public:
  spSourceFile sourceIn;
  int pointIn;
  OffsetPosition(spSourceFile sourceIn, int pointIn);
};

class UndefinedPosition : public Position {
public:
  UndefinedPosition();
};

}; // namespace

#endif
