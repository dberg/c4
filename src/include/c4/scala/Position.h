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

class UndefinedPosition : public Position {
public:
  UndefinedPosition();
};

}; // namespace

#endif
