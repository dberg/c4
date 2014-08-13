//-*- C++ -*-
#ifndef __C4_SCALA_POSITION_H__
#define __C4_SCALA_POSITION_H__

#include "c4/scala/TypeDefs.h"

namespace c4s {

class Position {
public:
  Position();
  bool isOpaqueRange();
};

class UndefinedPosition : public Position {
public:
  UndefinedPosition();
};

}; // namespace

#endif
