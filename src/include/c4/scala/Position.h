//-*- C++ -*-
#ifndef __C4_SCALA_POSITION_H__
#define __C4_SCALA_POSITION_H__

namespace c4s {

class Position {
public:
  Position();
  bool isOpaqueRange();
};

class UndefinedPosition {
public:
  UndefinedPosition();
};

class NoPosition : public UndefinedPosition {
public:
  NoPosition();
};

}; // namespace

#endif
