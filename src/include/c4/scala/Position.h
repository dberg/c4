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
  virtual int point();
  virtual int start();

  static spPosition offset(SourceFile *source, int point);
};

class DefinedPosition : public Position {
public:
  DefinedPosition();
};

class OffsetPosition : public DefinedPosition {
public:
  SourceFile *sourceIn;
  int pointIn;
  OffsetPosition(SourceFile *sourceIn, int pointIn);
  virtual int point();
  virtual int start();
};

class UndefinedPosition : public Position {
public:
  UndefinedPosition();
};

extern const spPosition NoPosition;

}; // namespace

#endif
