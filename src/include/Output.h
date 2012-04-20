//-*- C++ -*-
#ifndef __OUTPUT_H__
#define __OUTPUT_H__
#include "AST.h"

namespace djp {

class Output {
  spCompilationUnit compilationUnit;

public:
  Output(spCompilationUnit _compilationUnit)
    : compilationUnit(_compilationUnit) {}

  void print();
};

}

#endif
