//-*- C++ -*-
#ifndef __C4_SCALA_SYMBOL_TABLE_H__
#define __C4_SCALA_SYMBOL_TABLE_H__

#include "c4/scala/Names.h"

namespace c4s {

class SymbolTable {
public:
  spNames names;
  SymbolTable(): names(spNames(new Names)) {}
};

} // namespace

#endif
