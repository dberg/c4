//-*- C++ -*-
#ifndef __C4_SCALA_TREE_PRINTER_H__
#define __C4_SCALA_TREE_PRINTER_H__

#include <string>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class TreePrinter {
public:
  TreePrinter();
  std::string print(Tree *tree);
};

} // namespace

#endif
