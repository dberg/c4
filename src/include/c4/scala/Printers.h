//-*- C++ -*-
#ifndef __C4_SCALA_PRINTERS_H__
#define __C4_SCALA_PRINTERS_H__

#include <string>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Printers {
public:
  Printers();
  std::string treeToString(Tree *tree);
};

}

#endif
