//-*- C++ -*-
#ifndef __C4_SCALA_PRINTERS_H__
#define __C4_SCALA_PRINTERS_H__

#include <string>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Printers {
private:

  // TODO: see Printers::show.
  std::string render(Tree *tree);

  // TODO: additional parameters are
  //       printTypes, printIds, printOwners, printKinds,
  //       printMirrors and printPositions.
  // We'll skip that for now.
  std::string show(Tree *tree);
public:
  Printers();
  std::string treeToString(Tree *tree);
};

}

#endif
