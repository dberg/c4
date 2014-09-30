//-*- C++ -*-
#ifndef __C4_SCALA_PRINTERS_H__
#define __C4_SCALA_PRINTERS_H__

#include <string>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Printers {
private:
  // reflect.internal
  std::string quotedName(Name *name, bool decode);
  std::string symNameInternal(Tree *tree, Name *name, bool decoded);
  std::string decodedSymName(Tree *tree, Name *name);
  std::string symName(Tree *tree, Name *name);

  std::string render(Tree *tree);
  std::string show(Tree *tree);
public:
  Printers();
  ~Printers();
  std::string treeToString(Tree *tree);
  std::string printTree(Tree *tree);
};

}

#endif
