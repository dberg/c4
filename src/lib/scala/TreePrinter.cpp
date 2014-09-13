#include "c4/scala/TreePrinter.h"
#include "c4/scala/Trees.h"
#include "c4/scala/Symbols.h"

namespace c4s {

/** Constructor */
TreePrinter::TreePrinter() {}

std::string TreePrinter::print(Tree *tree) {
  // TODO:
  //printPosition(tree)
  if (tree->isDef() /*&& tree->symbol != NoSymbol*/ && tree->symbol()->isInitialized()) {
    // TODO:
    //printPosition(tree)
    //printTree(

    //)
  } else {
    // TODO:
    //tree
  }

  return "TODO";
}

} // namespace
