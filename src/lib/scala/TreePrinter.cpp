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
    //
  } else {
    //tree
  }
  // case unit: CompilationUnit
  // case arg => super.print(arg)

  // TODO: case Tree
  return printTree(tree);
}

std::string TreePrinter::printTree(Tree *tree) {
  return "TODO";
}

} // namespace
