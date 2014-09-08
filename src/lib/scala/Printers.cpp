#include "c4/scala/Printers.h"

namespace c4s {

/** Constructor */
Printers::Printers() {}

std::string Printers::render(Tree *tree) {
  // TODO: Print missing parameters. See Printers::show.
  return "TODO";
}

std::string Printers::show(Tree *tree) {
  return render(tree);
}

std::string Printers::treeToString(Tree *tree) {
  return show(tree);
}

} // namespace
