#include "c4/scala/Printers.h"
#include "c4/scala/TreePrinter.h"

namespace c4s {

/** Constructor */
Printers::Printers() {}

std::string Printers::render(Tree *tree) {
  // TODO: Print missing parameters. See Printers::show.
  std::string buffer;
  auto printer = std::make_shared<TreePrinter>();
  buffer.append(printer->print(tree));
  return buffer;
}

std::string Printers::show(Tree *tree) {
  return render(tree);
}

std::string Printers::treeToString(Tree *tree) {
  return show(tree);
}

} // namespace
