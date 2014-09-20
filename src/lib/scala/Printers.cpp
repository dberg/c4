#include "c4/scala/Printers.h"
#include "c4/scala/TreePrinter.h"
#include "c4/scala/Trees.h"

namespace c4s {

/** Constructor */
Printers::Printers() {}

std::string Printers::quotedName(spName name, bool decode) {
  // TODO:
  return "TODO";
}

std::string Printers::symNameInternal(Tree *tree, spName name, bool decoded) {
  spSymbol sym = tree->symbol();
  // TODO:
  if (sym == nullptr /* || sym == NoSymbol*/) {
    // Names.class
    // Implicit conversion Name -> NameOps[Name]
    //return quotedName(name->dropLocal, decoded);
  } //else if (sym.isErroneous) {
    //return s"<$qname: error>";
  //} else if (sym.isMixinConstructor) {
  //  return s"/*$qowne*/$symbol";
  //} else {
  //  return qsymbol;
  //}
  return "TODO";
}

std::string Printers::decodedSymName(Tree * tree, spName name) {
  return symNameInternal(tree, name, false);
}

std::string Printers::symName(Tree *tree, spName name) {
  return symNameInternal(tree, name, false);
}

std::string Printers::render(Tree *tree) {
  // TODO: Print missing parameters. See Printers::show.
  std::string buffer;
  auto printer = std::make_shared<TreePrinter>();
  buffer.append(printer->print(tree));
  return buffer;
}

// TODO: additional parameters are
//       printTypes, printIds, printOwners, printKinds,
//       printMirrors and printPositions.
// We'll skip that for now.
std::string Printers::show(Tree *tree) {
  return render(tree);
}

std::string Printers::treeToString(Tree *tree) {
  return show(tree);
}

std::string printTree(Tree *tree) {
  // TODO:
  // case id @ Ident(name) =>
  //   val str = symName(tree, name)
  //   print( if (id.isBackquoted) "`" + str + "`" else str)
  return "TODO";
}

} // namespace
