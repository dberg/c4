#include "c4/scala/Parsers.h"

namespace c4s {

spTree Parser::parse() {
  return spTree(compilationUnit());
}

/**
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 */
PackageDef* Parser::compilationUnit() {
  // TODO:
  return new PackageDef;
}

} // namespace
