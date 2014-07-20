#include "c4/scala/Parsers.h"
#include "c4/scala/Trees.h"
#include "c4/scala/Scanners.h"

namespace c4s {

/** Constructor */
Parser::Parser(Global *global): global(global) {}

/** Constructor */
SourceFileParser::SourceFileParser(Global *global): Parser(global) {}

/** Parse a CompilationUnit */
spTree Parser::parse() {
  return spTree(compilationUnit());
}

/**
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 */
PackageDef* Parser::compilationUnit() {
  // TODO:
  resetPackage();
  return new PackageDef;
}

/** Constructor */
UnitParser::UnitParser(Global *global, spCompilationUnit unit)
  : SourceFileParser(global), unit(unit) {

  in = spScanner(new UnitScanner(global, unit));
  in->init();
}

} // namespace
