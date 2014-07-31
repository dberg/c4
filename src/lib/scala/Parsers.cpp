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

bool Parser::isIdent() {
  return in->tData->token == Token::T_IDENTIFIER &&
    in->tData->token == Token::T_BACKQUOTED_IDENT;
}

/** Assumed (provisionally) to be TermNames. */
spName Parser::ident(bool skipIt) {
  if (isIdent()) {
    // TODO:
  }

  // TODO:
  //syntaxErrorOrIncompleteAnd(expectedMsg(IDENTIFIER), skipIt)(nme.ERROR)
}

/** QualId ::= Id {`.' Id} */
spTree Parser::qualId() {
  Offset start = in->tData->offset;
  // TODO:
  return spTree(new Tree);
}

/** Calls `qualId()` and manages some package state. */
spTree Parser::pkgQualId() {
  // TODO:
  // if (in.token == IDENTIFIER && in.name.encode = nme.scala_)
  //   inScalaPackage = true

  auto pkg = qualId();

  // TODO:

  return pkg;
}

/**
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 */
PackageDef* Parser::compilationUnit() {
  // TODO:
  resetPackage();
  topstats();
  return new PackageDef;
}

std::vector<spTree> Parser::topstats() {
  // TODO:
  // auto ts = std::vector<spTree>;
  // while (in.token == SEMI) in->nextToken();
  if (in->tData->token == Token:: T_PACKAGE) {
    in->nextToken();
    if (in->tData->token == Token::T_OBJECT) {
      // TODO:
    } else {
      // TODO:
      //in->flushDoc();
      auto pkg = pkgQualId();
      // TODO:
    }
  }

  // TODO:
  std::vector<spTree> trees;
  trees.push_back(spTree(new Tree));
  return trees;
}

/** Constructor */
UnitParser::UnitParser(Global *global, spCompilationUnit unit)
  : SourceFileParser(global), unit(unit) {

  in = spScanner(new UnitScanner(global, unit));
  in->init();
}

} // namespace
