#include "c4/scala/Parsers.h"
#include "c4/scala/Trees.h"
#include "c4/scala/Scanners.h"
#include "c4/scala/Names.h"

namespace c4s {

/** Constructor */
Parser::Parser(Global *global): global(global) {}

/** Constructor */
SourceFileParser::SourceFileParser(Global *global): Parser(global) {}

/** Parse a CompilationUnit */
spTree Parser::parse() {
  return spTree(compilationUnit());
}

// TODO:
//spPosition Parser::r2p(Offset start, Offset mid, Offset end);
//spPosition Parser::r2p(Offset start, Offset mid);
//spPosition Parser::r2p(Offset offset);

spTree Parser::atPos(Offset offset, spTree t) {
  // TODO:
}

bool Parser::isIdent() {
  return in->tData->token == Token::T_IDENTIFIER ||
    in->tData->token == Token::T_BACKQUOTED_IDENT;
}

/** Assumed (provisionally) to be TermNames. */
spName Parser::ident(bool skipIt) {
  if (isIdent()) {
    auto name = in->tData->name->encode();
    in->nextToken();
    return name;
  }

  // TODO:
  //syntaxErrorOrIncompleteAnd(expectedMsg(IDENTIFIER), skipIt)(nme.ERROR)
}

/** QualId ::= Id {`.' Id} */
spTree Parser::qualId() {
  Offset start = in->tData->offset;
  // TODO:
  // auto id = apPos(start, spTree(new Ident(ident())));
  return spTree(new Tree);
}

/** Calls `qualId()` and manages some package state. */
spTree Parser::pkgQualId() {
  // TODO:
  // if (in.token == IDENTIFIER && in.name.encode = nme.scala_)
  //   inScalaPackage = true

  spTree pkg = qualId();
  // TODO: newLineOptWhenFollowedBy(LBRACE)

  if (currentPackage == "") {
    //currentPackage = pkg->toString();
  } else {
    //currentPackage += ".";
    //currentPackage += pkg->toString();
  }

  return pkg;
}

/**
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 */
spTree Parser::compilationUnit() {
  resetPackage();
  std::vector<spTree> res = topstats();
  // TODO:
  return spTree(new PackageDef());
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
      spTree pkg = pkgQualId();
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
