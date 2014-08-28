#include <algorithm>
#include "c4/scala/Parsers.h"
#include "c4/scala/Trees.h"
#include "c4/scala/Scanners.h"
#include "c4/scala/Names.h"
#include "c4/scala/Global.h"
#include "c4/scala/Positions.h"
#include "c4/scala/Position.h"

namespace c4s {

/** Constructor */
Parser::Parser(Global *global): global(global) {}

/** Constructor */
SourceFileParser::SourceFileParser(Global *global): Parser(global) {}

/** Parse a CompilationUnit */
spTree Parser::parse() {
  return spTree(compilationUnit());
}

spPosition Parser::r2p(Offset start, Offset mid, Offset end) {
  return global->positions->rangePos(in->source, start, mid, end);
}

spPosition Parser::r2p(Offset start, Offset mid) {
  return r2p(start, mid, std::max(in->tData->lastOffset, start));
}

spPosition Parser::r2p(Offset offset) {
  return r2p(offset, offset);
}

spTree Parser::atPos(Offset offset, spTree t) {
  return atPos(r2p(offset), t);
}

spTree Parser::atPos(Offset offset, Offset point, spTree t) {
  return atPos(r2p(offset, point), t);
}

spTree Parser::atPos(spPosition pos, spTree t) {
  return global->positions->atPos(pos, t);
}

bool Parser::isIdent() {
  return in->tData->token == Token::T_IDENTIFIER ||
    in->tData->token == Token::T_BACKQUOTED_IDENT;
}

bool Parser::isStatSep(Token token) {
  return token == Token::T_NEWLINE
    || token == Token::T_NEWLINES
    || token == Token::T_SEMI;
}

bool Parser::isStatSep() {
  return isStatSep(in->tData->token);
}

/** Assumed (provisionally) to be TermNames. */
spName Parser::ident(bool skipIt) {
  if (isIdent()) {
    auto name = in->tData->name->encode();
    in->nextToken();
    return name;
  }

  // TODO:
  //return syntaxErrorOrIncompleteAnd(expectedMsg(IDENTIFIER), skipIt)(nme.ERROR)
}

spTree Parser::selector(spTree t) {
  Offset point = in->tData->offset;
  if (t != EMPTY_TREE) {
    spTree selector = spTree(new Select(t, ident(false)));
    // TODO:
    //return selector->setPos(r2p(t.pos.start, point, in.lastOffset))
    return selector;
  } else {
    // TODO:
    //return errorTermTree;
  }
}

spTree Parser::selectors(spTree t, bool typeOK, Offset dotOffset) {
  if (typeOK && in->tData->token == Token::T_TYPE) {
    in->nextToken();
    // TODO:
    //return atPos(t->pos->start, dotOffset, SingletonTypeTree(t));
  } else {
    // TODO:
    spTree t1 = selector(t);
    //if (in->tData->token == Token::T_DOT) {
    //  return selectors(t1, typeOK, in->skipToken());
    //} else {
      return t1;
    //}
  }
}

/** QualId ::= Id {`.' Id} */
spTree Parser::qualId() {
  Offset start = in->tData->offset;
  spTree id = atPos(start, spTree(new Ident(ident())));
  if (in->tData->token == Token::T_DOT) {
    // TODO:
    //return selectors(id, typeOK = false, in.skipToken())
  }

  return id;
}

/** Calls `qualId()` and manages some package state. */
spTree Parser::pkgQualId() {
  // TODO:
  //if (in->tData->token == Token::T_IDENTIFIER && in.name.encode = nme.scala_) {
  //  inScalaPackage = true
  //}

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

/** Create a tree representing a packaging. */
spTree Parser::makePackaging(
  Offset start, spTree pkg, std::vector<spTree> stats) {

  spTree pkgDef = spTree(new PackageDef(pkg, stats));
  return atPos(start, pkg->pos()->point(), pkgDef);
}

/**
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 */
spTree Parser::compilationUnit() {
  resetPackage();
  std::vector<spTree> res = topstats();
  // TODO:
}

std::vector<spTree> Parser::topstats() {
  auto ts = std::vector<spTree>();

  while (in->tData->token == Token::T_SEMI) {
    in->nextToken();
  }

  Offset start = in->tData->offset;

  if (in->tData->token == Token::T_PACKAGE) {
    in->nextToken();
    if (in->tData->token == Token::T_OBJECT) {
      // TODO:
    } else {
      // TODO:
      //in->flushDoc();
      spTree pkg = pkgQualId();

      if (in->tData->token == Token::T_EOF) {
        // TODO:
      } else if (isStatSep()) {
        in->nextToken();
        spTree pkgDef = makePackaging(start, pkg, topstats());
        ts.push_back(pkgDef);
      } else {
        // TODO:
      }
    }
  } else {
    // TODO:
  }

  return ts;
}

/** Constructor */
UnitParser::UnitParser(Global *global, spCompilationUnit unit)
  : SourceFileParser(global), unit(unit) {

  in = spScanner(new UnitScanner(global, unit));
  in->init();
}

} // namespace
