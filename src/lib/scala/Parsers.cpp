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
Parser::Parser(Global* global): global(global) {}

Parser::~Parser() {}

/** Parse a CompilationUnit */
Tree* Parser::parse() {
  return compilationUnit();
}

Position* Parser::r2p(Offset start, Offset mid, Offset end) {
  return global->positions->rangePos(in->source, start, mid, end);
}

Position* Parser::r2p(Offset start, Offset mid) {
  return r2p(start, mid, std::max(in->tData->lastOffset, start));
}

Position* Parser::r2p(Offset offset) {
  return r2p(offset, offset);
}

Tree* Parser::atPos(Offset offset, Tree* t) {
  return atPos(r2p(offset), t);
}

Tree* Parser::atPos(Offset offset, Offset point, Tree* t) {
  return atPos(r2p(offset, point), t);
}

Tree* Parser::atPos(Position* pos, Tree* t) {
  return global->positions->atPos(pos, t);
}

bool Parser::isModifier() {
  switch (in->tData->token) {
  case Token::T_ABSTRACT:
  case Token::T_FINAL:
  case Token::T_SEALED:
  case Token::T_PRIVATE:
  case Token::T_PROTECTED:
  case Token::T_OVERRIDE:
  case Token::T_IMPLICIT:
  case Token::T_LAZY:
    return true;
  default:
    return false;
  }
}

bool Parser::isAnnotation() {
  return in->tData->token == Token::T_AT;
}

bool Parser::isTemplateIntro() {
  switch (in->tData->token) {
  case Token::T_OBJECT:
  case Token::T_CASEOBJECT:
  case Token::T_CLASS:
  case Token::T_CASECLASS:
  case Token::T_TRAIT:
    return true;
  default:
    return false;
  }
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
Name* Parser::ident(bool skipIt) {
  if (isIdent()) {
    Name* name = in->tData->name->encode();
    in->nextToken();
    return name;
  }

  // TODO:
  //return syntaxErrorOrIncompleteAnd(expectedMsg(IDENTIFIER), skipIt)(nme.ERROR)
}

Tree* Parser::selector(Tree* t) {
  Offset point = in->tData->offset;
  if (t != EMPTY_TREE) {
    Tree* selector = new Select(global, t, ident(false));
    // TODO:
    //return selector->setPos(r2p(t.pos.start, point, in.lastOffset))
    return selector;
  } else {
    // TODO:
    //return errorTermTree;
  }
}

Tree* Parser::selectors(Tree* t, bool typeOK, Offset dotOffset) {
  if (typeOK && in->tData->token == Token::T_TYPE) {
    in->nextToken();
    // TODO:
    //return atPos(t->pos->start, dotOffset, SingletonTypeTree(t));
  } else {
    // TODO:
    Tree* t1 = selector(t);
    //if (in->tData->token == Token::T_DOT) {
    //  return selectors(t1, typeOK, in->skipToken());
    //} else {
      return t1;
    //}
  }
}

/** QualId ::= Id {`.' Id} */
Tree* Parser::qualId() {
  Offset start = in->tData->offset;
  Tree* id = atPos(start, new Ident(global, ident()));
  if (in->tData->token == Token::T_DOT) {
    // TODO:
    //return selectors(id, typeOK = false, in.skipToken())
  }

  return id;
}

/** Calls `qualId()` and manages some package state. */
Tree* Parser::pkgQualId() {
  // TODO:
  //if (in->tData->token == Token::T_IDENTIFIER && in.name.encode = nme.scala_) {
  //  inScalaPackage = true
  //}

  Tree* pkg = qualId();
  // TODO: newLineOptWhenFollowedBy(LBRACE)

  if (currentPackage == "") {
    currentPackage = pkg->toString();
  } else {
    currentPackage += ".";
    currentPackage += pkg->toString();
  }

  return pkg;
}

/**
 *  Modifiers ::= {Modifier}
 *  Modifier  ::= LocalModifier
 *              |  AccessModifier
 *              |  override
 */
Modifiers* Parser::modifiers() {
  auto loop = [this](Modifiers mods) {
    switch(in->tData->token) {
    case Token::T_PRIVATE:
    case Token::T_PROTECTED:
      // TODO:
      //return loop(accessQualifierOpt(addMod(mods, flagTokens(in.token), tokenRange(in))))
    case Token::T_ABSTRACT:
    case Token::T_FINAL:
    case Token::T_SEALED:
    case Token::T_OVERRIDE:
    case Token::T_IMPLICIT:
    case Token::T_LAZY:
      // TODO:
      //return loop(addMod(mods, flagTokens(in.token), tokenRange(in)))
    case Token::T_NEWLINE:
      // TODO:
      in->nextToken();
      //return loop(mods)
    default: return mods;
    }
  };
  //loop(NoMods);
}

Offset Parser::caseAwareTokenOffset() {
  if (in->tData->token == Token::T_CASECLASS
      || in->tData->token == Token::T_CASEOBJECT) {
    return in->sData->prev->offset;
  } else {
    return in->tData->offset;
  }
}

Tree* Parser::topLevelTmpDef() {
  // TODO:
  //val annots = annotations(skipNewLines = true)
  Offset pos = caseAwareTokenOffset();
  //val mods   = modifiers() withAnnotations annots
  //tmplDef(pos, mods)
}

//Tree* Parser::tmplDef(Offset pos, Modifiers mods) {
// TODO:
//}

Tree* Parser::packageOrPackageObject(Offset start) {
  if (in->tData->token == Token::T_OBJECT) {
    // TODO:
    //joinComment(packageObjectDef(start) :: Nil).head
  } else {
    // TODO:
    //in->flushDoc
    //return makePackaging(start, pkgQualId(), inBracesOrNil(topStatSeq()));
  }
}

/** Create a tree representing a packaging. */
Tree* Parser::makePackaging(
  Offset start, Tree* pkg, std::vector<Tree*> stats) {

  Tree* pkgDef = new PackageDef(global, pkg, stats);
  return atPos(start, pkg->pos()->point(), pkgDef);
}

//std::vector<Tree*> Parser::statSeq(PartialFunction[Token, List[Tree]] stat, std::string errorMsg) {
  // TODO:
//}

/**
 * TopStatSeq ::= TopStat {semi TopStat}
 * TopStat ::= Annotations Modifiers TmplDef
 *           | Packaging
 *           | package object objectDef
 *           | Import
 */
std::vector<Tree*> Parser::topStatSeq() {
  // TODO:
  //statSeq(topStat, errorMsg = "expected class or object definition")
}

std::function<std::vector<Tree*> (Token)> Parser::topStat() {
  return [this](Token t) {
    if (t == Token::T_PACKAGE) {
      std::vector<Tree*> v(1);
      v.push_back(packageOrPackageObject(in->skipToken()));
      return v;
    } else if (t == Token::T_IMPORT) {
      // TODO:
    } else if (isAnnotation() || isTemplateIntro() || isModifier()) {
      // TODO:
      //joinComment(topLevelTmplDef :: Nil)
    } else {
      // TODO:
    }

    // TODO: dummy value
    std::vector<Tree*> v;
    return v;
  };
}

/**
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 */
PackageDef* Parser::compilationUnit() {
  resetPackage();
  std::vector<Tree*> stats = topstats();

  if (stats.size() == 1 && static_cast<PackageDef*>(stats[0])) {
    return static_cast<PackageDef*>(stats[0]);
  } else {
    bool allEmpty = true;
    for (Tree* t: stats) {
      if (t != EMPTY_TREE) {
        allEmpty = false;
      }
    }
    int start = 0;
    if (!allEmpty) {
      //auto wpos = wrappingPos(stats);
      //if (wpos.isDefined) {
      //  start = wpos.start;
      //} else {
        start = 0;
      //}
    }

    //return makeEmptyPackage(start, stats);
  }
}

std::vector<Tree*> Parser::topstats() {
  auto ts = std::vector<Tree*>();

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
      Tree* pkg = pkgQualId();

      if (in->tData->token == Token::T_EOF) {
        Tree* pkgDef = makePackaging(start, pkg, std::vector<Tree*>());
        ts.push_back(pkgDef);
      } else if (isStatSep()) {
        in->nextToken();
        Tree* pkgDef = makePackaging(start, pkg, topstats());
        ts.push_back(pkgDef);
      } else {
        // TODO: ts += inBraces(makePackaging(start, pkg, topStatSeq()))
        // TODO: acceptStatSepOpt()
        auto ts2 = topStatSeq();
        ts.insert(ts.end(), ts2.begin(), ts2.end());
      }
    }
  }

  return ts;
}

/** Constructor */
SourceFileParser::SourceFileParser(Global* global): Parser(global) {}

/** Destructor */
SourceFileParser::~SourceFileParser() {}

/** Constructor */
UnitParser::UnitParser(Global* global, CompilationUnit* unit)
  : SourceFileParser(global), unit(unit) {

  in = new UnitScanner(global, unit);
  in->init();
}

/** Destructor */
UnitParser::~UnitParser() {
  delete in;
}

} // namespace
