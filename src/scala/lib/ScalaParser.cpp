#include "djp/ScalaParser.h"

namespace djp {

/**
 * CompilationUnit ::= {‘package’ QualId semi} TopStatSeq
 */
void ScalaParser::parseCompilationUnit() {
  // TODO:
  //if (lexer->getCurToken() == STok::PACKAGE) {
  //
  //}

  // TopStatSeq
  compUnit->topStatSeq = spTopStatSeq(new TopStatSeq);
  parseTopStatSeq(compUnit->topStatSeq);
}

/**
 * ObjectDef ::= id ClassTemplateOpt
 */
void ScalaParser::parseObjectDef(spObjectDef &objectDef) {
  // TODO:
}

/**
 * TmplDef ::= [‘case’] ‘class’ ClassDef
 *           | [‘case’] ‘object’ ObjectDef
 *           | ‘trait’ TraitDef
 */
void ScalaParser::parseTmplDef(spTmplDef &tmplDef) {
  // TODO:
}

/**
 * TopStat ::= {Annotation [nl]} {Modifier} TmplDef
 *           | Import
 *           | Packaging
 *           | PackageObject
 */
void ScalaParser::parseTopStat(spTopStat &topStat) {
  // TODO:
}

/**
 * TopStatSeq ::= TopStat {semi TopStat}
 */
void ScalaParser::parseTopStatSeq(spTopStatSeq &topStatSeq) {
  topStatSeq->topStat = spTopStat(new TopStat);
  parseTopStat(topStatSeq->topStat);
  if (topStatSeq->topStat->err) {
    topStatSeq->addErr(-1);
  }

  // TODO:
  // {semi TopStat}
}

void ScalaParser::parse() {
  buildParseTree();
}

void ScalaParser::buildParseTree() {
  lexer->getNextToken();
  while (true) {
    switch (lexer->getCurToken()) {
    case STok::END_OF_FILE:
      return;
    case STok::ERROR:
      return;
    default:
      parseCompilationUnit();
      return;
    }
  }
}

} // namespace
