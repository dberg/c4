//-*- C++ -*-
#ifndef __SCALA_PARSER_H__
#define __SCALA_PARSER_H__
#include <string>
#include "c4/SourceCodeStream.h"
#include "c4/Diagnosis.h"
#include "ScalaAST.h"
#include "ScalaLexer.h"
#include "ScalaParserState.h"
#include "ScalaToken.h"

namespace c4s {

class ScalaParser {
  const std::string filename;
  c4::spSourceCodeStream src;
  spScalaLexer lexer;

  // lexical grammar
  spLexId parseLexId();
  void parseSemi(spSemi &semi);
  spStringLiteral parseStringLiteral();

  // grammar
  void buildParseTree();
  void parseAnnotation(spAnnotation &annotation);
  void parseAnnotType(spAnnotType &annotType);
  void parseArgumentExprs(spArgumentExprs &argExprs);
  void parseArgumentExprsHelper(spArgumentExprs &argExprs);
  void parseBlock(spBlock &block);
  void parseBlockExpr(spBlockExpr &blockExpr);
  void parseBlockStat(spBlockStat &blockStat);
  void parseClassDef(spClassDef &classDef);
  void parseClassParents(spClassParents &classParents);
  void parseClassTemplate(spClassTemplate &classTmpl);
  void parseClassTemplateOpt(spClassTemplateOpt &classTmplOpt);
  void parseCompilationUnit();
  void parseConstr(spConstr &constr);
  void parseDef(spDef &def);
  void parseFunDef(spFunDef &funDef);
  void parseFunSig(spFunSig &funSig);
  void parseExpr(spExpr &expr);
  void parseExpr1(spExpr1 &expr1);
  void parseExprs(spExprs &exprs);
  void parseImport(spImport &import);
  void parseImportExpr(spImportExpr &importExpr);
  void parseImportSelector(spImportSelector &importSelector);
  void parseImportSelectors(spImportSelectors &importSelectors);
  void parseInfixExpr(spInfixExpr &infixExpr);
  void parseLiteral(spLiteral &literal);
  void parseObjectDef(spObjectDef &objectDef);
  void parsePackaging(spPackaging &packaging);
  void parsePath(spPath &path);
  void parsePeriodId(spPeriodId &periodId);
  void parsePrefixExpr(spPrefixExpr &prefixExpr);
  void parsePostfixExpr(spPostfixExpr &postfixExpr);
  void parseQualId(spQualId &qualId);
  void parseSimpleExpr(spSimpleExpr &simpleExpr);
  void parseSimpleExpr1(spSimpleExpr1 &simpleExpr1);
  void parseSimpleExpr1Head(spSimpleExpr1Head &head);
  void parseSimpleExpr1Tail(spSimpleExpr1Tail &tail);
  void parseSimpleType(spSimpleType &simpleType);
  void parseSimpleTypeHead(spSimpleTypeHead &head);
  void parseSimpleTypeTail(spSimpleTypeTail &tail);
  void parseSimpleTypeTails(spSimpleTypeTails &tails);
  void parseStableId(spStableId &stableId);
  void parseStableIdHead(spStableIdHead &head);
  void parseStableIdTail(spStableIdTail &tail);
  void parseTemplateBody(spTemplateBody &tmplBody);
  void parseTemplateStat(spTemplateStat &tmplStat);
  void parseTmplDef(spTmplDef &tmplDef);
  void parseTopStat(spTopStat &topStat);
  void parseTopStatSeq(spTopStatSeq &topStatSeq);
  void parseTraitDef(spTraitDef &traitDef);
  void parseTraitParents(spTraitParents &parents);
  void parseTraitTemplate(spTraitTemplate &traitTemplate);
  void parseTraitTemplateOpt(spTraitTemplateOpt &traitTemplateOpt);

  // helper methods
  int addErr(int err);
  void saveState(State &state);
  void restoreState(State &state);

public:
  ScalaParser(const std::string filename, const std::string &buffer)
    : filename(filename),
      src(c4::spSourceCodeStream(new c4::SourceCodeStream(buffer))),
      compUnit(spCompilationUnit(new CompilationUnit))
  {
    diag = c4::spDiagnosis(new c4::Diagnosis);
    lexer = spScalaLexer(new ScalaLexer(src, diag, indentMap));
  }

  c4::spDiagnosis diag;
  spCompilationUnit compUnit;
  std::vector<spComment> comments;
  ScalaLineIndentationMap indentMap;

  void parse();
};

} // namespace

#endif
