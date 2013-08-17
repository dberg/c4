//-*- C++ -*-
#ifndef __SCALA_PARSER_H__
#define __SCALA_PARSER_H__
#include <string>
#include "djp/SourceCodeStream.h"
#include "djp/Diagnosis.h"
#include "ScalaAST.h"
#include "ScalaLexer.h"
#include "ScalaParserState.h"
#include "ScalaToken.h"
using namespace djp::scala;

namespace djp {

class ScalaParser {
  const std::string filename;
  spSourceCodeStream src;
  spScalaLexer lexer;

  // lexical grammar
  spLexId parseLexId();
  void parseSemi(spSemi &semi);
  spStringLiteral parseStringLiteral();

  // grammar
  void buildParseTree();
  void parseAnnotType(spAnnotType &annotType);
  void parseArgumentExprs(spArgumentExprs &argExprs);
  void parseArgumentExprsHelper(spArgumentExprs &argExprs);
  void parseBlock(spBlock &block);
  void parseBlockExpr(spBlockExpr &blockExpr);
  void parseBlockStat(spBlockStat &blockStat);
  void parseClassParents(spClassParents &classParents);
  void parseClassTemplate(spClassTemplate &classTmpl);
  void parseClassTemplateOpt(spClassTemplateOpt &classTmplOpt);
  void parseCompilationUnit();
  void parseConstr(spConstr &constr);
  void parseExpr(spExpr &expr);
  void parseExpr1(spExpr1 &expr1);
  void parseExprs(spExprs &exprs);
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
  void parseStableId(spStableId &stableId);
  void parseStableIdHead(spStableIdHead &head);
  void parseStableIdTail(spStableIdTail &tail);
  void parseTemplateBody(spTemplateBody &tmplBody);
  void parseTmplDef(spTmplDef &tmplDef);
  void parseTopStat(spTopStat &topStat);
  void parseTopStatSeq(spTopStatSeq &topStatSeq);

  // helper methods
  int addErr(int err);
  void saveState(State &state);
  void restoreState(State &state);

public:
  ScalaParser(const std::string filename, const std::string &buffer)
    : filename(filename), src(spSourceCodeStream(new SourceCodeStream(buffer))),
      compUnit(spCompilationUnit(new CompilationUnit)), error(0)
  {
    diag = spDiagnosis(new Diagnosis);
    lexer = spScalaLexer(new ScalaLexer(src, diag, indentMap));
  }

  spDiagnosis diag;
  spCompilationUnit compUnit;
  ScalaLineIndentationMap indentMap;
  int error;
  std::string error_msg;

  void parse();
};

} // namespace

#endif
