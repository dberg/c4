//-*- C++ -*-
#ifndef __SCALA_PARSER_H__
#define __SCALA_PARSER_H__
#include <string>
#include "c4/common/SourceCodeStream.h"
#include "c4/common/Diagnosis.h"
#include "c4/scala/AST.h"
#include "c4/scala/Lexer.h"
#include "c4/scala/ParserState.h"
#include "c4/scala/Token.h"

namespace c4s {

class Parser {
  const std::string filename;
  c4::spSourceCodeStream src;
  spLexer lexer;

  // lexical grammar
  spLexId parseLexId();
  void parseSemi(spSemi &semi);
  spStringLiteral parseStringLiteral();

  // grammar
  void buildParseTree();
  void parseAccessModifier(spAccessModifier &accessModifier);
  void parseAccessQualifier(spAccessQualifier &accessQual);
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
  void parseCompoundType(spCompoundType &compoundType);
  void parseDef(spDef &def);
  void parseFunDef(spFunDef &funDef);
  void parseFunSig(spFunSig &funSig);
  void parseFunTypeParamClause(spFunTypeParamClause &funTypeParamClause);
  void parseExpr(spExpr &expr);
  void parseExpr1(spExpr1 &expr1);
  void parseExprs(spExprs &exprs);
  void parseImport(spImport &import);
  void parseImportExpr(spImportExpr &importExpr);
  void parseImportSelector(spImportSelector &importSelector);
  void parseImportSelectors(spImportSelectors &importSelectors);
  void parseInfixExpr(spInfixExpr &infixExpr);
  void parseInfixType(spInfixType &infixType);
  void parseLiteral(spLiteral &literal);
  void parseLocalModifier(spLocalModifier &localModifier);
  void parseModifier(spModifier &modifier);
  void parseObjectDef(spObjectDef &objectDef);
  void parsePackaging(spPackaging &packaging);
  void parseParam(spParam &param);
  void parseParams(spParams &params);
  void parseParamClause(spParamClause &paramClause);
  void parseParamClauses(spParamClauses &paramClauses);
  void parseParamType(spParamType &paramType);
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
  void parseType(spType &type);
  void parseTypes(spTypes &types);
  void parseTypeArgs(spTypeArgs &typeArgs);
  void parseTypeParam(spTypeParam &typeParam);
  void parseTypeParamClause(spTypeParamClause &typeParamClause);
  void parseVariantTypeParam(spVariantTypeParam &varTypeParam);

  // helper methods
  int addErr(int err);
  void saveState(State &state);
  void restoreState(State &state);
  bool isAccessModifier(STok tok);
  bool isLocalModifier(STok tok);
  bool isModifier(STok tok);

public:
  Parser(const std::string filename, const std::string &buffer)
    : filename(filename),
      src(c4::spSourceCodeStream(new c4::SourceCodeStream(buffer))),
      compUnit(spCompilationUnit(new CompilationUnit))
  {
    diag = c4::spDiagnosis(new c4::Diagnosis);
    lexer = spLexer(new Lexer(src, diag, indentMap));
  }

  c4::spDiagnosis diag;
  spCompilationUnit compUnit;
  std::vector<spComment> comments;
  LineIndentationMap indentMap;

  void parse();
};

} // namespace

#endif
