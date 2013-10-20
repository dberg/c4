//-*- C++ -*-
#ifndef __SCALA_SYNTAX_HIGHLIGHTING_H__
#define __SCALA_SYNTAX_HIGHLIGHTING_H__
#include <sstream>
#include <vector>
#include "c4/ScalaAST.h"

namespace c4s {

class ScalaSyntaxHighlighting {
  spCompilationUnit &compUnit;
  std::vector<spComment> &comments;

  void setKeyword(spTokenNode &tok);
  void setOp(spTokenNode &tok);
  void setOp(unsigned int ini, unsigned int end);

  void setAnnotation(spAnnotation &annotation);
  void setAnnotType(spAnnotType &annotType);
  void setArgumentExprs(spArgumentExprs &argExprs);
  void setBlock(spBlock &block);
  void setBlockExpr(spBlockExpr &blockExpr);
  void setBlockStat(spBlockStat &blockStat);
  void setClassDef(spClassDef &classDef);
  void setClassParents(spClassParents &classParents);
  void setClassTemplate(spClassTemplate &classTmpl);
  void setClassTemplateOpt(spClassTemplateOpt &classTmplOpt);
  void setCompoundType(spCompoundType &compoundType);
  void setConstr(spConstr &constr);
  void setDef(spDef &def);
  void setExpr(spExpr &expr);
  void setExpr1(spExpr1 &expr1);
  void setExprs(spExprs &exprs);
  void setFunDef(spFunDef &funDef);
  void setFunSig(spFunSig &funSig);
  void setIdPeriod(spIdPeriod &idPeriod);
  void setImport(spImport &import);
  void setImportExpr(spImportExpr &importExpr);
  void setInfixExpr(spInfixExpr &infixExpr);
  void setInfixType(spInfixType &infixType);
  void setIntegerLiteral(spIntegerLiteral &intLit);
  void setLexId(spLexId &id);
  void setLiteral(spLiteral &literal);
  void setObjectDef(spObjectDef &objectDef);
  void setPackaging(spPackaging &packing);
  void setParam(spParam &param);
  void setParams(spParams &params);
  void setParamClause(spParamClause &paramClause);
  void setParamClauses(spParamClauses &paramClauses);
  void setParamType(spParamType &paramType);
  void setPath(spPath &path);
  void setPeriodId(spPeriodId &periodId);
  void setPostfixExpr(spPostfixExpr &postfixExpr);
  void setPrefixExpr(spPrefixExpr &prefixExpr);
  void setQualId(spQualId &qualId);
  void setSemi(spSemi &semi);
  void setSimpleExpr(spSimpleExpr &simpleExpr);
  void setSimpleExpr1(spSimpleExpr1 &simpleExpr1);
  void setSimpleExpr1Head(spSimpleExpr1Head &head);
  void setSimpleExpr1Tail(spSimpleExpr1Tail &tail);
  void setSimpleType(spSimpleType &simpleType);
  void setSimpleTypeHead(spSimpleTypeHead &head);
  void setSimpleTypeTails(spSimpleTypeTails &tails);
  void setStableId(spStableId &stableId);
  void setStableIdHead(spStableIdHead &head);
  void setStableIdTail(spStableIdTail &tail);
  void setStringLiteral(spStringLiteral &strLit);
  void setTemplateBody(spTemplateBody &tmplBody);
  void setTemplateStat(spTemplateStat &tmplStat);
  void setTmplDef(spTmplDef &tmplDef);
  void setTopStat(spTopStat &topStat);
  void setTopStatSeq(spTopStatSeq &topStatSeq);
  void setTraitDef(spTraitDef &traitDef);
  void setTraitParents(spTraitParents &parents);
  void setTraitTemplate(spTraitTemplate &traitTemplate);
  void setTraitTemplateOpt(spTraitTemplateOpt &traitTemplateOpt);
  void setType(spType &type);
  void setTypeParam(spTypeParam &typeParam);
  void setTypeParamClause(spTypeParamClause &typeParamClause);
  void setVariantTypeParam(spVariantTypeParam &varTypeParam);

  // Emacs output
  std::stringstream sh;
public:

  ScalaSyntaxHighlighting(
    spCompilationUnit &compUnit, std::vector<spComment> &comments)
    : compUnit(compUnit), comments(comments) {}
  void build();
  std::string get();
};

} // namespace

#endif
