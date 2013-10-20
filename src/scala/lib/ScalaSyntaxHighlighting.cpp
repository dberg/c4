#include "c4/ScalaSyntaxHighlighting.h"

namespace c4s {

// ----------------------------------------------------------------------------
// Public interface
// ----------------------------------------------------------------------------

void ScalaSyntaxHighlighting::build() {
  // Compilation Unit
  sh << "[";

  for (auto tuple : compUnit->tuples) {
    setKeyword(std::get<0>(tuple));
    setQualId(std::get<1>(tuple));
    setSemi(std::get<2>(tuple));
  }

  if (compUnit->topStatSeq) {
    setTopStatSeq(compUnit->topStatSeq);
  }

  // Comments
  for (auto comment : comments) {
    sh << "(c4s-sh-comment "
      << (comment->ini + 1) << " "
      << (comment->end + 1) << ")";
  }

  sh << "]";
}

std::string ScalaSyntaxHighlighting::get() {
  return sh.str();
}

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------
void ScalaSyntaxHighlighting::setKeyword(spTokenNode &tok) {
  sh << "(c4s-sh-keyword " << (tok->ini + 1) << " " << (tok->end + 1) << ")";
}

void ScalaSyntaxHighlighting::setOp(spTokenNode &tok) {
  setOp(tok->ini + 1, tok->end + 1);
}

void ScalaSyntaxHighlighting::setOp(unsigned int ini, unsigned int end) {
  sh << "(c4s-sh-op " << ini << " " << end << ")";
}

// ----------------------------------------------------------------------------
// AST
// ----------------------------------------------------------------------------
void ScalaSyntaxHighlighting::setAnnotation(spAnnotation &annotation) {
  if (annotation->tokAt) {
    setOp(annotation->tokAt);
  }

  if (annotation->simpleType) {
    setSimpleType(annotation->simpleType);
  }

  for (auto argExprs : annotation->argExprsVec) {
    setArgumentExprs(argExprs);
  }
}

void ScalaSyntaxHighlighting::setAnnotType(spAnnotType &annotType) {
  if (annotType->simpleType) {
    setSimpleType(annotType->simpleType);
  }

  // TODO: std::vector<Annotation> annotations;
}

void ScalaSyntaxHighlighting::setArgumentExprs(spArgumentExprs &argExprs) {
  if (argExprs->opt == ArgumentExprs::Opt::EXPRS) {
    if (argExprs->tokLParen) {
      setOp(argExprs->tokLParen);
    }

    if (argExprs->exprs) {
      setExprs(argExprs->exprs);
    }

    if (argExprs->tokRParen) {
      setOp(argExprs->tokRParen);
    }

    return;
  }

  if (argExprs->opt == ArgumentExprs::Opt::EXPRS_POSTFIX_EXPR) {
    // TODO:
    return;
  }

  if (argExprs->opt == ArgumentExprs::Opt::BLOCK_EXPR) {
    if (argExprs->blockExpr) {
      setBlockExpr(argExprs->blockExpr);
    }
    return;
  }
}

void ScalaSyntaxHighlighting::setBlock(spBlock &block) {
  for (auto pair : block->paBlockStatSemi) {
    auto blockStat = pair.first;

    if (blockStat) {
      setBlockStat(blockStat);
    }

    auto semi = pair.second;
    if (semi) {
      setSemi(semi);
    }
  }
}

void ScalaSyntaxHighlighting::setBlockExpr(spBlockExpr &blockExpr) {
  if (blockExpr->opt == BlockExpr::Opt::CASE) {
    // TODO:
    return;
  }

  if (blockExpr->opt == BlockExpr::Opt::BLOCK) {
    if (blockExpr->tokLCurlyB) {
      setOp(blockExpr->tokLCurlyB);
    }

    if (blockExpr->block) {
      setBlock(blockExpr->block);
    }

    if (blockExpr->tokRCurlyB) {
      setOp(blockExpr->tokRCurlyB);
    }
    return;
  }
}

void ScalaSyntaxHighlighting::setBlockStat(spBlockStat &blockStat) {
  if (blockStat->opt == BlockStat::Opt::DEF) {
    // TODO:
    return;
  }

  if (blockStat->opt == BlockStat::Opt::TMPL_DEF) {
    // TODO:
    return;
  }

  if (blockStat->opt == BlockStat::Opt::EXPR1) {
    if (blockStat->expr1) {
      setExpr1(blockStat->expr1);
    }
    return;
  }
}

void ScalaSyntaxHighlighting::setClassDef(spClassDef &classDef) {
  if (classDef->id) {
    setLexId(classDef->id);
  }

  // TODO: [TypeParamClause]
  // TODO: {ConstrAnnotation}
  // TODO: [AccessModifier]
  // TODO: ClassParamClauses

  if (classDef->classTmplOpt) {
    setClassTemplateOpt(classDef->classTmplOpt);
  }
}

void ScalaSyntaxHighlighting::setClassParents(spClassParents &classParents) {
  if (classParents->constr) {
    setConstr(classParents->constr);
  }

  // TODO: std::vector<spTokenNode, spAnnotType> paTokNodeAnnotType;
}

void ScalaSyntaxHighlighting::setClassTemplate(spClassTemplate &classTmpl) {
  // TODO: spEarlyDefs earlyDefs;

  if (classTmpl->classParents) {
    setClassParents(classTmpl->classParents);
  }

  // TODO: spTemplateBody tmplBody;
}

void ScalaSyntaxHighlighting::setClassTemplateOpt(
  spClassTemplateOpt &classTmplOpt) {

  if (classTmplOpt->opt == ClassTemplateOpt::Opt::CLASS_TEMPLATE) {
    if (classTmplOpt->tokExtends) {
      setKeyword(classTmplOpt->tokExtends);
    }

    if (classTmplOpt->classTmpl) {
      setClassTemplate(classTmplOpt->classTmpl);
    }

    return;
  }

  if (classTmplOpt->opt == ClassTemplateOpt::Opt::TEMPLATE_BODY) {
    if (classTmplOpt->tokExtends) {
      setKeyword(classTmplOpt->tokExtends);
    }

    if (classTmplOpt->tmplBody) {
      setTemplateBody(classTmplOpt->tmplBody);
    }

    return;
  }
}

void ScalaSyntaxHighlighting::setCompoundType(spCompoundType &compoundType) {
  if (compoundType->opt == CompoundType::Opt::ANNOT_TYPE) {
    if (compoundType->annotType) { setAnnotType(compoundType->annotType); }
    // TODO: {'with' AnnotType} [Refinement]
    return;
  }

  if (compoundType->opt == CompoundType::Opt::REFINEMENT) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setConstr(spConstr &constr) {
  if (constr->annotType) {
    setAnnotType(constr->annotType);
  }

  for (auto argExprs : constr->argExprsVec) {
    setArgumentExprs(argExprs);
  }
}

void ScalaSyntaxHighlighting::setDef(spDef &def) {
  if (def->opt == Def::Opt::PAT_VAR_DEF) {
    // TODO:
    return;
  }

  if (def->opt == Def::Opt::DEF) {
    if (def->tokDef) { setKeyword(def->tokDef); }
    if (def->funDef) { setFunDef(def->funDef); }
    return;
  }

  if (def->opt == Def::Opt::TYPE) {
    // TODO:
    return;
  }

  if (def->opt == Def::Opt::TMPL_DEF) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setExpr(spExpr &expr) {
  if (expr->opt == Expr::Opt::EXPR) {
    // TODO:
    return;
  }

  if (expr->opt == Expr::Opt::EXPR1) {
    if (expr->expr1) {
      setExpr1(expr->expr1);
    }
    return;
  }
}

void ScalaSyntaxHighlighting::setExpr1(spExpr1 &expr1) {
  if (expr1->opt == Expr1::Opt::IF) {
    // TODO:
    return;
  }

  if (expr1->opt == Expr1::Opt::WHILE) {
    // TODO:
    return;
  }

  if (expr1->opt == Expr1::Opt::TRY) {
    // TODO:
    return;
  }

  if (expr1->opt == Expr1::Opt::DO) {
    // TODO:
    return;
  }

  if (expr1->opt == Expr1::Opt::FOR) {
    // TODO:
    return;
  }

  if (expr1->opt == Expr1::Opt::THROW) {
    // TODO:
    return;
  }

  if (expr1->opt == Expr1::Opt::RETURN) {
    // TODO:
    return;
  }

  if (expr1->opt == Expr1::Opt::ID_EQUALS_EXPR) {
    if (expr1->simpleExpr) { setSimpleExpr(expr1->simpleExpr); }
    if (expr1->tokPeriod) { setOp(expr1->tokPeriod); }
    if (expr1->id) { setLexId(expr1->id); }
    if (expr1->tokEquals) { setOp(expr1->tokEquals); }
    if (expr1->expr) { setExpr(expr1->expr); }
    return;
  }

  if (expr1->opt == Expr1::Opt::SIMPLE_EXPR1) {
    if (expr1->simpleExpr) { setSimpleExpr(expr1->simpleExpr); }
    if (expr1->tokPeriod) { setOp(expr1->tokPeriod); }
    if (expr1->id) { setLexId(expr1->id); }
    if (expr1->tokEquals) { setOp(expr1->tokEquals); }
    if (expr1->expr) { setExpr(expr1->expr); }
    return;
  }

  if (expr1->opt == Expr1::Opt::POSTFIX_EXPR) {
    if (expr1->postfixExpr) {
      setPostfixExpr(expr1->postfixExpr);
    }
    return;
  }

  if (expr1->opt == Expr1::Opt::POSTFIX_EXPR_ASCRIPTION) {
    // TODO:
    return;
  }

  if (expr1->opt == Expr1::Opt::POSTFIX_EXPR_MATCH) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setExprs(spExprs &exprs) {
  if (exprs->expr) {
    setExpr(exprs->expr);
  }

  for (auto pair : exprs->pairs) {
    setOp(pair.first + 1, pair.first + 2);
    setExpr(pair.second);
  }
}

void ScalaSyntaxHighlighting::setFunDef(spFunDef &funDef) {
  if (funDef->opt == FunDef::Opt::FUN_SIG_EQUALS_EXPR) {
    // FunSig [':' Type] '=' Expr
    if (funDef->funSig) { setFunSig(funDef->funSig); }
    if (funDef->tokColon) { setOp(funDef->tokColon); }
    if (funDef->type) { setType(funDef->type); }
    if (funDef->tokEquals) { setOp(funDef->tokEquals); }
    if (funDef->expr) { setExpr(funDef->expr); }
  }

  if (funDef->opt == FunDef::Opt::FUN_SIG_BLOCK) {
    // TODO: FunSig [nl] '{' Block '}'
  }

  if (funDef->opt == FunDef::Opt::THIS_PARAM_CLAUSE) {
    // TODO: 'this' ParamClause ParamClauses
    //       ('=' ConstrExpr | [nl] ConstrBlock)
  }
}

void ScalaSyntaxHighlighting::setFunSig(spFunSig &funSig) {
  if (funSig->id) { setLexId(funSig->id); }
  // TODO: [FunTypeParamClause]

  if (funSig->paramClauses) {
    setParamClauses(funSig->paramClauses);
  }
}

void ScalaSyntaxHighlighting::setIdPeriod(spIdPeriod &idPeriod) {
  // TODO:
}

void ScalaSyntaxHighlighting::setImport(spImport &import) {
  if (import->tokImport) {
    setKeyword(import->tokImport);
  }

  if (import->importExpr) {
    setImportExpr(import->importExpr);
  }

  for (auto pair : import->pairs) {
    setOp(pair.first);
    setImportExpr(pair.second);
  }
}

void ScalaSyntaxHighlighting::setImportExpr(spImportExpr &importExpr) {
  if (importExpr->qualId) {
    setQualId(importExpr->qualId);
  }

  if (importExpr->tokPeriod) {
    setOp(importExpr->tokPeriod);
    if (importExpr->tokUnderscore) {
      setOp(importExpr->tokUnderscore);
    }
    return;
  }

  // TODO:
  //if (importExpr->importSelectors) {
  //  setImportSelectors(importExpr->importSelectors);
  //}
}

void ScalaSyntaxHighlighting::setInfixExpr(spInfixExpr &infixExpr) {
  if (infixExpr->opt == InfixExpr::Opt::PREFIX) {
    if (infixExpr->prefixExpr) {
      setPrefixExpr(infixExpr->prefixExpr);
    }
    return;
  }

  if (infixExpr->opt == InfixExpr::Opt::INFIX) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setInfixType(spInfixType &infixType) {
  if (infixType->compoundType) {
    setCompoundType(infixType->compoundType);
  }

  // TODO: {id [nl] CompoundType}
}

void ScalaSyntaxHighlighting::setIntegerLiteral(spIntegerLiteral &intLit) {
  sh << "(c4s-sh-literal-number "
     << (intLit->ini + 1)
     << " "
     << (intLit->end + 1)
     << ")";
}

void ScalaSyntaxHighlighting::setLexId(spLexId &id) {
  sh << "(c4s-sh-identifier " << (id->ini + 1) << " " << (id->end + 1) << ")";
}

void ScalaSyntaxHighlighting::setLiteral(spLiteral &literal) {
  if (literal->opt == Literal::Opt::INTEGER) {
    if (literal->intLit) { setIntegerLiteral(literal->intLit); }
    return;
  }

  if (literal->opt == Literal::Opt::FLOATING_POINT) {
    // TODO:
    return;
  }

  if (literal->opt == Literal::Opt::BOOLEAN) {
    // TODO:
    return;
  }

  if (literal->opt == Literal::Opt::CHARACTER) {
    // TODO:
    return;
  }

  if (literal->opt == Literal::Opt::STRING) {
    if (literal->strLit) { setStringLiteral(literal->strLit); }
    return;
  }

  if (literal->opt == Literal::Opt::SYMBOL) {
    // TODO:
    return;
  }

  if (literal->opt == Literal::Opt::NULL_LITERAL) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setObjectDef(spObjectDef &objectDef) {
  if (objectDef->id) {
    setLexId(objectDef->id);
  }

  if (objectDef->classTmplOpt) {
    setClassTemplateOpt(objectDef->classTmplOpt);
  }
}

void ScalaSyntaxHighlighting::setPackaging(spPackaging &packing) {
  if (packing->tokPackage) {
    setKeyword(packing->tokPackage);
  }

  if (packing->qualId) {
    setQualId(packing->qualId);
  }
}

void ScalaSyntaxHighlighting::setParam(spParam &param) {
  for (auto annotation : param->annotations) {
    setAnnotation(annotation);
  }

  if (param->id) { setLexId(param->id); }
  if (param->tokColon) { setOp(param->tokColon); }
  if (param->paramType) { setParamType(param->paramType); }
  if (param->tokEquals) { setOp(param->tokEquals); }
  if (param->expr) { setExpr(param->expr); }
}

void ScalaSyntaxHighlighting::setParams(spParams &params) {
  if (params->param) { setParam(params->param); }

  for (auto pair : params->pairs) {
    setOp(pair.first);
    setParam(pair.second);
  }
}

void ScalaSyntaxHighlighting::setParamClause(spParamClause &paramClause) {
  if (paramClause->tokLParen) { setOp(paramClause->tokLParen); }
  if (paramClause->params) { setParams(paramClause->params); }
  if (paramClause->tokRParen) { setOp(paramClause->tokRParen); }
}

void ScalaSyntaxHighlighting::setParamClauses(spParamClauses &paramClauses) {
  for (auto paramClause : paramClauses->paramClauses) {
    setParamClause(paramClause);
  }

  // TODO: '(' 'implicit' Params ')']
}

void ScalaSyntaxHighlighting::setParamType(spParamType &paramType) {
  if (paramType->opt == ParamType::Opt::TYPE) {
    if (paramType->type) { setType(paramType->type); }
    return;
  }

  if (paramType->opt == ParamType::Opt::ARROW_TYPE) {
    // TODO:
    return;
  }

  if (paramType->opt == ParamType::Opt::TYPE_STAR) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setPath(spPath &path) {
  if (path->opt == Path::Opt::STABLE_ID) {
    if (path->stableId) {
      setStableId(path->stableId);
    }
    return;
  }

  if (path->opt == Path::Opt::THIS) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setPeriodId(spPeriodId &periodId) {
  if (periodId->tok) {
    setOp(periodId->tok);
  }

  if (periodId->id) {
    setLexId(periodId->id);
  }
}

void ScalaSyntaxHighlighting::setPostfixExpr(spPostfixExpr &postfixExpr) {
  if (postfixExpr->infixExpr) {
    setInfixExpr(postfixExpr->infixExpr);
  }

  // TODO: if (postfixExpr->id) { setLexId(postfixExpr->id); }
}

void ScalaSyntaxHighlighting::setPrefixExpr(spPrefixExpr &prefixExpr) {
  if (prefixExpr->tok) { setOp(prefixExpr->tok); }
  if (prefixExpr->simpleExpr) { setSimpleExpr(prefixExpr->simpleExpr); }
}

void ScalaSyntaxHighlighting::setQualId(spQualId &qualId) {
  if (qualId->id) {
    setLexId(qualId->id);
  }

  for (auto periodId : qualId->periodIds) {
    setPeriodId(periodId);
  }
}

void ScalaSyntaxHighlighting::setSemi(spSemi &semi) {
  if (semi->tokSemiColon) {
    setOp(semi->tokSemiColon);
  }
}

void ScalaSyntaxHighlighting::setSimpleExpr(spSimpleExpr &simpleExpr) {
  if (simpleExpr->opt == SimpleExpr::Opt::NEW) {
    if (simpleExpr->tokNew) { setKeyword(simpleExpr->tokNew); }
    if (simpleExpr->classTmpl) { setClassTemplate(simpleExpr->classTmpl); }
    if (simpleExpr->tmplBody) { setTemplateBody(simpleExpr->tmplBody); }
    return;
  }

  if (simpleExpr->opt == SimpleExpr::Opt::BLOCK_EXPR) {
    // TODO:
    return;
  }

  if (simpleExpr->opt == SimpleExpr::Opt::SIMPLE_EXPR1) {
    if (simpleExpr->simpleExpr1) {
      setSimpleExpr1(simpleExpr->simpleExpr1);
    }
    return;
  }
}

void ScalaSyntaxHighlighting::setSimpleExpr1(spSimpleExpr1 &simpleExpr1) {
  if (simpleExpr1->head) {
    setSimpleExpr1Head(simpleExpr1->head);
  }

  if (simpleExpr1->tail) {
    setSimpleExpr1Tail(simpleExpr1->tail);
  }
}

void ScalaSyntaxHighlighting::setSimpleExpr1Head(spSimpleExpr1Head &head) {
  if (head->opt == SimpleExpr1Head::Opt::LITERAL) {
    if (head->literal) {
      setLiteral(head->literal);
    }

    return;
  }

  if (head->opt == SimpleExpr1Head::Opt::PATH) {
    if (head->path) {
      setPath(head->path);
    }
    return;
  }

  if (head->opt == SimpleExpr1Head::Opt::UNDERSCORE) {
    // TODO:
    return;
  }

  if (head->opt == SimpleExpr1Head::Opt::EXPRS) {
    // TODO:
    return;
  }

  if (head->opt == SimpleExpr1Head::Opt::SIMPLE_EXPR_ID) {
    // TODO:
    return;
  }

  if (head->opt == SimpleExpr1Head::Opt::SIMPLE_EXPR_TYPE_ARGS) {
    // TODO:
    return;
  }

  if (head->opt == SimpleExpr1Head::Opt::XMLEXPR) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setSimpleExpr1Tail(spSimpleExpr1Tail &tail) {
  if (tail->argExprs) {
    setArgumentExprs(tail->argExprs);
  }

  if (tail->tail) {
    setSimpleExpr1Tail(tail->tail);
  }
}

void ScalaSyntaxHighlighting::setSimpleType(spSimpleType &simpleType) {
  if (simpleType->head) {
    setSimpleTypeHead(simpleType->head);
  }

  if (simpleType->tails) {
    setSimpleTypeTails(simpleType->tails);
  }
}

void ScalaSyntaxHighlighting::setSimpleTypeHead(spSimpleTypeHead &head) {
  if (head->opt == SimpleTypeHead::Opt::STABLE_ID) {
    if (head->stableId) {
      setStableId(head->stableId);
    }
    return;
  }

  // TODO:
  //if (head->opt == SimpleTypeHead::Opt::PATH_TYPE) {
  //  return;
  //}

  // TODO:
  //if (head->opt == SimpleTypeHead::Opt::TYPES) {
  //  return;
  //}
}

void ScalaSyntaxHighlighting::setSimpleTypeTails(spSimpleTypeTails &tails) {
  // TODO:
}

void ScalaSyntaxHighlighting::setStableId(spStableId &stableId) {
  if (stableId->head) {
    setStableIdHead(stableId->head);
  }

  if (stableId->tail) {
    setStableIdTail(stableId->tail);
  }
}

void ScalaSyntaxHighlighting::setStableIdHead(spStableIdHead &head) {
  if (head->opt == StableIdHead::Opt::ID) {
    if (head->id) {
      setLexId(head->id);
    }

    return;
  }

  if (head->opt == StableIdHead::Opt::THIS) {
    if (head->idPeriod) {
      setIdPeriod(head->idPeriod);
    }

    if (head->tokThis) {
      setKeyword(head->tokThis);
    }

    if (head->periodId) {
      setPeriodId(head->periodId);
    }

    return;
  }

  if (head->opt == StableIdHead::Opt::SUPER) {
    if (head->idPeriod) {
      setIdPeriod(head->idPeriod);
    }

    if (head->tokSuper) {
      setKeyword(head->tokSuper);
    }

    // TODO:
    //if (head->classQualifier) {
    //  setClassQualifier(head->classQualifier);
    //}

    if (head->periodId) {
      setPeriodId(head->periodId);
    }

    return;
  }
}

void ScalaSyntaxHighlighting::setStableIdTail(spStableIdTail &tail) {
  if (tail->periodId) {
    setPeriodId(tail->periodId);
  }

  if (tail->tail) {
    setStableIdTail(tail->tail);
  }
}

void ScalaSyntaxHighlighting::setStringLiteral(spStringLiteral &strLit) {
  sh << "(c4s-sh-string-literal "
     << (strLit->ini + 1)
     << " "
     << (strLit->end + 1)
     << ")";
}

void ScalaSyntaxHighlighting::setTemplateBody(spTemplateBody &tmplBody) {
  if (tmplBody->tokLCurlyB) { setOp(tmplBody->tokLCurlyB); }
  // TODO: SelfType
  if (tmplBody->tmplStat) { setTemplateStat(tmplBody->tmplStat); }
  // TODO: {semi TemplateStat}
  if (tmplBody->tokRCurlyB) { setOp(tmplBody->tokRCurlyB); }
}

void ScalaSyntaxHighlighting::setTemplateStat(spTemplateStat &tmplStat) {
  if (tmplStat->opt == TemplateStat::Opt::IMPORT) {
    // TODO:
    return;
  }

  if (tmplStat->opt == TemplateStat::Opt::DEF) {
    if (tmplStat->def) { setDef(tmplStat->def); }
    return;
  }

  if (tmplStat->opt == TemplateStat::Opt::DCL) {
    // TODO:
    return;
  }

  if (tmplStat->opt == TemplateStat::Opt::EXPR) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setTmplDef(spTmplDef &tmplDef) {
  if (tmplDef->opt == TmplDef::Opt::CASE_CLASS) {
    // 'case'
    if (tmplDef->tokCase) {
      setKeyword(tmplDef->tokCase);
    }

    if (tmplDef->tokClass) {
      setKeyword(tmplDef->tokClass);
    }

    if (tmplDef->classDef) {
      setClassDef(tmplDef->classDef);
    }

    return;
  }

  if (tmplDef->opt == TmplDef::Opt::CASE_OBJECT) {
    if (tmplDef->tokCase) {
      setKeyword(tmplDef->tokCase);
    }

    if (tmplDef->tokObject) {
      setKeyword(tmplDef->tokObject);
    }

    if (tmplDef->objectDef) {
      setObjectDef(tmplDef->objectDef);
    }

    return;
  }

  if (tmplDef->opt == TmplDef::Opt::TRAIT) {
    if (tmplDef->tokTrait) {
      setKeyword(tmplDef->tokTrait);
    }

    if (tmplDef->traitDef) {
      setTraitDef(tmplDef->traitDef);
    }

    return;
  }
}

void ScalaSyntaxHighlighting::setTopStat(spTopStat &topStat) {
  if (topStat->opt == TopStat::Opt::TMPL_DEF) {
    // {Annotation [nl]}
    for (auto annotation : topStat->annotations) {
      setAnnotation(annotation);
    }

    // TODO: {Modifier}

    if (topStat->tmplDef) {
      setTmplDef(topStat->tmplDef);
    }
    return;
  }

  if (topStat->opt == TopStat::Opt::IMPORT) {
    setImport(topStat->import);
    return;
  }

  if (topStat->opt == TopStat::Opt::PACKAGING) {
    if (topStat->packaging) {
      setPackaging(topStat->packaging);
    }
    return;
  }

  if (topStat->opt == TopStat::Opt::PACKAGE_OBJECT) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setTopStatSeq(spTopStatSeq &topStatSeq) {
  if (topStatSeq->topStat) {
    setTopStat(topStatSeq->topStat);
  }

  for (auto pair : topStatSeq->pairs) {
    setSemi(pair.first);
    setTopStat(pair.second);
  }
}

void ScalaSyntaxHighlighting::setTraitDef(spTraitDef &traitDef) {
  if (traitDef->id) {
    setLexId(traitDef->id);
  }

  if (traitDef->typeParamClause) {
    setTypeParamClause(traitDef->typeParamClause);
  }

  if (traitDef->traitTemplateOpt) {
    setTraitTemplateOpt(traitDef->traitTemplateOpt);
  }
}

void ScalaSyntaxHighlighting::setTraitParents(spTraitParents &parents) {
  if (parents->annotType) {
    setAnnotType(parents->annotType);
  }

  for (auto pair : parents->pairs) {
    setKeyword(pair.first);
    setAnnotType(pair.second);
  }
}

void ScalaSyntaxHighlighting::setTraitTemplate(spTraitTemplate &traitTemplate) {
  // TODO: [EarlyDefs]

  if (traitTemplate->traitParents) {
    setTraitParents(traitTemplate->traitParents);
  }

  // TODO: [TemplateBody]
}

void ScalaSyntaxHighlighting::setTraitTemplateOpt(
  spTraitTemplateOpt &traitTemplateOpt) {

  if (traitTemplateOpt->opt == TraitTemplateOpt::Opt::TRAIT_TEMPLATE) {
    if (traitTemplateOpt->tokExtends) {
      setKeyword(traitTemplateOpt->tokExtends);
    }

    if (traitTemplateOpt->traitTemplate) {
      setTraitTemplate(traitTemplateOpt->traitTemplate);
    }

    return;
  }

  if (traitTemplateOpt->opt == TraitTemplateOpt::Opt::TEMPLATE_BODY) {
    if (traitTemplateOpt->tokExtends) {
      setKeyword(traitTemplateOpt->tokExtends);
    }

    if (traitTemplateOpt->templateBody) {
      setTemplateBody(traitTemplateOpt->templateBody);
    }
  }
}

void ScalaSyntaxHighlighting::setType(spType &type) {
  if (type->opt == Type::Opt::FUNC_ARG_TYPES) {
    // TODO:
    return;
  }

  if (type->opt == Type::Opt::INFIX_TYPE) {
    if (type->infixType) { setInfixType(type->infixType); }
    // TODO: [ExistentialClause]
    return;
  }
}

void ScalaSyntaxHighlighting::setTypeParam(spTypeParam &typeParam) {
  if (typeParam->id) {
    setLexId(typeParam->id);
  }

  if (typeParam->tokUnderscore) {
    setOp(typeParam->tokUnderscore);
  }

  if (typeParam->typeParamClause) {
    setTypeParamClause(typeParam->typeParamClause);
  }

  // TODO: ['>:' Type] ['<:' Type]
  // TODO: {'<%' Type} {':' Type}
}

void ScalaSyntaxHighlighting::setTypeParamClause(
  spTypeParamClause &typeParamClause) {

  if (typeParamClause->tokLBracket) {
    setOp(typeParamClause->tokLBracket);
  }

  if (typeParamClause->varTypeParam) {
    setVariantTypeParam(typeParamClause->varTypeParam);
  }

  for (auto pair : typeParamClause->pairs) {
    auto comma = pair.first;
    setOp(comma);

    auto varTypeParam = pair.second;
    setVariantTypeParam(varTypeParam);
  }

  if (typeParamClause->tokLBracket) {
    setOp(typeParamClause->tokLBracket);
  }
}

void ScalaSyntaxHighlighting::setVariantTypeParam(
  spVariantTypeParam &varTypeParam) {

  for (auto annotation : varTypeParam->annotations) {
    setAnnotation(annotation);
  }

  if (varTypeParam->tokPlus) { setOp(varTypeParam->tokPlus); }
  if (varTypeParam->tokMinus) { setOp(varTypeParam->tokMinus); }
  if (varTypeParam->typeParam) { setTypeParam(varTypeParam->typeParam); }
}

} // namespace
