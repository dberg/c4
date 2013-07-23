#include "djp/ScalaParser.h"

namespace djp {

// -----------------------------------------------------------------------------
// Lexical grammar
// -----------------------------------------------------------------------------
spLexId ScalaParser::parseLexId() {
  spLexId lId = spLexId(new LexId);
  if (lexer->getCurToken() != STok::ID) {
    lId->addErr(ERR_EXP_IDENTIFIER);
    return lId;
  }

  lId->ini = lexer->getCurTokenIni();
  lId->end = lexer->getCurTokenEnd();
  lId->id = lexer->getCurTokenStr();
  return lId;
}

// -----------------------------------------------------------------------------
// Grammar
// -----------------------------------------------------------------------------

/**
 * AnnotType ::= SimpleType {Annotation}
 */
void ScalaParser::parseAnnotType(spAnnotType &annotType) {
  annotType->simpleType = spSimpleType(new SimpleType);
  parseSimpleType(annotType->simpleType);
  if (annotType->simpleType->err) {
    annotType->addErr(-1);
    return;
  }

  // TODO: {Annotation}
}

/**
 * ArgumentExprs ::= ‘(’ [Exprs] ‘)’
 *                 | ‘(’ [Exprs ‘,’] PostfixExpr ‘:’ ‘_’ ‘*’ ’)’
 *                 | [nl] BlockExpr
 */
void ScalaParser::parseArgumentExprs(spArgumentExprs &argExprs) {
  // TODO: ‘(’ [Exprs] ‘)’
  // TODO: ‘(’ [Exprs ‘,’] PostfixExpr ‘:’ ‘_’ ‘*’ ’)’
  // TODO: [nl]

  argExprs->opt = ArgumentExprs::Opt::BLOCK_EXPR;
  argExprs->blockExpr = spBlockExpr(new BlockExpr);
  parseBlockExpr(argExprs->blockExpr);
  if (argExprs->blockExpr->err) {
    argExprs->addErr(-1);
  }
}

/**
 * Block ::= {BlockStat semi} [ResultExpr]
 */
void ScalaParser::parseBlock(spBlock &block) {
  // TODO:
}

/**
 * BlockExpr ::= ‘{’ CaseClauses ‘}’
 *             | ‘{’ Block ‘}’
 */
void ScalaParser::parseBlockExpr(spBlockExpr &blockExpr) {
  // TODO: CaseClauses

  // '{'
  spTokenNode tokLCurlyB = lexer->getCurTokenNode();
  if (tokLCurlyB->tok != STok::LCURLYB) {
    blockExpr->addErr(addErr(ERR_EXP_LCURLY_BRACKET));
    return;
  }

  blockExpr->tokLCurlyB = tokLCurlyB;

  blockExpr->opt = BlockExpr::Opt::BLOCK;
  blockExpr->block = spBlock(new Block);
  parseBlock(blockExpr->block);
  if (blockExpr->block->err) {
    blockExpr->addErr(-1);
  }

  spTokenNode tokRCurlyB = lexer->getCurTokenNode();
  if (tokRCurlyB->tok != STok::RCURLYB) {
    blockExpr->addErr(addErr(ERR_EXP_RCURLY_BRACKET));
    return;
  }

  blockExpr->tokRCurlyB = tokRCurlyB;
}

/**
 * ClassParents ::= Constr {‘with’ AnnotType}
 */
void ScalaParser::parseClassParents(spClassParents &classParents) {
  classParents->constr = spConstr(new Constr);
  parseConstr(classParents->constr);
  if (classParents->constr->err) {
    classParents->constr->addErr(-1);
    return;
  }

  // TODO: {‘with’ AnnotType}
}

/**
 * ClassTemplate ::= [EarlyDefs] ClassParents [TemplateBody]
 */
void ScalaParser::parseClassTemplate(spClassTemplate &classTmpl) {
  // TODO: [EarlyDefs]

  classTmpl->classParents = spClassParents(new ClassParents);
  parseClassParents(classTmpl->classParents);
  if (classTmpl->classParents->err) {
    classTmpl->addErr(-1);
    return;
  }

  // TODO: [TemplateBody]
}

/**
 * ClassTemplateOpt ::= ‘extends’ ClassTemplate
 *                    | [[‘extends’] TemplateBody]
 */
void ScalaParser::parseClassTemplateOpt(spClassTemplateOpt &classTmplOpt) {
  if (lexer->getCurToken() == STok::EXTENDS) {
    classTmplOpt->tokExtends = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'extends'

    // We have to decide if we have a ClassTemplate or a TemplateBody, so we
    // first try ClassTemplate and if there's an error we try TemplateBody next.
    spClassTemplate classTmpl = spClassTemplate(new ClassTemplate);
    parseClassTemplate(classTmpl);
    if (classTmpl->err == false) {
      classTmplOpt->opt = ClassTemplateOpt::Opt::CLASS_TEMPLATE;
      classTmplOpt->classTmpl = classTmpl;
      return;
    }
  }

  // TemplateBody
  classTmplOpt->opt = ClassTemplateOpt::Opt::TEMPLATE_BODY;
  classTmplOpt->tmplBody = spTemplateBody(new TemplateBody);
  parseTemplateBody(classTmplOpt->tmplBody);
  if (classTmplOpt->tmplBody->err) {
    classTmplOpt->addErr(-1);
  }
}

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
 * Constr ::= AnnotType {ArgumentExprs}
 */
void ScalaParser::parseConstr(spConstr &constr) {
  constr->annotType = spAnnotType(new AnnotType);
  parseAnnotType(constr->annotType);
  if (constr->annotType->err) {
    constr->addErr(-1);
    return;
  }

  // {ArgumentExprs}
  State state;
  while (true) {
    saveState(state);
    spArgumentExprs argExprs = spArgumentExprs(new ArgumentExprs);
    parseArgumentExprs(argExprs);
    if (argExprs->err) {
      restoreState(state);
      return;
    }

    constr->argExprs.push_back(argExprs);
  }
}

/**
 * ObjectDef ::= id ClassTemplateOpt
 */
void ScalaParser::parseObjectDef(spObjectDef &objectDef) {
  objectDef->lId = parseLexId();
  if (objectDef->lId->err) {
    objectDef->addErr(-1);
    return;
  }

  objectDef->classTmplOpt = spClassTemplateOpt(new ClassTemplateOpt);
  parseClassTemplateOpt(objectDef->classTmplOpt);
  if (objectDef->classTmplOpt->err) {
    objectDef->addErr(-1);
  }
}

/**
 * SimpleType ::= SimpleType TypeArgs
 *              | SimpleType ‘#’ id
 *              | StableId
 *              | Path ‘.’ ‘type’
 *              | ‘(’ Types ’)’
 */
void ScalaParser::parseSimpleType(spSimpleType &simpleType) {
  // TODO: SimpleType TypeArgs
  // TODO: SimpleType ‘#’ id

  simpleType->opt = SimpleType::Opt::STABLE_ID;
  simpleType->stableId = spStableId(new StableId);
  parseStableId(simpleType->stableId);
  if (simpleType->stableId->err) {
    simpleType->addErr(-1);
    return;
  }

  // TODO: Path ‘.’ ‘type’
  // TODO: ‘(’ Types ’)’
}

/**
 * StableId ::= id
 *            | Path ‘.’ id
 *            | [id ’.’] ‘super’ [ClassQualifier] ‘.’ id
 */
void ScalaParser::parseStableId(spStableId &stableId) {
  spLexId id = parseLexId();
  if (id->err == false) {
    stableId->opt = StableId::Opt::ID;
    stableId->id = id;
    return;
  }

  // TODO: Path ‘.’ id
  // TODO: [id ’.’] ‘super’ [ClassQualifier] ‘.’ id
}

/**
 * TemplateBody ::= [nl] ‘{’ [SelfType] TemplateStat {semi TemplateStat} ‘}’
 */
void ScalaParser::parseTemplateBody(spTemplateBody &tmplBody) {

}

/**
 * TmplDef ::= [‘case’] ‘class’ ClassDef
 *           | [‘case’] ‘object’ ObjectDef
 *           | ‘trait’ TraitDef
 */
void ScalaParser::parseTmplDef(spTmplDef &tmplDef) {
  // ‘case’
  if (lexer->getCurToken() == STok::CASE) {
    tmplDef->tokCase = lexer->getCurTokenNode();
    lexer->getNextToken();
  }

  // TODO: [‘case’] ‘class’ ClassDef
  if (lexer->getCurToken() == STok::CLASS) {
    // TODO:
    return;
  }

  // [‘case’] ‘object’ ObjectDef
  if (lexer->getCurToken() == STok::OBJECT) {
    tmplDef->opt = TmplDef::Opt::CASE_OBJECT;

    tmplDef->tokObject = lexer->getCurTokenNode();
    lexer->getNextToken();

    tmplDef->objectDef = spObjectDef(new ObjectDef);
    parseObjectDef(tmplDef->objectDef);
    if (tmplDef->objectDef->err) {
      tmplDef->addErr(-1);
    }
    return;
  }

  // TODO: ‘trait’ TraitDef
}

/**
 * TopStat ::= {Annotation [nl]} {Modifier} TmplDef
 *           | Import
 *           | Packaging
 *           | PackageObject
 */
void ScalaParser::parseTopStat(spTopStat &topStat) {
  // TODO: {Annotation [nl]} {Modifier}
  topStat->opt = TopStat::Opt::ANNOTATION;
  topStat->tmplDef = spTmplDef(new TmplDef);
  parseTmplDef(topStat->tmplDef);

  // TODO: Import
  // TODO: Packaging
  // TODO:PackageObject
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

// -----------------------------------------------------------------------------
// Helper methods
// -----------------------------------------------------------------------------

/**
 * Add Diagnosis error including buffer ini and end positions.
 */
int ScalaParser::addErr(int err) {
  unsigned int cursor = src->getCursor();
  unsigned int ini = cursor - lexer->getCurTokenStr().size();
  unsigned int end = cursor - 1;
  return diag->addErr(err, ini, end);
}

void ScalaParser::saveState(State &state) {
  state.diagErrorsSize = diag->errors.size();
  lexer->saveState(state);
}

void ScalaParser::restoreState(State &state) {
  while (diag->errors.size() > state.diagErrorsSize) {
    diag->errors.pop_back();
  }

  lexer->restoreState(state);
}
} // namespace
