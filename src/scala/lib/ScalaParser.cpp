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

/**
 * semi ::= ‘;’ | nl {nl}
 */
void ScalaParser::parseSemi(spSemi &semi) {
  if (lexer->getCurToken() == STok::SEMI_COLON) {
    semi->opt = Semi::Opt::SEMI_COLON;
    semi->tokSemiColon = lexer->getCurTokenNode();
    return;
  }

  // TODO: nl {nl}
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
  while (true) {
    spBlockStat blockStat = spBlockStat(new BlockStat);
    parseBlockStat(blockStat);
    if (blockStat->err) {
      break;
    }

    spSemi semi = spSemi(new Semi);
    parseSemi(semi);
    if (semi->err) {
      block->addErr(-1);
    }

    block->paBlockStatSemi.push_back(std::make_pair(blockStat, semi));
  }

  // TODO: [ResultExpr]
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
 * BlockStat ::= Import
 *             | {Annotation} [‘implicit’ | ‘lazy’] Def
 *             | {Annotation} {LocalModifier} TmplDef
 *             | Expr1
 */
void ScalaParser::parseBlockStat(spBlockStat &blockStat) {
  // TODO: Import
  // TODO: {Annotation} [‘implicit’ | ‘lazy’] Def
  // TODO: {Annotation} {LocalModifier} TmplDef

  // Expr1
  blockStat->opt = BlockStat::Opt::EXPR1;
  blockStat->expr1 = spExpr1(new Expr1);
  parseExpr1(blockStat->expr1);
  if (blockStat->expr1->err) {
    blockStat->addErr(-1);
  }
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
    State state;
    saveState(state);
    spClassTemplate classTmpl = spClassTemplate(new ClassTemplate);
    parseClassTemplate(classTmpl);
    if (classTmpl->err == false) {
      classTmplOpt->opt = ClassTemplateOpt::Opt::CLASS_TEMPLATE;
      classTmplOpt->classTmpl = classTmpl;
      return;
    }

    restoreState(state);
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
 * Expr1 ::= ‘if’ ‘(’ Expr ‘)’ {nl} Expr [[semi] else Expr]
 *         | ‘while’ ‘(’ Expr ‘)’ {nl} Expr
 *         | ‘try’ ‘{’ Block ‘}’ [‘catch’ ‘{’ CaseClauses ‘}’]
 *           [‘finally’ Expr]
 *         | ‘do’ Expr [semi] ‘while’ ‘(’ Expr ’)’
 *         | ‘for’ (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’)
 *           {nl} [‘yield’] Expr
 *         | ‘throw’ Expr
 *         | ‘return’ [Expr]
 *         | [SimpleExpr ‘.’] id ‘=’ Expr
 *         | SimpleExpr1 ArgumentExprs ‘=’ Expr
 *         | PostfixExpr
 *         | PostfixExpr Ascription
 *         | PostfixExpr ‘match’ ‘{’ CaseClauses ‘}’
 */
void ScalaParser::parseExpr1(spExpr1 &expr1) {
  // TODO: ‘if’ ‘(’ Expr ‘)’ {nl} Expr [[semi] else Expr]
  // TODO: ‘while’ ‘(’ Expr ‘)’ {nl} Expr
  // TODO: ‘try’ ‘{’ Block ‘}’ [‘catch’ ‘{’ CaseClauses ‘}’] [‘finally’ Expr]
  // TODO: ‘do’ Expr [semi] ‘while’ ‘(’ Expr ’)’
  // TODO: ‘for’ (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’) {nl} [‘yield’] Expr
  // TODO: ‘throw’ Expr
  // TODO: ‘return’ [Expr]
  // TODO: [SimpleExpr ‘.’] id ‘=’ Expr
  // TODO: SimpleExpr1 ArgumentExprs ‘=’ Expr

  // PostfixExpr
  expr1->opt = Expr1::Opt::POSTFIX_EXPR;
  expr1->postfixExpr = spPostfixExpr(new PostfixExpr);
  parsePostfixExpr(expr1->postfixExpr);
  if (expr1->postfixExpr->err) {
    expr1->addErr(-1);
    return;
  }

  // TODO: PostfixExpr Ascription
  // TODO: PostfixExpr ‘match’ ‘{’ CaseClauses ‘}’
}

/**
 * InfixExpr ::= PrefixExpr
 *             | InfixExpr id [nl] InfixExpr
 */
void ScalaParser::parseInfixExpr(spInfixExpr &infixExpr) {
  infixExpr->prefixExpr = spPrefixExpr(new PrefixExpr);
  parsePrefixExpr(infixExpr->prefixExpr);
  if (infixExpr->prefixExpr->err) {
    infixExpr->addErr(-1);
    return;
  }

  // TODO: InfixExpr id [nl] InfixExpr
}

/**
 * ObjectDef ::= id ClassTemplateOpt
 */
void ScalaParser::parseObjectDef(spObjectDef &objectDef) {
  objectDef->id = parseLexId();
  if (objectDef->id->err) {
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
 * PrefixExpr ::= [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr
 */
void ScalaParser::parsePrefixExpr(spPrefixExpr &prefixExpr) {
  // TODO: [‘-’ | ‘+’ | ‘~’ | ‘!’]
  prefixExpr->simpleExpr = spSimpleExpr(new SimpleExpr);
  parseSimpleExpr(prefixExpr->simpleExpr);
  if (prefixExpr->simpleExpr->err) {
    prefixExpr->addErr(-1);
  }
}

/**
 * PostfixExpr ::= InfixExpr [id [nl]]
 */
void ScalaParser::parsePostfixExpr(spPostfixExpr &postfixExpr) {
  postfixExpr->infixExpr = spInfixExpr(new InfixExpr);
  parseInfixExpr(postfixExpr->infixExpr);
  if (postfixExpr->infixExpr->err) {
    postfixExpr->addErr(-1);
    return;
  }

  // TODO: [id [nl]]
}

/**
 * SimpleExpr ::= ‘new’ (ClassTemplate | TemplateBody)
 *              | BlockExpr
 *              | SimpleExpr1 [‘_’]
 */
void ScalaParser::parseSimpleExpr(spSimpleExpr &simpleExpr) {
  // TODO: ‘new’ (ClassTemplate | TemplateBody)
  // TODO: BlockExpr

  simpleExpr->opt = SimpleExpr::Opt::SIMPLE_EXPR1;
  simpleExpr->simpleExpr1 = spSimpleExpr1(new SimpleExpr1);
  parseSimpleExpr1(simpleExpr->simpleExpr1);
  if (simpleExpr->simpleExpr1->err) {
    simpleExpr->addErr(-1);
  }
}

/**
 * Literal
 * Path
 * ‘_’
 * ‘(’ [Exprs] ‘)’
 * SimpleExpr ‘.’ id
 * SimpleExpr TypeArgs
 * SimpleExpr1 ArgumentExprs
 * XmlExpr
 */
void ScalaParser::parseSimpleExpr1(spSimpleExpr1 &simpleExpr1) {
  // TODO: Literal
  // TODO: Path
  // TODO: ‘_’
  // TODO: ‘(’ [Exprs] ‘)’
  // TODO: SimpleExpr ‘.’ id
  // TODO: SimpleExpr TypeArgs
  // TODO: SimpleExpr1 ArgumentExprs
  // TODO: XmlExpr
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
