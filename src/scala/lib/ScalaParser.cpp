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
  lId->val = lexer->getCurTokenStr();
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

spStringLiteral ScalaParser::parseStringLiteral() {
  spStringLiteral strLit = spStringLiteral(new StringLiteral);
  if (lexer->getCurToken() != STok::STRING_LITERAL) {
    strLit->addErr(ERR_EXP_STRING_LITERAL);
    return strLit;
  }

  strLit->ini = lexer->getCurTokenIni();
  strLit->end = lexer->getCurTokenEnd();
  strLit->val = lexer->getCurTokenStr();
  return strLit;
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
  // TODO: [nl]
  // BlockExpr
  if (lexer->getCurToken() != STok::LPAREN) {
    argExprs->opt = ArgumentExprs::Opt::BLOCK_EXPR;
    argExprs->blockExpr = spBlockExpr(new BlockExpr);
    parseBlockExpr(argExprs->blockExpr);
    if (argExprs->blockExpr->err) {
      argExprs->addErr(-1);
    }

    return;
  }

  // We assume ‘(’ [Exprs] ‘)’ until we see a ',' or the PostfixExpr production
  // ‘(’ [Exprs] ‘)’
  argExprs->opt = ArgumentExprs::Opt::EXPRS;
  argExprs->tokLParen = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '('

  // '(' ')'
  if (lexer->getCurToken() == STok::RPAREN) {
    argExprs->tokRParen = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume ')'
    return;
  }

  // Try to parse Exprs
  State state;
  saveState(state);

  argExprs->exprs = spExprs(new Exprs);
  parseExprs(argExprs->exprs);
  if (argExprs->exprs->err) {
    restoreState(state);

    // Our only option now is a Postfix
    argExprs->opt = ArgumentExprs::Opt::EXPRS_POSTFIX_EXPR;

    // PostfixExpr ‘:’ ‘_’ ‘*’
    parseArgumentExprsHelper(argExprs);
    if (argExprs->err) {
      return;
    }

    // ')'
    if (lexer->getCurToken() != STok::RPAREN) {
      argExprs->addErr(ERR_EXP_RPAREN);
      return;
    }

    argExprs->tokRParen = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume ')'
    return;
  }

  // '(' Exprs ')'
  if (lexer->getCurToken() == STok::RPAREN) {
    argExprs->tokRParen = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume ')'
    return;
  }

  if (lexer->getCurToken() != STok::COMMA) {
    argExprs->addErr(ERR_EXP_COMMA);
    return;
  }

  argExprs->tokComma = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume ,

  // PostfixExpr ‘:’ ‘_’ ‘*’
  argExprs->opt = ArgumentExprs::Opt::EXPRS_POSTFIX_EXPR;
  parseArgumentExprsHelper(argExprs);
  if (argExprs->err) {
    return;
  }

  // ')'
  if (lexer->getCurToken() != STok::RPAREN) {
    argExprs->addErr(ERR_EXP_RPAREN);
    return;
  }

  argExprs->tokRParen = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume ')'
}

/**
 * PostfixExpr ‘:’ ‘_’ ‘*’
 */
void ScalaParser::parseArgumentExprsHelper(spArgumentExprs &argExprs) {
  argExprs->postfixExpr = spPostfixExpr(new PostfixExpr);
  parsePostfixExpr(argExprs->postfixExpr);
  if (argExprs->postfixExpr->err) {
    argExprs->addErr(-1);
    return;
  }

  // TODO: ‘:’ ‘_’ ‘*’
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
  lexer->getNextToken(); // consume '{'
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
  lexer->getNextToken(); // consume '}'
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
 * Expr ::= (Bindings | [‘implicit’] id | ‘_’) ‘=>’ Expr
 *        | Expr1
 */
void ScalaParser::parseExpr(spExpr &expr) {
  // TODO: (Bindings | [‘implicit’] id | ‘_’) ‘=>’ Expr

  expr->opt = Expr::Opt::EXPR1;
  expr->expr1 = spExpr1(new Expr1);
  parseExpr1(expr->expr1);
  if (expr->expr1->err) {
    expr->addErr(-1);
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
 * Exprs ::= Expr {‘,’ Expr}
 */
void ScalaParser::parseExprs(spExprs &exprs) {
  // Expr
  exprs->expr = spExpr(new Expr);
  parseExpr(exprs->expr);
  if (exprs->expr->err) {
    exprs->addErr(-1);
    return;
  }

  // {‘,’ Expr}
  State state;
  unsigned comma;
  while (lexer->getCurToken() == STok::COMMA) {
    saveState(state);
    comma = lexer->getCurTokenIni();
    lexer->getNextToken(); // consume ','

    spExpr expr = spExpr(new Expr);
    parseExpr(expr);
    if (expr->err) {
      restoreState(state);
      return;
    }

    exprs->pairs.push_back(std::make_pair(comma, expr));
  }
}

/**
 * InfixExpr ::= PrefixExpr
 *             | InfixExpr id [nl] InfixExpr
 */
void ScalaParser::parseInfixExpr(spInfixExpr &infixExpr) {
  infixExpr->opt = InfixExpr::Opt::PREFIX;
  infixExpr->prefixExpr = spPrefixExpr(new PrefixExpr);
  parsePrefixExpr(infixExpr->prefixExpr);
  if (infixExpr->prefixExpr->err) {
    infixExpr->addErr(-1);
    return;
  }

  // TODO: InfixExpr id [nl] InfixExpr
}

/**
 * Literal ::= [‘-’] integerLiteral
 *           | [‘-’] floatingPointLiteral
 *           | booleanLiteral
 *           | characterLiteral
 *           | stringLiteral
 *           | symbolLiteral
 *           | ‘null’
 */
void ScalaParser::parseLiteral(spLiteral &literal) {
  // TODO: [‘-’] integerLiteral
  // TODO: [‘-’] floatingPointLiteral
  // TODO: booleanLiteral
  // TODO: characterLiteral

  // stringLiteral
  if (lexer->getCurToken() == STok::STRING_LITERAL) {
    literal->strLit = parseStringLiteral();
    lexer->getNextToken(); // consume string literal
    return;
  }

  // TODO: symbolLiteral
  // TODO: ‘null’

  literal->addErr(-1);
}

/**
 * ObjectDef ::= id ClassTemplateOpt
 */
void ScalaParser::parseObjectDef(spObjectDef &objectDef) {
  objectDef->id = parseLexId();
  lexer->getNextToken(); // consume id
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
 * Path ::= StableId
 *        | [id ‘.’] ‘this’
 */
void ScalaParser::parsePath(spPath &path) {
  path->opt = Path::Opt::STABLE_ID;
  path->stableId = spStableId(new StableId);
  parseStableId(path->stableId);
  if (path->stableId->err) {
    path->addErr(-1);
    return;
  }

  // TODO: [id ‘.’] ‘this’
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
 * SimpleExpr1 ::= SimpleExpr1Head SimpleExpr1Tail | SimpleExpr1Head
 */
void ScalaParser::parseSimpleExpr1(spSimpleExpr1 &simpleExpr1) {
  simpleExpr1->head = spSimpleExpr1Head(new SimpleExpr1Head);
  parseSimpleExpr1Head(simpleExpr1->head);
  if (simpleExpr1->head->err) {
    simpleExpr1->addErr(-1);
    return;
  }

  // SimpleExpr1Tail
  State state;
  saveState(state);

  spSimpleExpr1Tail tail = spSimpleExpr1Tail(new SimpleExpr1Tail);
  parseSimpleExpr1Tail(tail);

  // If there's an error we restore the state and assume it's
  // SimpleExpr1Head production.
  if (tail->err) {
    restoreState(state);
    return;
  }

  // SimpleExpr1Head SimpleExpr1Tail
  simpleExpr1->tail = tail;
}

/**
 * SimpleExpr1Head ::= Literal
 *                   | Path
 *                   | ‘_’
 *                   | ‘(’ [Exprs] ‘)’
 *                   | SimpleExpr ‘.’ id
 *                   | SimpleExpr TypeArgs
 *                   | XmlExpr
 */
void ScalaParser::parseSimpleExpr1Head(spSimpleExpr1Head &head) {
  State state;
  saveState(state);

  // Literal
  spLiteral literal = spLiteral(new Literal);
  parseLiteral(literal);
  if (literal->err == false) {
    head->opt = SimpleExpr1Head::Opt::LITERAL;
    head->literal = literal;
    return;
  }

  restoreState(state);
  saveState(state);

  // Path
  spPath path = spPath(new Path);
  parsePath(path);
  if (path->err == false) {
    head->opt = SimpleExpr1Head::Opt::PATH;
    head->path = path;
    return;
  }

  // TODO: ‘_’
  // TODO: ‘(’ [Exprs] ‘)’
  // TODO: SimpleExpr ‘.’ id
  // TODO: SimpleExpr TypeArgs
  // TODO: XmlExpr
}

/**
 * SimpleExpr1Tail ::=  ArgumentExprs SimpleExpr1Tail | ArgumentExprs
 */
void ScalaParser::parseSimpleExpr1Tail(spSimpleExpr1Tail &tail) {
  tail->argExprs = spArgumentExprs(new ArgumentExprs);
  parseArgumentExprs(tail->argExprs);
  if (tail->argExprs->err) {
    tail->err = addErr(-1);
    return;
  }

  State state;
  saveState(state);

  // If there's an error we restore the state and assume it's
  // ArgumentExprs production.
  spSimpleExpr1Tail tmpTail = spSimpleExpr1Tail(new SimpleExpr1Tail);
  parseSimpleExpr1Tail(tmpTail);
  if (tmpTail->err) {
    restoreState(state);
    return;
  }

  // ArgumentExprs SimpleExpr1Tail
  tail->tail = tmpTail;
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
  lexer->getNextToken(); // consume 'id'
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
  topStat->opt = TopStat::Opt::TMPL_DEF;
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
