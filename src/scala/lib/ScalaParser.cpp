#include "djp/ScalaParser.h"
#include <iostream>

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
  if (lexer->getCurToken() == STok::SEMICOLON) {
    semi->opt = Semi::Opt::SEMICOLON;
    semi->tokSemiColon = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume ';'
    return;
  }

  // nl {nl}
  // We only check we've seen a linebreak and we don't keep track of its
  // position or if we've seen more than one linebreak.
  if (lexer->getSeenLineBreak()) {
    semi->opt = Semi::Opt::NL;
    return;
  }

  semi->addErr(ERR_EXP_NL);
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
  if (lexer->getCurToken() == STok::LCURLYB) {
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

  if (argExprs->tokLParen->tok != STok::LPAREN) {
    argExprs->addErr(ERR_EXP_LPAREN);
    return;
  }

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
  State state;
  STok tok = lexer->getCurToken();
  while (tok != STok::ERROR
      && tok != STok::END_OF_FILE
      && tok != STok::RCURLYB) {

    saveState(state);
    spBlockStat blockStat = spBlockStat(new BlockStat);
    parseBlockStat(blockStat);
    if (blockStat->err) {
      restoreState(state);
      return;
    }

    saveState(state);
    spSemi semi = spSemi(new Semi);
    parseSemi(semi);
    if (semi->err) {
      restoreState(state);
      return;
    }

    block->paBlockStatSemi.push_back(std::make_pair(blockStat, semi));
    tok = lexer->getCurToken();
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

  // Block
  blockExpr->opt = BlockExpr::Opt::BLOCK;
  blockExpr->block = spBlock(new Block);
  parseBlock(blockExpr->block);
  if (blockExpr->block->err) {
    blockExpr->addErr(-1);
    return;
  }

  // '}'
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
 * CompilationUnit ::= {'package' QualId semi} TopStatSeq
 */
void ScalaParser::parseCompilationUnit() {
  while (lexer->getCurToken() == STok::PACKAGE) {
    auto tok = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume token

    auto qualId = spQualId(new QualId);
    parseQualId(qualId);
    if (qualId->err) {
      compUnit->addErr(-1);
      break;
    }

    auto semi = spSemi(new Semi);
    parseSemi(semi);
    if (semi->err) {
      compUnit->addErr(-1);
      break;
    }

    compUnit->tuples.push_back(std::tuple<spTokenNode, spQualId, spSemi>(
      tok, qualId, semi));
  }

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
 * Import ::= 'import' ImportExpr {',' ImportExpr}
 */
void ScalaParser::parseImport(spImport &import) {
  // import
  import->tokImport = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume 'import'

  import->importExpr = spImportExpr(new ImportExpr);
  parseImportExpr(import->importExpr);
  if (import->importExpr->err) {
    import->addErr(-1);
    return;
  }

  State state;
  while (lexer->getCurToken() == STok::COMMA) {
    saveState(state);
    // ,
    auto comma = lexer->getCurTokenNode();
    lexer->getNextToken();

    // ImportExpr
    auto importExpr = spImportExpr(new ImportExpr);
    parseImportExpr(importExpr);
    if (importExpr->err) {
      restoreState(state);
      return;
    }

    import->pairs.push_back(std::make_pair(comma, importExpr));
  }
}

/**
 * ImportExpr ::= StableId '.' (id | '_' | ImportSelectors)
 */
void ScalaParser::parseImportExpr(spImportExpr &importExpr) {
  // StableId
  importExpr->stableId = spStableId(new StableId);
  parseStableId(importExpr->stableId);
  if (importExpr->stableId->err) {
    importExpr->addErr(-1);
    return;
  }

  // '.'
  if (lexer->getCurToken() != STok::PERIOD) {
    importExpr->addErr(ERR_EXP_PERIOD);
    return;
  }

  importExpr->tokPeriod = lexer->getCurTokenNode();
  lexer->getNextToken();

  // We now have to check one of each
  // (id | '_' | ImportSelectors)

  // id
  if (lexer->getCurToken() == STok::ID) {
    importExpr->id = parseLexId();
    lexer->getNextToken();
    return;
  }

  // '_'
  if (lexer->getCurToken() == STok::UNDERSCORE) {
    importExpr->tokUnderscore = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume '_'
    return;
  }

  // TODO:
  // ImportSelectors

  importExpr->addErr(-1);
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
    literal->opt = Literal::Opt::STRING;
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
 * Packaging ::= ‘package’ QualId [nl] ‘{’ TopStatSeq ‘}’
 */
void ScalaParser::parsePackaging(spPackaging &packaging) {
  packaging->tokPackage = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume package

  packaging->qualId = spQualId(new QualId);
  parseQualId(packaging->qualId);
  if (packaging->qualId->err) {
    packaging->addErr(-1);
    return;
  }

  // TODO: [nl] ‘{’ TopStatSeq ‘}’
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
 * PeriodId ::= '.' id
 */
void ScalaParser::parsePeriodId(spPeriodId &periodId) {
  // '.'
  if (lexer->getCurToken() != STok::PERIOD) {
    periodId->addErr(ERR_EXP_PERIOD);
    return;
  }

  periodId->tok = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '.'

  // id
  periodId->id = parseLexId();
  lexer->getNextToken(); // consume id
  if (periodId->id->err) {
    periodId->addErr(-1);
  }
}

/**
 * PrefixExpr ::= ['-' | '+' | '~' | '!'] SimpleExpr
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
 * QualId ::= id {‘.’ id}
 */
void ScalaParser::parseQualId(spQualId &qualId) {
  qualId->id = parseLexId();
  lexer->getNextToken(); // consume id

  if (qualId->id->err) {
    qualId->addErr(-1);
    return;
  }

  State state;
  while (lexer->getCurToken() == STok::PERIOD) {
    saveState(state);
    spPeriodId perId = spPeriodId(new PeriodId);
    parsePeriodId(perId);
    if (perId->err) {
      restoreState(state);
      return;
    }

    qualId->periodIds.push_back(perId);
  }
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

  restoreState(state);

  // TODO: ‘_’
  // TODO: ‘(’ [Exprs] ‘)’
  // TODO: SimpleExpr ‘.’ id
  // TODO: SimpleExpr TypeArgs
  // TODO: XmlExpr

  head->addErr(-1);
}

/**
 * SimpleExpr1Tail ::=  ArgumentExprs SimpleExpr1Tail | ArgumentExprs
 */
void ScalaParser::parseSimpleExpr1Tail(spSimpleExpr1Tail &tail) {
  tail->argExprs = spArgumentExprs(new ArgumentExprs);
  parseArgumentExprs(tail->argExprs);
  if (tail->argExprs->err) {
    tail->addErr(-1);
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
 * SimpleType ::= SimpleTypeHead SimpleTypeTails | SimpleTypeHead
 */
void ScalaParser::parseSimpleType(spSimpleType &simpleType) {
  simpleType->head = spSimpleTypeHead(new SimpleTypeHead);
  parseSimpleTypeHead(simpleType->head);
  if (simpleType->head->err) {
    simpleType->addErr(-1);
    return;
  }

  // TODO: SimpleTypeTails
}

/**
 * SimpleTypeHead ::= StableId
 *                  | Path ‘.’ ‘type’
 *                  | ‘(’ Types ’)’
 */
void ScalaParser::parseSimpleTypeHead(spSimpleTypeHead &head) {
  // StableId
  head->opt = SimpleTypeHead::Opt::STABLE_ID;
  head->stableId = spStableId(new StableId);
  parseStableId(head->stableId);
  if (head->stableId->err) {
    head->addErr(-1);
    return;
  }

  // TODO: Path ‘.’ ‘type’
  // TODO: ‘(’ Types ’)’
}

/**
 * SimpleTypeTail ::= TypeArgs | ‘#’ id
 */
void ScalaParser::parseSimpleTypeTail(spSimpleTypeTail &tail) {
  // TODO: TypeArgs
  // TODO: ‘#’ id
}

/**
 * SimpleTypeTails ::= SimpleTypeTail SimpleTypeTails | SimpleTypeTail
 */
void ScalaParser::parseSimpleTypeTails(spSimpleTypeTails &tails) {
  // TODO:
}

/**
 * StableId ::= StableIdHead StableIdTail | StableIdHead
 */
void ScalaParser::parseStableId(spStableId &stableId) {
  stableId->head = spStableIdHead(new StableIdHead);
  parseStableIdHead(stableId->head);
  if (stableId->head->err) {
    stableId->addErr(-1);
    return;
  }

  // StableIdTail
  State state;
  saveState(state);
  auto tail = spStableIdTail(new StableIdTail);
  parseStableIdTail(tail);
  if (tail->err) {
    restoreState(state);
    return;
  }

  stableId->tail = tail;
}

/**
 * StableIdHead ::= id
 *                | [IdPeriod] ‘this’ PeriodId
 *                | [IdPeriod] ‘super’ [ClassQualifier] PeriodId
 */
void ScalaParser::parseStableIdHead(spStableIdHead &head) {
  spLexId id = parseLexId();
  lexer->getNextToken(); // consume 'id'
  if (id->err == false) {
    head->opt = StableIdHead::Opt::ID;
    head->id = id;
    return;
  }

  // TODO: Path '.' id
  // TODO: [id '.'] ‘super’ [ClassQualifier] '.' id
}

/**
 * StableIdTail ::= PeriodId StableIdTail | PeriodId
 */
void ScalaParser::parseStableIdTail(spStableIdTail &tail) {
  tail->periodId = spPeriodId(new PeriodId);
  parsePeriodId(tail->periodId);
  if (tail->periodId->err) {
    tail->addErr(-1);
    return;
  }

  // StableIdTail
  State state;
  saveState(state);
  auto tail2 = spStableIdTail(new StableIdTail);
  parseStableIdTail(tail2);
  if (tail2->err) {
    restoreState(state);
    return;
  }

  tail->tail = tail2;
}

/**
 * TemplateBody ::= [nl] ‘{’ [SelfType] TemplateStat {semi TemplateStat} ‘}’
 */
void ScalaParser::parseTemplateBody(spTemplateBody &tmplBody) {
  // TODO:
}

/**
 * TmplDef ::= ['case'] 'class' ClassDef
 *           | ['case'] 'object' ObjectDef
 *           | 'trait' TraitDef
 */
void ScalaParser::parseTmplDef(spTmplDef &tmplDef) {
  // 'case'
  if (lexer->getCurToken() == STok::CASE) {
    tmplDef->tokCase = lexer->getCurTokenNode();
    lexer->getNextToken();
  }

  // TODO: ['case'] 'class' ClassDef
  if (lexer->getCurToken() == STok::CLASS) {
    // TODO:
    return;
  }

  // ['case'] 'object' ObjectDef
  if (lexer->getCurToken() == STok::OBJECT) {
    tmplDef->opt = TmplDef::Opt::CASE_OBJECT;

    tmplDef->tokObject = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'case'

    tmplDef->objectDef = spObjectDef(new ObjectDef);
    parseObjectDef(tmplDef->objectDef);
    if (tmplDef->objectDef->err) {
      tmplDef->addErr(-1);
    }
    return;
  }

  // 'trait' TraitDef
  if (lexer->getCurToken() == STok::TRAIT) {
    tmplDef->opt = TmplDef::Opt::TRAIT;

    tmplDef->tokTrait = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'trait'

    tmplDef->traitDef = spTraitDef(new TraitDef);
    parseTraitDef(tmplDef->traitDef);
    if (tmplDef->traitDef->err) {
      tmplDef->addErr(-1);
    }

    return;
  }

  tmplDef->addErr(-1);
}

/**
 * TopStat ::= {Annotation [nl]} {Modifier} TmplDef
 *           | Import
 *           | Packaging
 *           | PackageObject
 */
void ScalaParser::parseTopStat(spTopStat &topStat) {
  // Packaging
  if (lexer->getCurToken() == STok::PACKAGE) {
    topStat->opt = TopStat::Opt::PACKAGING;
    topStat->packaging = spPackaging(new Packaging);
    parsePackaging(topStat->packaging);
    if (topStat->packaging->err) {
      topStat->addErr(-1);
    }
    return;
  }

  // Import
  if (lexer->getCurToken() == STok::IMPORT) {
    topStat->opt = TopStat::Opt::IMPORT;
    topStat->import = spImport(new Import);
    parseImport(topStat->import);
    if (topStat->import->err) {
      topStat->addErr(-1);
    }
    return;
  }

  // TmplDef
  // TODO: {Annotation [nl]} {Modifier}
  topStat->opt = TopStat::Opt::TMPL_DEF;
  topStat->tmplDef = spTmplDef(new TmplDef);
  parseTmplDef(topStat->tmplDef);
  if (topStat->tmplDef->err) {
    topStat->addErr(-1);
    return;
  }

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

  // {semi TopStat}
  while (true) {
    auto semi = spSemi(new Semi);
    parseSemi(semi);
    if (semi->err) {
      return;
    }

    auto topStat = spTopStat(new TopStat);
    parseTopStat(topStat);
    if (topStat->err) {
      return;
    }

    topStatSeq->pairs.push_back(make_pair(semi, topStat));
  }
}

/**
 * TraitDef ::= id [TypeParamClause] TraitTemplateOpt
 */
void ScalaParser::parseTraitDef(spTraitDef &traitDef) {
  // id
  traitDef->id = parseLexId();
  lexer->getNextToken(); // consume id
  if (traitDef->id->err) {
    traitDef->addErr(-1);
    return;
  }

  // TODO: [TypeParamClause]

  // TraitTemplateOpt
  traitDef->traitTemplateOpt = spTraitTemplateOpt(new TraitTemplateOpt);
  parseTraitTemplateOpt(traitDef->traitTemplateOpt);
  if (traitDef->traitTemplateOpt->err) {
    traitDef->addErr(-1);
  }
}

/**
 * TraitParents ::= AnnotType {'with' AnnotType}
 */
void ScalaParser::parseTraitParents(spTraitParents &parents) {
  // AnnotType
  parents->annotType = spAnnotType(new AnnotType);
  parseAnnotType(parents->annotType);
  if (parents->annotType->err) {
    parents->addErr(-1);
    return;
  }

  State state;
  while (lexer->getCurToken() == STok::WITH) {
    saveState(state);

    auto tok = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'with'

    auto annotType = spAnnotType(new AnnotType);
    parseAnnotType(annotType);
    if (annotType->err) {
      restoreState(state);
      return;
    }

    parents->pairs.push_back(std::make_pair(tok, annotType));
  }
}

/**
 * TraitTemplate ::= [EarlyDefs] TraitParents [TemplateBody]
 */
void ScalaParser::parseTraitTemplate(spTraitTemplate &traitTemplate) {
  // TODO: [EarlyDefs]

  traitTemplate->traitParents = spTraitParents(new TraitParents);
  parseTraitParents(traitTemplate->traitParents);
  if (traitTemplate->traitParents->err) {
    traitTemplate->addErr(-1);
    return;
  }

  // TODO: [TemplateBody]
}

/**
 * TraitTemplateOpt ::= 'extends' TraitTemplate | [['extends'] TemplateBody]
 */
void ScalaParser::parseTraitTemplateOpt(spTraitTemplateOpt &traitTemplateOpt) {
  // TemplateBody
  if (lexer->getCurToken() != STok::EXTENDS) {
    traitTemplateOpt->opt = TraitTemplateOpt::Opt::TEMPLATE_BODY;
    traitTemplateOpt->templateBody = spTemplateBody(new TemplateBody);
    parseTemplateBody(traitTemplateOpt->templateBody);
    if (traitTemplateOpt->templateBody->err) {
      traitTemplateOpt->addErr(-1);
    }

    return;
  }

  // 'extends' is mandatory
  if (lexer->getCurToken() != STok::EXTENDS) {
    traitTemplateOpt->addErr(ERR_EXP_TRAIT);
    return;
  }

  traitTemplateOpt->tokExtends = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume 'extends'

  // We have two options: TraitTemplate or TemplateBody. We try each.
  State state;
  saveState(state);

  auto traitTemplate = spTraitTemplate(new TraitTemplate);
  parseTraitTemplate(traitTemplate);
  if (traitTemplate->err == false) {
    traitTemplateOpt->opt = TraitTemplateOpt::Opt::TRAIT_TEMPLATE;
    traitTemplateOpt->traitTemplate = traitTemplate;
    return;
  }

  restoreState(state);
  traitTemplateOpt->opt = TraitTemplateOpt::Opt::TEMPLATE_BODY;
  traitTemplateOpt->templateBody = spTemplateBody(new TemplateBody);
  parseTemplateBody(traitTemplateOpt->templateBody);
  if (traitTemplateOpt->templateBody->err) {
    traitTemplateOpt->addErr(-1);
  }
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
