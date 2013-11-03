#include "c4/ScalaParser.h"
#include <iostream>

namespace c4s {

// -----------------------------------------------------------------------------
// Lexical grammar
// -----------------------------------------------------------------------------
spLexId ScalaParser::parseLexId() {
  spLexId lId = spLexId(new LexId);
  if (lexer->getCurToken() != STok::ID) {
    lId->addErr(c4::ERR_EXP_IDENTIFIER);
    return lId;
  }

  lId->ini = lexer->getCurTokenIni();
  lId->end = lexer->getCurTokenEnd();
  lId->val = lexer->getCurTokenStr();
  return lId;
}

/**
 * semi ::= ';' | nl {nl}
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

  semi->addErr(c4::ERR_EXP_NL);
}

spStringLiteral ScalaParser::parseStringLiteral() {
  spStringLiteral strLit = spStringLiteral(new StringLiteral);
  if (lexer->getCurToken() != STok::STRING_LITERAL) {
    strLit->addErr(c4::ERR_EXP_STRING_LITERAL);
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
 * AccessModifier ::= ('private' | 'protected') [AccessQualifier]
 */
void ScalaParser::parseAccessModifier(spAccessModifier &accessModifier) {
  if (!isAccessModifier(lexer->getCurToken())) {
    accessModifier->addErr(-1);
    return;
  }

  accessModifier->tok = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume 'private' or 'protected'

  // [AccessQualifier]
  if (lexer->getCurToken() != STok::LBRACKET) {
    return;
  }

  accessModifier->accessQual = spAccessQualifier(new AccessQualifier);
  parseAccessQualifier(accessModifier->accessQual);
  if (accessModifier->accessQual->err) {
    accessModifier->addErr(-1);
    return;
  }
}

/**
 * AccessQualifier ::= '[' (id | 'this') ']'
 */
void ScalaParser::parseAccessQualifier(spAccessQualifier &accessQual) {
  // '['
  if (lexer->getCurToken() != STok::LBRACKET) {
    accessQual->addErr(c4::ERR_EXP_LBRACKET);
    return;
  }

  accessQual->tokLBracket = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '['

  // (id | 'this')
  if (lexer->getCurToken() == STok::THIS) {
    accessQual->tokThis = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'this'
  } else {
    accessQual->id = parseLexId();
    lexer->getNextToken(); // consume 'id'
    if (accessQual->id->err) {
      accessQual->addErr(c4::ERR_EXP_IDENTIFIER);
      return;
    }
  }

  // ']'
  if (lexer->getCurToken() != STok::RBRACKET) {
    accessQual->addErr(c4::ERR_EXP_RBRACKET);
    return;
  }

  accessQual->tokRBracket = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume ']'
}

/**
 * Annotation ::= '@' SimpleType {ArgumentExprs}
 */
void ScalaParser::parseAnnotation(spAnnotation &annotation) {
  // '@'
  if (lexer->getCurToken() != STok::AT) {
    annotation->addErr(-1);
    return;
  }

  annotation->tokAt = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '@'

  // SimpleType
  annotation->simpleType = spSimpleType(new SimpleType);
  parseSimpleType(annotation->simpleType);
  if (annotation->simpleType->err) {
    annotation->addErr(-1);
    return;
  }

  // {ArgumentExprs}
  State state;
  while (true) {
    saveState(state);
    auto argExprs = spArgumentExprs(new ArgumentExprs);
    parseArgumentExprs(argExprs);
    if (argExprs->err) {
      restoreState(state);
      return;
    }

    annotation->argExprsVec.push_back(argExprs);
  }
}

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

  // {Annotation}
  while (lexer->getCurToken() == STok::AT) {
    auto annotation = spAnnotation(new Annotation);
    parseAnnotation(annotation);
    if (annotation->err) {
      annotType->addErr(-1);
      break;
    }

    annotType->annotations.push_back(annotation);
  }
}

/**
 * ArgumentExprs ::= '(' [Exprs] ')'
 *                 | '(' [Exprs ','] PostfixExpr ':' '_' '*' ')'
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

  // We assume '(' [Exprs] ')' until we see a ',' or the PostfixExpr production
  // '(' [Exprs] ')'
  argExprs->opt = ArgumentExprs::Opt::EXPRS;
  argExprs->tokLParen = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '('

  if (argExprs->tokLParen->tok != STok::LPAREN) {
    argExprs->addErr(c4::ERR_EXP_LPAREN);
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

    // PostfixExpr ':' '_' '*'
    parseArgumentExprsHelper(argExprs);
    if (argExprs->err) {
      return;
    }

    // ')'
    if (lexer->getCurToken() != STok::RPAREN) {
      argExprs->addErr(c4::ERR_EXP_RPAREN);
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
    argExprs->addErr(c4::ERR_EXP_COMMA);
    return;
  }

  argExprs->tokComma = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume ,

  // PostfixExpr ':' '_' '*'
  argExprs->opt = ArgumentExprs::Opt::EXPRS_POSTFIX_EXPR;
  parseArgumentExprsHelper(argExprs);
  if (argExprs->err) {
    return;
  }

  // ')'
  if (lexer->getCurToken() != STok::RPAREN) {
    argExprs->addErr(c4::ERR_EXP_RPAREN);
    return;
  }

  argExprs->tokRParen = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume ')'
}

/**
 * PostfixExpr ':' '_' '*'
 */
void ScalaParser::parseArgumentExprsHelper(spArgumentExprs &argExprs) {
  argExprs->postfixExpr = spPostfixExpr(new PostfixExpr);
  parsePostfixExpr(argExprs->postfixExpr);
  if (argExprs->postfixExpr->err) {
    argExprs->addErr(-1);
    return;
  }

  // TODO: ':' '_' '*'
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
 * BlockExpr ::= '{' CaseClauses '}'
 *             | '{' Block '}'
 */
void ScalaParser::parseBlockExpr(spBlockExpr &blockExpr) {
  // TODO: CaseClauses

  // '{'
  spTokenNode tokLCurlyB = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '{'
  if (tokLCurlyB->tok != STok::LCURLYB) {
    blockExpr->addErr(addErr(c4::ERR_EXP_LCURLY_BRACKET));
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
    blockExpr->addErr(addErr(c4::ERR_EXP_RCURLY_BRACKET));
    return;
  }

  blockExpr->tokRCurlyB = tokRCurlyB;
}

/**
 * BlockStat ::= Import
 *             | {Annotation} ['implicit' | 'lazy'] Def
 *             | {Annotation} {LocalModifier} TmplDef
 *             | Expr1
 */
void ScalaParser::parseBlockStat(spBlockStat &blockStat) {
  // Import
  if (lexer->getCurToken() == STok::IMPORT) {
    blockStat->opt = BlockStat::Opt::IMPORT;
    blockStat->import = spImport(new Import);
    parseImport(blockStat->import);
    if (blockStat->import->err) {
      blockStat->addErr(-1);
    }

    return;
  }

  // {Annotation}
  auto seenAnnotation = false;
  while (lexer->getCurToken() == STok::AT) {
    seenAnnotation = true;
    auto annotation = spAnnotation(new Annotation);
    parseAnnotation(annotation);
    if (annotation->err) {
      blockStat->addErr(-1);
      break;
    }

    blockStat->annotations.push_back(annotation);
  }

  // ['implicit' | 'lazy'] Def
  bool seenDefPrefix = false;
  if (lexer->getCurToken() == STok::IMPLICIT) {
    seenDefPrefix = true;
    blockStat->tokImplicit = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'implicit'
  } else if (lexer->getCurToken() == STok::LAZY) {
    seenDefPrefix = true;
    blockStat->tokLazy = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'lazy'
  }

  // Try Def
  State state;
  saveState(state);
  auto def = spDef(new Def);
  parseDef(def);
  if (def->err == false) {
    blockStat->opt = BlockStat::Opt::DEF;
    blockStat->def = def;
    return;
  } else {
    if (seenDefPrefix) {
      blockStat->opt = BlockStat::Opt::DEF;
      blockStat->addErr(-1);
      return;
    }

    // Restore state and try next production rule
    restoreState(state);
  }

  // {LocalModifier} TmplDef
  auto seenLocalModifier = false;
  while (isLocalModifier(lexer->getCurToken())) {
    seenLocalModifier = true;
    auto localModifier = spLocalModifier(new LocalModifier);
    parseLocalModifier(localModifier);
    blockStat->localModifiers.push_back(localModifier);
  }

  auto tmplDef = spTmplDef(new TmplDef);
  parseTmplDef(tmplDef);
  if (tmplDef->err == false) {
    blockStat->opt = BlockStat::Opt::TMPL_DEF;
    blockStat->tmplDef = tmplDef;
    return;
  } else {
    if (seenLocalModifier) {
      blockStat->addErr(-1);
      return;
    }

    // Try next production rule if possible
    restoreState(state);
  }

  if (seenAnnotation) {
    blockStat->addErr(-1);
    return;
  }

  // Expr1
  blockStat->opt = BlockStat::Opt::EXPR1;
  blockStat->expr1 = spExpr1(new Expr1);
  parseExpr1(blockStat->expr1);
  if (blockStat->expr1->err) {
    blockStat->addErr(-1);
  }
}

/**
 * ClassDef ::= id [TypeParamClause] {ConstrAnnotation} [AccessModifier]
 *              ClassParamClauses ClassTemplateOpt
 */
void ScalaParser::parseClassDef(spClassDef &classDef) {
  classDef->id = parseLexId();
  lexer->getNextToken(); // consume id
  if (classDef->id->err) {
    classDef->addErr(-1);
    return;
  }

  // TODO: [TypeParamClause]
  // TODO: {ConstrAnnotation}
  // TODO: [AccessModifier]
  // TODO: ClassParamClauses

  // ClassTemplateOpt
  classDef->classTmplOpt = spClassTemplateOpt(new ClassTemplateOpt);
  parseClassTemplateOpt(classDef->classTmplOpt);
  if (classDef->classTmplOpt->err) {
    classDef->addErr(-1);
  }
}

/**
 * ClassParents ::= Constr {'with' AnnotType}
 */
void ScalaParser::parseClassParents(spClassParents &classParents) {
  classParents->constr = spConstr(new Constr);
  parseConstr(classParents->constr);
  if (classParents->constr->err) {
    classParents->constr->addErr(-1);
    return;
  }

  // TODO: {'with' AnnotType}
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
 * ClassTemplateOpt ::= 'extends' ClassTemplate
 *                    | [['extends'] TemplateBody]
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

  // [TemplateBody]
  if (lexer->getCurToken() != STok::LCURLYB) {
    return;
  }

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

    constr->argExprsVec.push_back(argExprs);
  }
}

/**
 * CompoundType ::= AnnotType {'with' AnnotType} [Refinement]
 *                | Refinement
 */
void ScalaParser::parseCompoundType(spCompoundType &compoundType) {
  compoundType->opt = CompoundType::Opt::ANNOT_TYPE;
  compoundType->annotType = spAnnotType(new AnnotType);
  parseAnnotType(compoundType->annotType);
  if (compoundType->annotType->err) {
    compoundType->addErr(-1);
    return;
  }

  // TODO: {'with' AnnotType} [Refinement]
  // TODO: Refinement
}

/**
 * Def ::= PatVarDef
 *       | 'def' FunDef
 *       | 'type' {nl} TypeDef
 *       | TmplDef
 */
void ScalaParser::parseDef(spDef &def) {

  // TODO: PatVarDef

  {
    // 'def' FunDef
    if (lexer->getCurToken() == STok::DEF) {
      def->opt = Def::Opt::DEF;
      def->tokDef = lexer->getCurTokenNode();
      lexer->getNextToken(); // consume 'def'

      def->funDef = spFunDef(new FunDef);
      parseFunDef(def->funDef);
      if (def->funDef->err) {
        def->addErr(-1);
      }
      return;
    }
  }

  // TODO: 'type' {nl} TypeDef
  // TODO: TmplDef

  def->addErr(-1);
}

/**
 * FunDef ::= FunSig [':' Type] '=' Expr
 *          | FunSig [nl] '{' Block '}'
 *          | 'this' ParamClause ParamClauses
 *            ('=' ConstrExpr | [nl] ConstrBlock)
 */
void ScalaParser::parseFunDef(spFunDef &funDef) {
  {
    // FunSig [':' Type] '=' Expr
    funDef->opt = FunDef::Opt::FUN_SIG_EQUALS_EXPR;
    funDef->funSig = spFunSig(new FunSig);
    parseFunSig(funDef->funSig);
    if (funDef->funSig->err) {
      funDef->addErr(-1);
      return;
    }

    // [':' Type]
    if (lexer->getCurToken() == STok::COLON) {
      funDef->tokColon = lexer->getCurTokenNode();
      lexer->getNextToken(); // consume ':'

      funDef->type = spType(new Type);
      parseType(funDef->type);
      if (funDef->type->err) {
        funDef->addErr(-1);
        return;
      }
    }

    if (lexer->getCurToken() != STok::EQUALS) {
      funDef->addErr(c4::ERR_EXP_OP_EQUALS);
      return;
    }

    funDef->tokEquals = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume '='

    funDef->expr = spExpr(new Expr);
    parseExpr(funDef->expr);
    if (funDef->expr->err) {
      funDef->addErr(-1);
    }

    return;
  }

  // TODO: FunSig [nl] '{' Block '}'

  // TODO: 'this' ParamClause ParamClauses ('=' ConstrExpr | [nl] ConstrBlock)

  funDef->addErr(-1);
}

/**
 * FunSig ::= id [FunTypeParamClause] ParamClauses
 */
void ScalaParser::parseFunSig(spFunSig &funSig) {
  funSig->id = parseLexId();
  lexer->getNextToken(); // consume 'id'
  if (funSig->id->err) {
    funSig->addErr(-1);
    return;
  }

  // [FunTypeParamClause]
  if (lexer->getCurToken() == STok::LBRACKET) {
    funSig->funTypeParamClause = spFunTypeParamClause(new FunTypeParamClause);
    parseFunTypeParamClause(funSig->funTypeParamClause);
    if (funSig->funTypeParamClause->err) {
      // We have an error but we don't exit here.
      // We try to keep parsing the function.
      funSig->addErr(-1);
    }
  }

  // ParamClauses
  funSig->paramClauses = spParamClauses(new ParamClauses);
  parseParamClauses(funSig->paramClauses);
  if (funSig->paramClauses->err) {
    funSig->addErr(-1);
  }
}

/**
 * FunTypeParamClause ::= '[' TypeParam {',' TypeParam} ']'
 */
void ScalaParser::parseFunTypeParamClause(
  spFunTypeParamClause &funTypeParamClause) {

  // '['
  if (lexer->getCurToken() != STok::LBRACKET) {
    funTypeParamClause->addErr(c4::ERR_EXP_LBRACKET);
    return;
  }

  funTypeParamClause->tokLBracket = lexer->getCurTokenNode();
  lexer->getNextToken(); // '['

  funTypeParamClause->typeParam = spTypeParam(new TypeParam);
  parseTypeParam(funTypeParamClause->typeParam);
  if (funTypeParamClause->typeParam->err) {
    funTypeParamClause->addErr(-1);
    return;
  }

  // {',' TypeParam}
  while (lexer->getCurToken() == STok::COMMA) {
    auto comma = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume ','
    auto typeParam = spTypeParam(new TypeParam);
    parseTypeParam(typeParam);
    if (typeParam->err) {
      funTypeParamClause->addErr(-1);
      return;
    }

    funTypeParamClause->pairs.push_back(std::make_pair(comma, typeParam));
  }

  // ']'
  if (lexer->getCurToken() != STok::RBRACKET) {
    funTypeParamClause->addErr(c4::ERR_EXP_RBRACKET);
    return;
  }

  funTypeParamClause->tokRBracket = lexer->getCurTokenNode();
  lexer->getNextToken(); // ']'
}

/**
 * Expr ::= (Bindings | ['implicit'] id | '_') '=>' Expr
 *        | Expr1
 */
void ScalaParser::parseExpr(spExpr &expr) {
  // TODO: (Bindings | ['implicit'] id | '_') '=>' Expr

  expr->opt = Expr::Opt::EXPR1;
  expr->expr1 = spExpr1(new Expr1);
  parseExpr1(expr->expr1);
  if (expr->expr1->err) {
    expr->addErr(-1);
  }
}

/**
 * Expr1 ::= 'if' '(' Expr ')' {nl} Expr [[semi] else Expr]
 *         | 'while' '(' Expr ')' {nl} Expr
 *         | 'try' '{' Block '}' ['catch' '{' CaseClauses '}']
 *           ['finally' Expr]
 *         | 'do' Expr [semi] 'while' '(' Expr ')'
 *         | 'for' ('(' Enumerators ')' | '{' Enumerators '}')
 *           {nl} ['yield'] Expr
 *         | 'throw' Expr
 *         | 'return' [Expr]
 *         | [SimpleExpr '.'] id '=' Expr
 *         | SimpleExpr1 ArgumentExprs '=' Expr
 *         | PostfixExpr
 *         | PostfixExpr Ascription
 *         | PostfixExpr 'match' '{' CaseClauses '}'
 */
void ScalaParser::parseExpr1(spExpr1 &expr1) {

  // TODO: 'if' '(' Expr ')' {nl} Expr [[semi] else Expr]
  // TODO: 'while' '(' Expr ')' {nl} Expr
  // TODO: 'try' '{' Block '}' ['catch' '{' CaseClauses '}'] ['finally' Expr]
  // TODO: 'do' Expr [semi] 'while' '(' Expr ')'
  // TODO: 'for' ('(' Enumerators ')' | '{' Enumerators '}') {nl} ['yield'] Expr
  // TODO: 'throw' Expr
  // TODO: 'return' [Expr]

  State state;
  // [SimpleExpr '.'] id '=' Expr
  // TODO: [SimpleExpr '.']
  saveState(state);
  if (lexer->getCurToken() == STok::ID) {
    auto id = parseLexId();
    lexer->getNextToken(); // consume 'id'

    if (lexer->getCurToken() == STok::EQUALS) {
      auto tokEquals = lexer->getCurTokenNode();
      lexer->getNextToken(); // consume '='

      auto expr = spExpr(new Expr);
      parseExpr(expr);
      if (expr->err == false) {
        expr1->opt = Expr1::Opt::ID_EQUALS_EXPR;
        expr1->id = id;
        expr1->tokEquals = tokEquals;
        expr1->expr = expr;
        return;
      }
    }

    restoreState(state);
  }

  // TODO: SimpleExpr1 ArgumentExprs '=' Expr

  // PostfixExpr
  expr1->opt = Expr1::Opt::POSTFIX_EXPR;
  expr1->postfixExpr = spPostfixExpr(new PostfixExpr);
  parsePostfixExpr(expr1->postfixExpr);
  if (expr1->postfixExpr->err) {
    expr1->addErr(-1);
    return;
  }

  // TODO: PostfixExpr Ascription
  // TODO: PostfixExpr 'match' '{' CaseClauses '}'
}

/**
 * Exprs ::= Expr {',' Expr}
 */
void ScalaParser::parseExprs(spExprs &exprs) {
  // Expr
  exprs->expr = spExpr(new Expr);
  parseExpr(exprs->expr);
  if (exprs->expr->err) {
    exprs->addErr(-1);
    return;
  }

  // {',' Expr}
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
 * ImportExpr ::= QualId [ ('.' '_') | '.' ImportSelectors ]
 */
void ScalaParser::parseImportExpr(spImportExpr &importExpr) {
  // QualId
  importExpr->qualId = spQualId(new QualId);
  parseQualId(importExpr->qualId);
  if (importExpr->qualId->err) {
    importExpr->addErr(-1);
    return;
  }

  if (lexer->getCurToken() != STok::PERIOD) {
    return;
  }

  importExpr->tokPeriod = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '.'

  // ('.' '_')
  if (lexer->getCurToken() == STok::UNDERSCORE) {
    importExpr->tokUnderscore = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume '_'
    return;
  }

  // '.' ImportSelectors
  importExpr->importSelectors = spImportSelectors(new ImportSelectors);
  parseImportSelectors(importExpr->importSelectors);
  if (importExpr->importSelectors->err) {
    importExpr->addErr(-1);
  }
}

/**
 * ImportSelector ::= id ['=>' id | '=>' '_']
 */
void ScalaParser::parseImportSelector(spImportSelector &importSelector) {
  importSelector->id = parseLexId();
  lexer->getNextToken(); // consume id
  if (importSelector->id->err) {
    importSelector->addErr(-1);
    return;
  }

  // TODO: ['=>' id | '=>' '_']
}


/**
 * ImportSelectors ::= '{' {ImportSelector ','} (ImportSelector | '_') '}'
 */
void ScalaParser::parseImportSelectors(spImportSelectors &importSelectors) {
  if (lexer->getCurToken() != STok::LCURLYB) {
    importSelectors->addErr(c4::ERR_EXP_LCURLY_BRACKET);
    return;
  }

  importSelectors->tokLCurlyB = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '{'

  while (true) {
    if (lexer->getCurToken() == STok::UNDERSCORE) {
      importSelectors->tokUnderscore = lexer->getCurTokenNode();
      lexer->getNextToken(); // consume '_'
      break;
    }

    spImportSelector importSelector = spImportSelector(new ImportSelector);
    parseImportSelector(importSelector);
    if (importSelector->err) {
      importSelectors->addErr(-1);
      return;
    }

    // {ImportSelector ','}
    if (lexer->getCurToken() == STok::COMMA) {
      spTokenNode tokComma = lexer->getCurTokenNode();
      lexer->getNextToken(); // consume ','
      importSelectors->pairs.push_back(std::make_pair(
        importSelector, tokComma));
      continue;
    }

    importSelectors->importSelector = importSelector;
    break;
  }

  if (lexer->getCurToken() != STok::RCURLYB) {
    importSelectors->addErr(c4::ERR_EXP_RCURLY_BRACKET);
    return;
  }

  importSelectors->tokRCurlyB = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '}'
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
 * InfixType ::= CompoundType {id [nl] CompoundType}
 */
void ScalaParser::parseInfixType(spInfixType &infixType) {
  infixType->compoundType = spCompoundType(new CompoundType);
  parseCompoundType(infixType->compoundType);
  if (infixType->compoundType->err) {
    infixType->addErr(-1);
    return;
  }

  // TODO: {id [nl] CompoundType}
}

/**
 * Literal ::= ['-'] integerLiteral
 *           | ['-'] floatingPointLiteral
 *           | booleanLiteral
 *           | characterLiteral
 *           | stringLiteral
 *           | symbolLiteral
 *           | 'null'
 */
void ScalaParser::parseLiteral(spLiteral &literal) {
  // ['-'] integerLiteral
  // ['-'] floatingPointLiteral
  bool seenMinus = false;
  if (lexer->getCurToken() == STok::MINUS) {
    seenMinus = true;
    literal->tokMinus = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume '-'
  }

  STok tok = lexer->getCurToken();
  if (tok == STok::DECIMAL_NUMERAL
    || tok == STok::DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX
    || tok == STok::HEX_NUMERAL
    || tok == STok::HEX_NUMERAL_WITH_INT_TYPE_SUFFIX
    || tok == STok::OCTAL_NUMERAL) {

    literal->opt = Literal::Opt::INTEGER;
    literal->intLit = spIntegerLiteral(new IntegerLiteral);
    literal->intLit->ini = lexer->getCurTokenIni();
    literal->intLit->end = lexer->getCurTokenEnd();
    literal->intLit->val = lexer->getCurTokenStr();

    lexer->getNextToken(); // consume decimal
    return;
  }

  if (tok == STok::DECIMAL_FLOATING_POINT_LITERAL
    || tok == STok::HEXADECIMAL_FLOATING_POINT_LITERAL) {

    literal->opt = Literal::Opt::FLOATING_POINT;
    literal->fpLit = spFloatingPointLiteral(new FloatingPointLiteral);
    literal->fpLit->ini = lexer->getCurTokenIni();
    literal->fpLit->end = lexer->getCurTokenEnd();
    literal->fpLit->val = lexer->getCurTokenStr();

    lexer->getNextToken(); // consume fp
    return;
  }

  if (seenMinus) { literal->addErr(-1); return; }

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
  // TODO: 'null'

  literal->addErr(-1);
}

/**
 * LocalModifier ::= 'abstract'
 *                 | 'final'
 *                 | 'sealed'
 *                 | 'implicit'
 *                 | 'lazy'
 */
void ScalaParser::parseLocalModifier(spLocalModifier &localModifier) {
  if (!isLocalModifier(lexer->getCurToken())) {
    localModifier->addErr(-1);
    return;
  }

  localModifier->tok = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume LocalModifier
}

/**
 * Modifier ::= LocalModifier
 *            | AccessModifier
 *            | 'override'
 */
void ScalaParser::parseModifier(spModifier &modifier) {
  if (isLocalModifier(lexer->getCurToken())) {
    modifier->opt = Modifier::Opt::LOCAL;
    modifier->localModifier = spLocalModifier(new LocalModifier);
    parseLocalModifier(modifier->localModifier);
    if (modifier->localModifier->err) {
      modifier->addErr(-1);
    }
    return;
  }

  if (isAccessModifier(lexer->getCurToken())) {
    modifier->opt = Modifier::Opt::ACCESS;
    modifier->accessModifier = spAccessModifier(new AccessModifier);
    parseAccessModifier(modifier->accessModifier);
    if (modifier->accessModifier->err) {
      modifier->addErr(-1);
    }
    return;
  }

  if (lexer->getCurToken() == STok::OVERRIDE) {
    modifier->opt = Modifier::Opt::OVERRIDE;
    modifier->tokOverride = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'override'
    return;
  }

  modifier->addErr(-1);
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
 * Packaging ::= 'package' QualId [nl] '{' TopStatSeq '}'
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

  // TODO: [nl] '{' TopStatSeq '}'
}

/**
 * Param ::= {Annotation} id [':' ParamType] ['=' Expr]
 */
void ScalaParser::parseParam(spParam &param) {
  // TODO: {Annotation}

  // id
  param->id = parseLexId();
  lexer->getNextToken(); // consume id
  if (param->id->err) {
    param->addErr(-1);
    return;
  }

  // [':' ParamType]
  if (lexer->getCurToken() == STok::COLON) {
    param->tokColon = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume ':'

    param->paramType = spParamType(new ParamType);
    parseParamType(param->paramType);
    if (param->paramType->err) {
      param->addErr(-1);
      return;
    }
  }

  // ['=' Expr]
}

/**
 * Params ::= Param {',' Param}
 */
void ScalaParser::parseParams(spParams &params) {
  // Param
  params->param = spParam(new Param);
  parseParam(params->param);
  if (params->param->err) {
    params->addErr(-1);
    return;
  }

  // {',' Param}
  State state;
  while (lexer->getCurToken() == STok::COMMA) {
    saveState(state);

    auto comma = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume ','

    auto param = spParam(new Param);
    parseParam(param);
    if (param->err) {
      restoreState(state);
      return;
    }

    params->pairs.push_back(std::make_pair(comma, param));
  }
}

/**
 * ParamClause ::= [nl] '(' [Params] ')'
 */
void ScalaParser::parseParamClause(spParamClause &paramClause) {
  // [nl] is ignored by the lexer
  if (lexer->getCurToken() != STok::LPAREN) {
    paramClause->addErr(c4::ERR_EXP_LPAREN);
    return;
  }

  paramClause->tokLParen = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '('

  // [Params]
  if (lexer->getCurToken() != STok::RPAREN) {
    paramClause->params = spParams(new Params);
    parseParams(paramClause->params);
    if (paramClause->params->err) {
      paramClause->addErr(-1);
      return;
    }
  }

  if (lexer->getCurToken() != STok::RPAREN) {
    paramClause->addErr(c4::ERR_EXP_RPAREN);
    return;
  }

  paramClause->tokRParen = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume ')'
}

/**
 * ParamClauses ::= {ParamClause} [[nl] '(' 'implicit' Params ')']
 */
void ScalaParser::parseParamClauses(spParamClauses &paramClauses) {
  // {ParamClause}
  State state;
  while (true) {
    saveState(state);
    auto paramClause = spParamClause(new ParamClause);
    parseParamClause(paramClause);
    if (paramClause->err) {
      restoreState(state);
      break;
    }

    paramClauses->paramClauses.push_back(paramClause);
  }

  // [[nl] '(' 'implicit' Params ')']
  if (lexer->getCurToken() != STok::LPAREN) {
    return;
  }

  paramClauses->tokLParen = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '('

  if (lexer->getCurToken() != STok::IMPLICIT) {
    paramClauses->addErr(c4::ERR_EXP_IMPLICIT);
    return;
  }

  paramClauses->tokImplicit = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume 'implicit'

  paramClauses->params = spParams(new Params);
  parseParams(paramClauses->params);
  if (paramClauses->params->err) {
    paramClauses->addErr(-1);
    return;
  }

  if (lexer->getCurToken() != STok::RPAREN) {
    paramClauses->addErr(c4::ERR_EXP_RPAREN);
    return;
  }

  paramClauses->tokRParen = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume ')'
}

/**
 * ParamType ::= Type
 *             | '=>' Type
 *             | Type '*'
 */
void ScalaParser::parseParamType(spParamType &paramType) {
  // Type
  paramType->opt = ParamType::Opt::TYPE;
  paramType->type = spType(new Type);
  parseType(paramType->type);
  if (paramType->type->err) {
    paramType->addErr(-1);
    return;
  }

  // TODO: '=>' Type
  // TODO: Type '*'
}

/**
 * Path ::= StableId
 *        | [id '.'] 'this'
 */
void ScalaParser::parsePath(spPath &path) {
  path->opt = Path::Opt::STABLE_ID;
  path->stableId = spStableId(new StableId);
  parseStableId(path->stableId);
  if (path->stableId->err) {
    path->addErr(-1);
    return;
  }

  // TODO: [id '.'] 'this'
}

/**
 * PeriodId ::= '.' id
 */
void ScalaParser::parsePeriodId(spPeriodId &periodId) {
  // '.'
  if (lexer->getCurToken() != STok::PERIOD) {
    periodId->addErr(c4::ERR_EXP_PERIOD);
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
  // TODO: ['-' | '+' | '~' | '!']
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
 * QualId ::= id {'.' id}
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
 * SimpleExpr ::= 'new' (ClassTemplate | TemplateBody)
 *              | BlockExpr
 *              | SimpleExpr1 ['_']
 */
void ScalaParser::parseSimpleExpr(spSimpleExpr &simpleExpr) {
  // 'new' (ClassTemplate | TemplateBody)
  if (lexer->getCurToken() == STok::NEW) {
    // 'new'
    simpleExpr->opt = SimpleExpr::Opt::NEW;
    simpleExpr->tokNew = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'new'

    // We first try
    // ClassTemplate
    State state;
    saveState(state);
    spClassTemplate classTmpl = spClassTemplate(new ClassTemplate);
    parseClassTemplate(classTmpl);
    if (classTmpl->err == false) {
      simpleExpr->classTmpl = classTmpl;
      return;
    }

    // TemplateBody
    restoreState(state);
    spTemplateBody tmplBody = spTemplateBody(new TemplateBody);
    parseTemplateBody(tmplBody);
    if (tmplBody->err) {
      simpleExpr->addErr(-1);
    }

    return;
  }

  // TODO: BlockExpr

  // SimpleExpr1 ['_']
  simpleExpr->opt = SimpleExpr::Opt::SIMPLE_EXPR1;
  simpleExpr->simpleExpr1 = spSimpleExpr1(new SimpleExpr1);
  parseSimpleExpr1(simpleExpr->simpleExpr1);
  if (simpleExpr->simpleExpr1->err) {
    simpleExpr->addErr(-1);
  }

  // TODO: ['_']
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
 *                   | '_'
 *                   | '(' [Exprs] ')'
 *                   | SimpleExpr '.' id
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

  // TODO: '_'
  // TODO: '(' [Exprs] ')'
  // TODO: SimpleExpr '.' id
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

  // SimpleTypeTails
  State state;
  saveState(state);
  auto tails = spSimpleTypeTails(new SimpleTypeTails);
  parseSimpleTypeTails(tails);
  if (tails->err) {
    restoreState(state);
  }

  simpleType->tails = tails;
}

/**
 * SimpleTypeHead ::= StableId
 *                  | Path '.' 'type'
 *                  | '(' Types ')'
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

  // TODO: Path '.' 'type'
  // TODO: '(' Types ')'
}

/**
 * SimpleTypeTail ::= TypeArgs | '#' id
 */
void ScalaParser::parseSimpleTypeTail(spSimpleTypeTail &tail) {
  // TypeArgs
  tail->opt = SimpleTypeTail::Opt::TYPE_ARGS;
  tail->typeArgs = spTypeArgs(new TypeArgs);
  parseTypeArgs(tail->typeArgs);
  if (tail->typeArgs->err) {
    tail->addErr(-1);
    return;
  }

  // TODO: '#' id
}

/**
 * SimpleTypeTails ::= SimpleTypeTail SimpleTypeTails | SimpleTypeTail
 */
void ScalaParser::parseSimpleTypeTails(spSimpleTypeTails &tails) {
  // SimpleTypeTail
  tails->tail = spSimpleTypeTail(new SimpleTypeTail);
  parseSimpleTypeTail(tails->tail);
  if (tails->tail->err) {
    tails->addErr(-1);
    return;
  }

  // SimpleTypeTails
  State state;
  saveState(state);
  auto tails2 = spSimpleTypeTails(new SimpleTypeTails);
  parseSimpleTypeTails(tails2);
  if (tails2->err) {
    restoreState(state);
    return;
  }

  tails->tails = tails2;
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
 *                | [IdPeriod] 'this' PeriodId
 *                | [IdPeriod] 'super' [ClassQualifier] PeriodId
 */
void ScalaParser::parseStableIdHead(spStableIdHead &head) {
  spLexId id = parseLexId();
  lexer->getNextToken(); // consume 'id'
  if (id->err == false) {
    head->opt = StableIdHead::Opt::ID;
    head->id = id;
    return;
  }

  // TODO: [IdPeriod] 'this' PeriodId
  // TODO: [IdPeriod] 'super' [ClassQualifier] PeriodId
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
 * TemplateBody ::= [nl] '{' [SelfType] [TemplateStat {semi TemplateStat}] '}'
 */
void ScalaParser::parseTemplateBody(spTemplateBody &tmplBody) {
  // '{'
  if (lexer->getCurToken() != STok::LCURLYB) {
    tmplBody->addErr(c4::ERR_EXP_LCURLY_BRACKET);
    return;
  }

  tmplBody->tokLCurlyB = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '{'

  // [nl] '{}'
  if (lexer->getCurToken() == STok::RCURLYB) {
    tmplBody->tokRCurlyB = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume '}'
    return;
  }

  // TODO: [SelfType]

  // TemplateStat
  tmplBody->tmplStat = spTemplateStat(new TemplateStat);
  parseTemplateStat(tmplBody->tmplStat);
  if (tmplBody->tmplStat->err) {
    tmplBody->addErr(-1);
    return;
  }

  // {semi TemplateStat}
  while (lexer->getCurToken() != STok::RCURLYB) {
    auto semi = spSemi(new Semi);
    parseSemi(semi);
    if (semi->err) {
      return;
    }

    auto tmplStat = spTemplateStat(new TemplateStat);
    parseTemplateStat(tmplStat);
    if (tmplStat->err) {
      tmplBody->addErr(-1);
      break;
    }

    tmplBody->pairs.push_back(std::make_pair(semi, tmplStat));
  }

  // '}'
  if (lexer->getCurToken() != STok::RCURLYB) {
    tmplBody->addErr(c4::ERR_EXP_RCURLY_BRACKET);
    return;
  }

  tmplBody->tokRCurlyB = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '}'
}

/**
 * TemplateStat ::= Import
 *                | {Annotation [nl]} {Modifier} Def
 *                | {Annotation [nl]} {Modifier} Dcl
 *                | Expr
 */
void ScalaParser::parseTemplateStat(spTemplateStat &tmplStat) {
  // Import
  if (lexer->getCurToken() == STok::IMPORT) {
    tmplStat->opt = TemplateStat::Opt::IMPORT;
    tmplStat->import = spImport(new Import);
    parseImport(tmplStat->import);
    if (tmplStat->import->err) {
      tmplStat->addErr(-1);
    }
    return;
  }


  bool seenAnnotation = false;

  // {Annotation [nl]}
  while (lexer->getCurToken() == STok::AT) {
    seenAnnotation = true;
    auto annotation = spAnnotation(new Annotation);
    parseAnnotation(annotation);
    if (annotation->err) {
      tmplStat->addErr(-1);
      break;
    }

    tmplStat->annotations.push_back(annotation);
  }

  bool seenModifier = false;
  while (isModifier(lexer->getCurToken())) {
    seenModifier = true;
    auto modifier = spModifier(new Modifier);
    parseModifier(modifier);
    if (modifier->err) {
      tmplStat->addErr(-1);
      break;
    }

    tmplStat->modifiers.push_back(modifier);
  }

  {
    // Def
    tmplStat->opt = TemplateStat::Opt::DEF;
    tmplStat->def = spDef(new Def);
    parseDef(tmplStat->def);
    if (tmplStat->def->err) {
      tmplStat->addErr(-1);
      return;
    }
  }

  // TODO: Dcl

  if (!seenAnnotation && !seenModifier) {
    // TODO: Expr
  }
}

/**
 * TmplDef ::= ['case'] 'class' ClassDef
 *           | ['case'] 'object' ObjectDef
 *           | 'trait' TraitDef
 */
void ScalaParser::parseTmplDef(spTmplDef &tmplDef) {
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

  // 'case'
  if (lexer->getCurToken() == STok::CASE) {
    tmplDef->tokCase = lexer->getCurTokenNode();
    lexer->getNextToken();
  }

  // ['case'] 'class' ClassDef
  if (lexer->getCurToken() == STok::CLASS) {
    tmplDef->opt = TmplDef::Opt::CASE_CLASS;

    tmplDef->tokClass = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'class'

    tmplDef->classDef = spClassDef(new ClassDef);
    parseClassDef(tmplDef->classDef);
    if (tmplDef->classDef->err) {
      tmplDef->addErr(-1);
    }

    return;
  }

  // ['case'] 'object' ObjectDef
  if (lexer->getCurToken() == STok::OBJECT) {
    tmplDef->opt = TmplDef::Opt::CASE_OBJECT;

    tmplDef->tokObject = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume 'object'

    tmplDef->objectDef = spObjectDef(new ObjectDef);
    parseObjectDef(tmplDef->objectDef);
    if (tmplDef->objectDef->err) {
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

  // {Annotation [nl]} {Modifier} TmplDef
  // {Annotation [nl]}
  while (lexer->getCurToken() == STok::AT) {
    auto annotation = spAnnotation(new Annotation);
    parseAnnotation(annotation);
    if (annotation->err) {
      break;
    }

    topStat->annotations.push_back(annotation);
  }

  // TODO: {Modifier}

  // TmplDef
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

  // [TypeParamClause]
  if (lexer->getCurToken() == STok::LBRACKET) {
    traitDef->typeParamClause = spTypeParamClause(new TypeParamClause);
    parseTypeParamClause(traitDef->typeParamClause);
    if (traitDef->typeParamClause->err) {
      traitDef->addErr(-1);
      return;
    }
  }

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
  // [TemplateBody]
  if (lexer->getCurToken() != STok::EXTENDS) {
    if (lexer->getCurToken() != STok::LCURLYB) {
      return;
    }

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
    traitTemplateOpt->addErr(c4::ERR_EXP_TRAIT);
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

/**
 * Type ::= FunctionArgTypes '=>' Type
 *        | InfixType [ExistentialClause]
 */
void ScalaParser::parseType(spType &type) {
  // TODO: FunctionArgTypes '=>' Type

  // InfixType [ExistentialClause]
  type->opt = Type::Opt::INFIX_TYPE;
  type->infixType = spInfixType(new InfixType);
  parseInfixType(type->infixType);
  if (type->infixType->err) {
    type->addErr(-1);
    return;
  }

  // TODO: [ExistentialClause]
}

/**
 * Types ::= Type {',' Type}
 */
void ScalaParser::parseTypes(spTypes &types) {
  types->type = spType(new Type);
  parseType(types->type);
  if (types->type->err) {
    types->addErr(-1);
    return;
  }

  State state;
  while (lexer->getCurToken() == STok::COMMA) {
    saveState(state);
    auto comma = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume ','

    auto type = spType(new Type);
    parseType(type);
    if (type->err) {
      restoreState(state);
      return;
    }

    types->pairs.push_back(std::make_pair(comma, type));
  }
}

/**
 * TypeArgs ::= '[' Types ']'
 */
void ScalaParser::parseTypeArgs(spTypeArgs &typeArgs) {
  // '['
  if (lexer->getCurToken() != STok::LBRACKET) {
    typeArgs->addErr(c4::ERR_EXP_LBRACKET);
    return;
  }

  typeArgs->tokLBracket = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '['

  // Types
  typeArgs->types = spTypes(new Types);
  parseTypes(typeArgs->types);
  if (typeArgs->types->err) {
    typeArgs->addErr(-1);
    return;
  }

  // ']'
  if (lexer->getCurToken() != STok::RBRACKET) {
    typeArgs->addErr(c4::ERR_EXP_RBRACKET);
    return;
  }

  typeArgs->tokRBracket = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume ']'
}

/**
 * TypeParam ::= (id | '_') [TypeParamClause] ['>:' Type] ['<:' Type]
 *               {'<%' Type} {':' Type}
 */
void ScalaParser::parseTypeParam(spTypeParam &typeParam) {
  // (id | '_')
  if (lexer->getCurToken() == STok::UNDERSCORE) {
    typeParam->tokUnderscore = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume '_'
  } else {
    typeParam->id = parseLexId();
    lexer->getNextToken();

    if (typeParam->id->err) {
      typeParam->id->addErr(1);
      return;
    }
  }

  // TODO: [TypeParamClause]
  // TODO: ['>:' Type] ['<:' Type]
  // TODO: {'<%' Type} {':' Type}
}

/**
 * TypeParamClause ::= '[' VariantTypeParam {',' VariantTypeParam} ']'
 */
void ScalaParser::parseTypeParamClause(spTypeParamClause &typeParamClause) {
  // '['
  if (lexer->getCurToken() != STok::LBRACKET) {
    typeParamClause->addErr(c4::ERR_EXP_LBRACKET);
    return;
  }

  typeParamClause->tokLBracket = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume '['

  // VariantTypeParam
  typeParamClause->varTypeParam = spVariantTypeParam(new VariantTypeParam);
  parseVariantTypeParam(typeParamClause->varTypeParam);
  if (typeParamClause->varTypeParam->err) {
    typeParamClause->addErr(-1);
    return;
  }

  // {',' VariantTypeParam}
  State state;
  while (lexer->getCurToken() == STok::COMMA) {
    saveState(state);

    auto comma = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume ','

    auto varTypeParam = spVariantTypeParam(new VariantTypeParam);
    parseVariantTypeParam(varTypeParam);
    if (varTypeParam->err) {
      restoreState(state);
      break;
    }

    typeParamClause->pairs.push_back(std::make_pair(comma, varTypeParam));
  }

  // ']'
  if (lexer->getCurToken() != STok::RBRACKET) {
    typeParamClause->addErr(c4::ERR_EXP_RBRACKET);
    return;
  }

  typeParamClause->tokRBracket = lexer->getCurTokenNode();
  lexer->getNextToken(); // consume ']'
}

/**
 * VariantTypeParam ::= {Annotation} ['+' | '-'] TypeParam
 */
void ScalaParser::parseVariantTypeParam(spVariantTypeParam &varTypeParam) {
  // {Annotation}
  while (lexer->getCurToken() == STok::AT) {
    auto annotation = spAnnotation(new Annotation);
    parseAnnotation(annotation);
    if (annotation->err) {
      break;
    }

    varTypeParam->annotations.push_back(annotation);
  }

  // ['+' | '-']
  if (lexer->getCurToken() == STok::PLUS) {
    varTypeParam->tokPlus = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume '+'
  } else if (lexer->getCurToken() == STok::MINUS) {
    varTypeParam->tokMinus = lexer->getCurTokenNode();
    lexer->getNextToken(); // consume '-'
  }

  // TypeParam
  varTypeParam->typeParam = spTypeParam(new TypeParam);
  parseTypeParam(varTypeParam->typeParam);
  if (varTypeParam->typeParam->err) {
    varTypeParam->addErr(-1);
  }
}

void ScalaParser::parse() {
  buildParseTree();
  comments = lexer->getComments();
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

bool ScalaParser::isAccessModifier(STok tok) {
  if (tok == STok::PRIVATE || tok == STok::PROTECTED) {
    return true;
  }

  return false;
}

bool ScalaParser::isLocalModifier(STok tok) {
  if (tok == STok::ABSTRACT
    || tok == STok::FINAL
    || tok == STok::SEALED
    || tok == STok::IMPLICIT
    || tok == STok::LAZY
  ) {
    return true;
  }

  return false;
}

bool ScalaParser::isModifier(STok tok) {
  if (isLocalModifier(tok) || isAccessModifier(tok) || tok == STok::OVERRIDE) {
    return true;
  }

  return false;
}
} // namespace
