#include <iostream>
#include "djp/ScalaParser.h"
#include "gtest/gtest.h"
using namespace djp;

/**
 * -----------------------------------------------------------------------------
 * object HelloWorld extends App { println("Hello world"); }
 * -----------------------------------------------------------------------------
 * TopStatSeq
 *   TopStat
 *     TmplDef(2)
 *       'object'
 *       ObjectDef
 *         id                          <-- HelloWorld
 *         ClassTemplateOpt(1)
 *           'extends'
 *           ClassTemplate             check [TemplateBody]
 *             ClassParents
 *               Constr
 *                 AnnoType
 *                   SimpleType(3)
 *                     StableId(1)
 *                       id            <-- App
 *                 ArgumentExprs[0](3)
 *                   BlockExpr[0](2)
 *                     '{'
 *                     Block
 *                       BlockStat(4)
 *                         Expr1*
 *                       semi          <-- ;
 *                     '}'
 *
 *
 * Expr1(10)*
 *   PostfixExpr
 *     InfixExpr(1)
 *       PrefixExpr
 *         SimpleExpr(3)
 *           SimpleExpr1(1)
 *             SimpleExpr1Head(2)
 *               Path
 *                 Stableid
 *                   id <-- println
 *             SimpleExpr1Tail(2)
 *               ArgumentExprs(1)
 *                 '('
 *                 Exprs
 *                   Expr(2)
 *                     Expr1(10)
 *                       PostfixExpr
 *                         InfixExpr(1)
 *                           PrefixExpr
 *                             SimpleExpr(3)
 *                               SimpleExpr1(2)
 *                                 SimpleExpr1Head(1)
 *                                   Literal(5)
 *                                     stringLiteral(1)
 *                                       "
 *                                       {stringElement} <-- "Hello world"
 *                                       "
 *                 ')'
 */
TEST(ScalaParser, HelloWorld) {
  std::string filename = "HelloWorld.scala";
  std::string buffer =
    "object HelloWorld extends App { println(\"Hello world\"); }";
  ScalaParser parser(filename, buffer);
  parser.parse();

  spTopStat topStat = parser.compUnit->topStatSeq->topStat;
  ASSERT_EQ(TopStat::Opt::TMPL_DEF, topStat->opt);

  spTmplDef tmplDef = topStat->tmplDef;
  ASSERT_EQ(TmplDef::Opt::CASE_OBJECT, tmplDef->opt);

  ASSERT_EQ(STok::OBJECT, tmplDef->tokObject->tok);
  ASSERT_EQ(0, tmplDef->tokObject->ini);
  ASSERT_EQ(6, tmplDef->tokObject->end);
  spObjectDef objectDef = tmplDef->objectDef;
  ASSERT_EQ("HelloWorld", objectDef->id->val);
  ASSERT_EQ(7, objectDef->id->ini);
  ASSERT_EQ(17, objectDef->id->end);

  spClassTemplateOpt classTmplOpt = objectDef->classTmplOpt;
  ASSERT_EQ(ClassTemplateOpt::Opt::CLASS_TEMPLATE, classTmplOpt->opt);
  ASSERT_EQ(STok::EXTENDS, classTmplOpt->tokExtends->tok);
  ASSERT_EQ(18, classTmplOpt->tokExtends->ini);
  ASSERT_EQ(25, classTmplOpt->tokExtends->end);

  spConstr constr = classTmplOpt->classTmpl->classParents->constr;

  spSimpleType simpleType = constr->annotType->simpleType;
  ASSERT_EQ(SimpleType::Opt::STABLE_ID, simpleType->opt);
  spStableId stableId  = simpleType->stableId;
  ASSERT_EQ(StableId::Opt::ID, stableId->opt);
  ASSERT_EQ("App", stableId->id->val);
  ASSERT_EQ(26, stableId->id->ini);
  ASSERT_EQ(29, stableId->id->end);

  ASSERT_EQ(1, constr->argExprs.size());
  spArgumentExprs argExprs = constr->argExprs[0];
  ASSERT_EQ(ArgumentExprs::Opt::BLOCK_EXPR, argExprs->opt);

  spBlockExpr blockExpr = argExprs->blockExpr;
  ASSERT_EQ(BlockExpr::Opt::BLOCK, blockExpr->opt);
  ASSERT_EQ(STok::LCURLYB, blockExpr->tokLCurlyB->tok);
  ASSERT_EQ(30, blockExpr->tokLCurlyB->ini);
  ASSERT_EQ(31, blockExpr->tokLCurlyB->end);
  ASSERT_EQ(STok::RCURLYB, blockExpr->tokRCurlyB->tok);
  ASSERT_EQ(56, blockExpr->tokRCurlyB->ini);
  ASSERT_EQ(57, blockExpr->tokRCurlyB->end);

  spBlock block = blockExpr->block;
  ASSERT_EQ(1, block->paBlockStatSemi.size());
  spBlockStat blockStat = block->paBlockStatSemi[0].first;
  ASSERT_EQ(BlockStat::Opt::EXPR1, blockStat->opt);
  ASSERT_EQ(Expr1::Opt::POSTFIX_EXPR, blockStat->expr1->opt);

  spInfixExpr infixExpr = blockStat->expr1->postfixExpr->infixExpr;
  ASSERT_EQ(InfixExpr::Opt::PREFIX, infixExpr->opt);

  spSimpleExpr simpleExpr = infixExpr->prefixExpr->simpleExpr;
  ASSERT_EQ(SimpleExpr::Opt::SIMPLE_EXPR1, simpleExpr->opt);

  spSimpleExpr1Head head = simpleExpr->simpleExpr1->head;
  ASSERT_EQ(SimpleExpr1Head::Opt::PATH, head->opt);
  ASSERT_EQ(Path::Opt::STABLE_ID, head->path->opt);
  ASSERT_EQ(StableId::Opt::ID, head->path->stableId->opt);
  ASSERT_EQ("println", head->path->stableId->id->val);

  spSimpleExpr1Tail tail = simpleExpr->simpleExpr1->tail;
  ASSERT_EQ(ArgumentExprs::Opt::EXPRS, tail->argExprs->opt);
  ASSERT_EQ(39, tail->argExprs->tokLParen->ini);
  ASSERT_EQ(40, tail->argExprs->tokLParen->end);
  ASSERT_EQ(53, tail->argExprs->tokRParen->ini);
  ASSERT_EQ(54, tail->argExprs->tokRParen->end);

  ASSERT_EQ(Expr::Opt::EXPR1, tail->argExprs->exprs->expr->opt);
  ASSERT_EQ(Expr1::Opt::POSTFIX_EXPR, tail->argExprs->exprs->expr->expr1->opt);
  {
    spInfixExpr infixExpr =
      tail->argExprs->exprs->expr->expr1->postfixExpr->infixExpr;
    ASSERT_EQ(InfixExpr::Opt::PREFIX, infixExpr->opt);

    spSimpleExpr simpleExpr = infixExpr->prefixExpr->simpleExpr;
    ASSERT_EQ(SimpleExpr::Opt::SIMPLE_EXPR1, simpleExpr->opt);

    spSimpleExpr1Head head = simpleExpr->simpleExpr1->head;
    ASSERT_EQ(SimpleExpr1Head::Opt::LITERAL, head->opt);
    ASSERT_EQ(Literal::Opt::STRING, head->literal->opt);
    ASSERT_EQ("\"Hello world\"", head->literal->strLit->val);
    ASSERT_EQ(40, head->literal->strLit->ini);
    ASSERT_EQ(53, head->literal->strLit->end);
  }

  spSemi semi = block->paBlockStatSemi[0].second;
  ASSERT_EQ(Semi::Opt::SEMICOLON, semi->opt);
  ASSERT_EQ(STok::SEMICOLON, semi->tokSemiColon->tok);
  ASSERT_EQ(54, semi->tokSemiColon->ini);
  ASSERT_EQ(55, semi->tokSemiColon->end);
}
