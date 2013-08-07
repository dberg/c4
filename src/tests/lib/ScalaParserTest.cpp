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
 *                             SimpleExpr
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
  spObjectDef objectDef = tmplDef->objectDef;
  ASSERT_EQ("HelloWorld", objectDef->id->val);

  spClassTemplateOpt classTmplOpt = objectDef->classTmplOpt;
  ASSERT_EQ(ClassTemplateOpt::Opt::CLASS_TEMPLATE, classTmplOpt->opt);
  ASSERT_EQ(STok::EXTENDS, classTmplOpt->tokExtends->tok);

  spConstr constr = classTmplOpt->classTmpl->classParents->constr;
  spSimpleType simpleType = constr->annotType->simpleType;
  ASSERT_EQ(SimpleType::Opt::STABLE_ID, simpleType->opt);
  spStableId stableId  = simpleType->stableId;
  ASSERT_EQ(StableId::Opt::ID, stableId->opt);
  ASSERT_EQ("App", stableId->id->val);

  ASSERT_EQ(1, constr->argExprs.size());

  // TODO:
}
