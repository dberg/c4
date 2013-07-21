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
 *                   SimpleType
 *                     StableId
 *                       id            <-- App
 *                 ArgumentExprs[0](3)
 *                   BlockExpr[0](2)
 *                     '{'
 *                     Block
 *                       BlockStat(4)
 *                         Expr1*
 *                     '}'
 *
 *
 * Expr1*
 *   PostfixExpr
 *     InfixExpr
 *       PrefixExpr
 *         SimpleExpr(3)
 *           SimpleExpr1(7)
 *             SimpleExpr1
 *               Path
 *                 Stableid
 *                   id <-- println
 *             ArgumentExprs
 *               '('
 *               Exprs
 *                 Expr
 *                   Expr1
 *                     PostfixExpr
 *                       InfixExpr
 *                         PrefixExpr
 *                           SimpleExpr
 *                             SimpleExpr1
 *                               Literal
 *                                 stringLiteral
 *                                   "
 *                                   {stringElement} <-- "Hello world"
 *                                   "
 *               ')'
 */
TEST(ScalaParser, HelloWorld) {
  std::string filename = "HelloWorld.scala";
  std::string buffer =
    "object HelloWorld extends App { println(\"Hello world\"); }";
  ScalaParser parser(filename, buffer);
  //parser.parse();
}
