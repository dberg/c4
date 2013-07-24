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
 * Expr1(10)*
 *   PostfixExpr
 *     InfixExpr(1)
 *       PrefixExpr
 *         SimpleExpr(3)
 *           SimpleExpr1(7)
 *             SimpleExpr1(2)
 *               Path
 *                 Stableid
 *                   id <-- println
 *             ArgumentExprs(1)
 *               '('
 *               Exprs
 *                 Expr(2)
 *                   Expr1(10)
 *                     PostfixExpr
 *                       InfixExpr(1)
 *                         PrefixExpr
 *                           SimpleExpr
 *                             SimpleExpr1(3)
 *                               Literal(5)
 *                                 stringLiteral(1)
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
