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
 *     TmplDef
 *       'object'
 *       ObjectDef
 *         id                          <-- HelloWorld
 *         ClassTemplateOpt
 *           'extends'
 *           ClassTemplate
 *             ClassParents
 *               Constr
 *                 AnnoType
 *                   SimpleType
 *                     StableId
 *                       id            <-- App
 *               ArgumentExprs
 *                 BlockExpr
 *                   '{'
 *                   Block
 *                     BlockStat
 *                       Expr1*
 *                   '}'
 *
 *
 * Expr1*
 *   PostfixExpr
 *     InfixExpr
 *       PrefixExpr
 *         SimpleExpr
 *           SimpleExpr1
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
