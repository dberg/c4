#include <iostream>
#include "c4/ScalaParser.h"
#include "gtest/gtest.h"
using namespace c4s;

/**
 * -----------------------------------------------------------------------------
 * @Api(Array(new ApiParam(k = \"value\")))
 * trait X
 * -----------------------------------------------------------------------------
 * CompilationUnit
 *   TopStatSeq
 *     TopStat
 *       Annotation
 *         @                    <--- @
 *         SimpleType
 *           SimpleTypeHead
 *             StableId
 *               StableIdHead
 *                 id           <--- Api
 *         ArgumentExprs[0]
 *           '('                <--- (
 *           Exprs
 *             Expr
 *               Expr1*
 *           ')'
 *       TmplDef
 *
 * ---> Array(new ApiParam(k = \"value\"))
 * Expr1*
 *   PostfixExpr
 *     InfixExpr
 *       PrefixExpr
 *         SimpleExpr(3)
 *           SimpleExpr1
 *             SimpleExpr1Head
 *               Path
 *                 StableId     <-- Array
 *             SimpleExpr1Tail
 *               ArgumentExprs(1)
 *                 '('
 *                 Exprs
 *                   Expr
 *                     Expr1**
 *                 ')'
 *
 * ---> new ApiParam(k = \"value\")
 * Expr1**(10)
 *   PostfixExpr
 *     InfixExpr
 *       PrefixExpr
 *         SimpleExpr(1)
 *           'new'
 *           ClassTemplate
 *             ClassParents
 *               Constr
 *                 AnnotType
 *                   SimpleType
 *                     SimpleTypeHead
 *                       StableId
 *                         id          <--- ApiParam
 *                 ArgumentExprs[0]
 *                   '('
 *                   Exprs
 *                     Expr
 *                       Expr1***
 *                   ')'
 *
 * Expr1***(8)
 *   id                                 <-- k
 *   '='
 *   Expr
 *     Expr1
 *       PostfixExpr
 *         InfixExpr
 *           PrefixExpr
 *             SimpleExpr(3)
 *               SimpleExpr1
 *                 SimpleExpr1Head
 *                   Literal
 *                     stringLiteral    <-- "value"
 */
TEST(ScalaParser, Annotations) {
  std::string filename = "Example.scala";
  std::string buffer =
    "@Api(Array(new ApiParam(k = \"value\")))"
    "trait X";

  ScalaParser parser(filename, buffer);
  parser.parse();

  auto topStat = parser.compUnit->topStatSeq->topStat;
  ASSERT_EQ(TopStat::Opt::TMPL_DEF, topStat->opt);

  ASSERT_EQ(1, topStat->annotations.size());
  auto annotation = topStat->annotations[0];
  ASSERT_EQ(STok::AT, annotation->tokAt->tok);
  ASSERT_EQ("Api", annotation->simpleType->head->stableId->head->id->val);

  ASSERT_EQ(1, annotation->argExprsVec.size());
  auto argExprs1 = annotation->argExprsVec[0];
  ASSERT_EQ(ArgumentExprs::Opt::EXPRS, argExprs1->opt);
  ASSERT_EQ(STok::LPAREN, argExprs1->tokLParen->tok);
  ASSERT_EQ(STok::RPAREN, argExprs1->tokRParen->tok);

  auto expr1 = argExprs1->exprs->expr->expr1;
  ASSERT_EQ(Expr1::Opt::POSTFIX_EXPR, expr1->opt);

  auto simpleExpr =  expr1->postfixExpr->infixExpr->prefixExpr->simpleExpr;
  ASSERT_EQ(SimpleExpr::Opt::SIMPLE_EXPR1, simpleExpr->opt);

  {
    // (new ApiParam ...
    auto argExprs = simpleExpr->simpleExpr1->tail->argExprs;
    auto expr1 = argExprs->exprs->expr->expr1;

    auto simpleExpr = expr1->postfixExpr->infixExpr->prefixExpr->simpleExpr;
    ASSERT_EQ(SimpleExpr::Opt::NEW, simpleExpr->opt);

    {
      ASSERT_EQ(1,
        simpleExpr->classTmpl->classParents->constr->argExprsVec.size());
      auto argExprs =
        simpleExpr->classTmpl->classParents->constr->argExprsVec[0];

      auto expr1 = argExprs->exprs->expr->expr1;
      ASSERT_EQ(Expr1::Opt::ID_EQUALS_EXPR, expr1->opt);

      ASSERT_EQ("k", expr1->id->val);
      ASSERT_EQ(STok::EQUALS, expr1->tokEquals->tok);
      ASSERT_EQ("\"value\"",
       expr1->expr->expr1->postfixExpr->infixExpr->prefixExpr
       ->simpleExpr->simpleExpr1->head->literal->strLit->val);
    }
  }
}

/**
 * -----------------------------------------------------------------------------
 * // This is a comment
 * trait A
 * -----------------------------------------------------------------------------
 * CompilationUnit
 *   TopStatSeq
 *     TopStat(1)
 *       TmplDef(3)
 *         'trait'
 *         TraitDef
 *           id
 */
TEST(ScalaParser, Comments) {
  std::string filename = "Example.scala";
  std::string buffer =
    "// This is a comment\n"
    "trait A";

  ScalaParser parser(filename, buffer);
  parser.parse();

  ASSERT_EQ(1, parser.comments.size());

  auto topStat = parser.compUnit->topStatSeq->topStat;
  ASSERT_EQ(TopStat::Opt::TMPL_DEF, topStat->opt);

  auto tmplDef = topStat->tmplDef;
  ASSERT_EQ(TmplDef::Opt::TRAIT, tmplDef->opt);
  ASSERT_EQ(STok::TRAIT, tmplDef->tokTrait->tok);
  ASSERT_EQ("A", tmplDef->traitDef->id->val);
}

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
 *                     SimpleTypeHead(1)
 *                       StableId
 *                         StableIdHead(1)
 *                           id            <-- App
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
 *               Path(1)
 *                 StableId
 *                   StableIdHead(1)
 *                     id <-- println
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
  ASSERT_EQ(SimpleTypeHead::Opt::STABLE_ID, simpleType->head->opt);
  spStableId stableId  = simpleType->head->stableId;
  ASSERT_EQ(StableIdHead::Opt::ID, stableId->head->opt);
  ASSERT_EQ("App", stableId->head->id->val);
  ASSERT_EQ(26, stableId->head->id->ini);
  ASSERT_EQ(29, stableId->head->id->end);

  ASSERT_EQ(1, constr->argExprsVec.size());
  spArgumentExprs argExprs = constr->argExprsVec[0];
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
  ASSERT_EQ(StableIdHead::Opt::ID, head->path->stableId->head->opt);
  ASSERT_EQ("println", head->path->stableId->head->id->val);

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

/**
 * -----------------------------------------------------------------------------
 * import com.A.utils._
 * import com.B.C
 * import com.D.{E,F}
 * -----------------------------------------------------------------------------
 * CompilationUnit
 *   TopStatSeq
 *     TopStat(2)
 *       Import
 *         'import'
 *         ImportExpr
 *           QualId      <-- 'com.A.utils'
 *           '.'
 *           '_'
 *     semi
 *     TopStat(2)
 *       Import
 *         ImportExpr
 *           QualId      <-- 'com.B.C'
 *     semi
 *     TopStat(2)
 *       Import
 *         ImportExpr
 */
TEST(ScalaParser, Imports) {
  std::string filename = "Example.scala";
  std::string buffer =
    "import com.A.utils._\n"
    "import com.B.C\n"
    "import com.D.{E,F}\n";

  ScalaParser parser(filename, buffer);
  parser.parse();

  // import com.A.utils._
  {
    auto topStat = parser.compUnit->topStatSeq->topStat;
    ASSERT_EQ(TopStat::Opt::IMPORT, topStat->opt);

    auto import = topStat->import;
    ASSERT_EQ(STok::IMPORT, import->tokImport->tok);

    ASSERT_EQ("com", import->importExpr->qualId->id->val);
    ASSERT_EQ(2, import->importExpr->qualId->periodIds.size());

    {
      auto periodId = import->importExpr->qualId->periodIds[0];
      ASSERT_EQ(STok::PERIOD, periodId->tok->tok);
      ASSERT_EQ("A", periodId->id->val);
    }

    {
      auto periodId = import->importExpr->qualId->periodIds[1];
      ASSERT_EQ(STok::PERIOD, periodId->tok->tok);
      ASSERT_EQ("utils", periodId->id->val);
    }

    ASSERT_EQ(STok::PERIOD, import->importExpr->tokPeriod->tok);
    ASSERT_EQ(STok::UNDERSCORE, import->importExpr->tokUnderscore->tok);
  }

  ASSERT_EQ(2, parser.compUnit->topStatSeq->pairs.size());

  // import com.B.C

  // import com.D.{E,F}
  {
    auto topStat = parser.compUnit->topStatSeq->pairs[1].second;
    ASSERT_EQ(TopStat::Opt::IMPORT, topStat->opt);

    auto import = topStat->import;
    ASSERT_EQ(STok::IMPORT, import->tokImport->tok);

    ASSERT_EQ("com", import->importExpr->qualId->id->val);
    ASSERT_EQ(1, import->importExpr->qualId->periodIds.size());

    // .D
    {
      auto periodId = import->importExpr->qualId->periodIds[0];
      ASSERT_EQ(STok::PERIOD, periodId->tok->tok);
      ASSERT_EQ("D", periodId->id->val);
    }

    // .{E,F}
    auto selectors = import->importExpr->importSelectors;
    ASSERT_EQ(STok::LCURLYB, selectors->tokLCurlyB->tok);
    ASSERT_EQ("F", selectors->importSelector->id->val);
    ASSERT_EQ(1, selectors->pairs.size());
    ASSERT_EQ(STok::RCURLYB, selectors->tokRCurlyB->tok);
  }
}

/**
 * class A { def x = 10 }
 *
 * CompilationUnit
 *   TopStatSeq
 *     TopStat
 *       TmplDef(1)
 *         'class'
 *         ClassDef
 *           id
 *           ClassTemplateOpt
 *             TemplateBody
 *               '{'
 *               TemplateStat(2)
 *                 Def*
 *               '}'
 *
 * Def(2)*
 *   'def'
 *   FunDef(1)
 *     FunSig
 *       id                                  <-- x
 *     '='
 *     Expr
 *       Expr1(10)
 *         PostfixExpr
 *           InfixExpr
 *             PrefixExpr
 *               SimpleExpr(3)
 *                 SimpleExpr1
 *                   SimpleExpr1Head
 *                     Literal(1)
 *                       integerLiteral      <-- 10
 */
TEST(ScalaParser, Methods) {
  std::string filename = "Example.scala";
  std::string buffer = "class A { def x = 10 }";

  ScalaParser parser(filename, buffer);
  parser.parse();

  auto tmplDef = parser.compUnit->topStatSeq->topStat->tmplDef;
  ASSERT_EQ(TmplDef::Opt::CASE_CLASS, tmplDef->opt);
  ASSERT_EQ(STok::CLASS, tmplDef->tokClass->tok);
  ASSERT_EQ("A", tmplDef->classDef->id->val);

  auto classTmplOpt = tmplDef->classDef->classTmplOpt;
  ASSERT_EQ(ClassTemplateOpt::Opt::TEMPLATE_BODY, classTmplOpt->opt);

  auto tmplStat = classTmplOpt->tmplBody->tmplStat;
  ASSERT_EQ(TemplateStat::Opt::DEF, tmplStat->opt);

  auto def = tmplStat->def;
  ASSERT_EQ(Def::Opt::DEF, def->opt);
  ASSERT_EQ(STok::DEF, def->tokDef->tok);

  auto funDef = def->funDef;
  ASSERT_EQ(FunDef::Opt::FUN_SIG_EQUALS_EXPR, funDef->opt);
  ASSERT_EQ("x", funDef->funSig->id->val);
  ASSERT_EQ(STok::EQUALS, funDef->tokEquals->tok);

  auto simpleExpr = funDef->expr->expr1->postfixExpr
    ->infixExpr->prefixExpr->simpleExpr;
  ASSERT_EQ(SimpleExpr::Opt::SIMPLE_EXPR1, simpleExpr->opt);

  auto literal = simpleExpr->simpleExpr1->head->literal;
  ASSERT_EQ(Literal::Opt::INTEGER, literal->opt);
  ASSERT_EQ("10", literal->intLit->val);
}

/**
 * -----------------------------------------------------------------------------
 * import com.service
 *
 * object J {
 *   def p(i: String): Js = Service.pJs(i)
 * }
 * -----------------------------------------------------------------------------
 * CompilationUnit
 *   TopStatSeq
 *     TopStat(2)
 *       Import
 *         'import'
 *         ImportExpr
 *           QualId                                              <-- com.service
 *     TopStat[0](1)
 *       TmplDef(2)
 *         'object'
 *         ObjectDef
 *           id                                                  <-- J
 *           ClassTemplateOpt
 *             TemplateBody
 *               '{'
 *               TemplateStat(2)
 *                 Def(2)
 *                   'def'
 *                   FunDef(1)
 *                     FunSig
 *                       id                                      <-- p
 *                       ParamClauses
 *                         ParamClause[0]
 *                           '('
 *                           Params
 *                             Param
 *                               id                              <-- i
 *                               ':'
 *                               ParamType
 *                                 Type(2)
 *                                   InfixType
 *                                     CompoundType
 *                                       AnnotType
 *                                         SimpleType
 *                                           SimpleTypeHead
 *                                             StableId
 *                                               StableIdHead
 *                                                 id                <-- String
 *                           ')'
 *                     ':'
 *                     Type(2)
 *                       InfixType
 *                         CompoundType
 *                           AnnotType
 *                             SimpleType
 *                               SimpleTypeHead
 *                                 StableId
 *                                   StableIdHead
 *                                     id                                <-- Js
 *                     '='
 *                     Expr(2)
 *                       Expr1*
 *               '}'
 *
 * Expr1*(10)                                                 <-- Service.pJs(i)
 *   PostfixExpr
 *     InfixExpr(1)
 *       PrefixExpr
 *         SimpleExpr(3)
 *           SimpleExpr1
 *             SimpleExpr1Head(2)
 *               Path
 *                 StableId
 *                   StableIdHead
 *                     id                                            <-- Service
 *                   StableIdTail
 *                     PeriodId                                      <-- .pJs
 *             SimpleExpr1Tail
 *               ArgumentExprs
 *                 '('
 *                 Exprs
 *                   Expr(2)
 *                     Expr1**
 *                 ')'
 *
 * Expr1**(10)
 *   InfixExpr(1)
 *     PrefixExpr
 *       SimpleExpr(3)
 *         SimpleExpr1
 *           SimpleExpr1Head(2)
 *             Path
 *               StableId
 *                 StableIdHead(1)
 *                   id                                              <-- i
 */
TEST(ScalaParser, ObjectDefs) {
  std::string filename = "Example.scala";
  std::string buffer =
    "import com.service\n"
    "\n"
    "object J {\n"
    "  def p(i: String): Js = Service.pJs(i)\n"
    "}\n";

  ScalaParser parser(filename, buffer);
  parser.parse();

  // import
  ASSERT_EQ(TopStat::Opt::IMPORT, parser.compUnit->topStatSeq->topStat->opt);

  // object
  ASSERT_EQ(1, parser.compUnit->topStatSeq->pairs.size());
  auto topStat = parser.compUnit->topStatSeq->pairs[0].second;
  ASSERT_EQ(TopStat::Opt::TMPL_DEF, topStat->opt);
}

/**
 * -----------------------------------------------------------------------------
 * package test
 * import com.company.utils._
 * trait X extends Y with Z
 * -----------------------------------------------------------------------------
 * CompilationUnit
 *   Packaging
 *     'package'
 *     QualId
 *   TopStatSeq
 *     TopStat(2)
 *       Import
 *         'import'
 *         ImportExpr
 *           StableId
 *             StableIdHead   <-- com.company.utils
 *               id           <-- com
 *             StableIdTail
 *               PeriodId     <-- .company
 *               StableIdTail
 *                 PeriodId   <-- .utils
 *           '.'
 *           '_'
 *     semi
 *     TopStat(1)
 *       TmplDef(3)
 *         'trait'
 *         TraitDef
 *           id       <-- X
 *           TraitTemplateOpt(1)
 *             'extends'
 *             TraitTemplate
 *               TraitParents
 *                 AnnotType
 *                   SimpleType
 *                     SimpleTypeHead
 *                       StableId
 *                         StableIdHead
 *                           id    <-- Y
 *                 'with'
 *                 AnnotType
 *                   SimpleType
 *                     SimpleTypeHead
 *                       StableId
 *                         StableIdHead
 *                           id   <-- Z
 * -----------------------------------------------------------------------------
 */
TEST(ScalaParser, Trait) {
  std::string filename = "Example.scala";
  std::string buffer =
    "package test\n"
    "import com.company.utils._\n"
    "trait X extends Y with Z";

  ScalaParser parser(filename, buffer);
  parser.parse();

  {
    // package test
    ASSERT_EQ(1, parser.compUnit->tuples.size());
    auto tuple = parser.compUnit->tuples[0];

    auto tokPackage = std::get<0>(tuple);
    ASSERT_EQ(STok::PACKAGE, tokPackage->tok);

    auto qualId = std::get<1>(tuple);
    ASSERT_EQ("test", qualId->id->val);

    auto semi = std::get<2>(tuple);
    ASSERT_EQ(Semi::Opt::NL, semi->opt);
  }

  {
    // import com.company.utils._\n
    auto topStat = parser.compUnit->topStatSeq->topStat;
    ASSERT_EQ(TopStat::Opt::IMPORT, topStat->opt);

    auto import = topStat->import;
    ASSERT_EQ(STok::IMPORT, import->tokImport->tok);

    ASSERT_EQ("com", import->importExpr->qualId->id->val);

    {
      auto periodId = import->importExpr->qualId->periodIds[0];
      ASSERT_EQ(STok::PERIOD, periodId->tok->tok);
      ASSERT_EQ("company", periodId->id->val);
    }

    {
      auto periodId = import->importExpr->qualId->periodIds[1];
      ASSERT_EQ(STok::PERIOD, periodId->tok->tok);
      ASSERT_EQ("utils", periodId->id->val);
    }

    ASSERT_EQ(STok::PERIOD, import->importExpr->tokPeriod->tok);
    ASSERT_EQ(STok::UNDERSCORE, import->importExpr->tokUnderscore->tok);

    // trait X extends Y with Z
    {
      ASSERT_EQ(1, parser.compUnit->topStatSeq->pairs.size());
      auto topStat = parser.compUnit->topStatSeq->pairs[0].second;
      ASSERT_EQ(TopStat::Opt::TMPL_DEF, topStat->opt);

      auto tmplDef = topStat->tmplDef;
      ASSERT_EQ(TmplDef::Opt::TRAIT, tmplDef->opt);
      ASSERT_EQ(STok::TRAIT, tmplDef->tokTrait->tok);

      // X
      auto traitDef = tmplDef->traitDef;
      ASSERT_EQ("X", traitDef->id->val);

      auto tmplOpt = traitDef->traitTemplateOpt;
      ASSERT_EQ(TraitTemplateOpt::Opt::TRAIT_TEMPLATE, tmplOpt->opt);
      ASSERT_EQ(STok::EXTENDS, tmplOpt->tokExtends->tok);

      auto parents = tmplOpt->traitTemplate->traitParents;
      ASSERT_EQ("Y",
        parents->annotType->simpleType->head->stableId->head->id->val);
      ASSERT_EQ(1, parents->pairs.size());

      auto tokWith = parents->pairs[0].first;
      ASSERT_EQ(STok::WITH, tokWith->tok);

      auto annotType = parents->pairs[0].second;
      ASSERT_EQ("Z",
        annotType->simpleType->head->stableId->head->id->val);
    }
  }
}

/**
 * -----------------------------------------------------------------------------
 * import scala.annotation.implicitNotFound
 *
 * @implicitNotFound("message")
 * trait F[A] extends W[A] with R[A]
 * trait OF[A] extends OW[A] with R[A] with F[A]
 * -----------------------------------------------------------------------------
 * CompilationUnit
 *   TopStatSeq
 *     TopStat(2)
 *       Import
 *     TopStat[0](1)
 *       Annotation
 *       TmplDef(3)
 *         'trait'
 *         TraitDef
 *           id                                                  <-- F
 *           TypeParamClause                                     <-- [A]
 *             '['
 *               VariantTypeParam
 *                 TypeParam
 *                   id                                          <-- A
 *             ']'
 *           TraitTemplateOpt(1)
 *             'extends'
 *             TraitTemplate
 *               TraitParents
 *                 AnnotType
 *                   SimpleType
 *                     SimpleTypeHead
 *                       StableId
 *                         StableIdHead
 *                           id                                  <-- W
 *                     SimpleTypeTails
 *                       SimpleTypeTail
 *                         TypeArgs
 *                           '['
 *                           Types
 *                             Type(2)
 *                               InfixType
 *                                 CompoundType
 *                                   AnnotType
 *                                     SimpleType
 *                                       SimpleTypeHead
 *                                         StableId
 *                                           StableIdHead
 *                                             id                <-- A
 *
 *                           '['
 *
 *                 'with'
 *                 AnnotType
 *                   SimpleType
 *                     SimpleTypeHead
 *                       StableId
 *                         StableIdHead
 *                           id                                  <-- R
 *                     SimpleTypeTails
 *                       SimpleTypeTail
 *                         TypeArgs
 *                           '['
 *                           Types
 *                             Type(2)
 *                               InfixType
 *                                 CompoundType
 *                                   AnnotType
 *                                     SimpleType
 *                                       SimpleTypeHead
 *                                         StableId
 *                                           StableIdHead
 *                                             id                <-- A
 *                           ']'
 *     TopStat[1](1)
 */
TEST(ScalaParser, VariantTypeParam) {
  std::string filename = "Example.scala";
  std::string buffer =
    "import scala.annotation.implicitNotFound\n\n"
    "@implicitNotFound(\"message\")\n"
    "trait F[A] extends W[A] with R[A]\n"
    "trait OF[A] extends OW[A] with R[A] with F[A]";

  ScalaParser parser(filename, buffer);
  parser.parse();

  // import scala.annotation.implicitNotFound
  ASSERT_EQ(TopStat::Opt::IMPORT, parser.compUnit->topStatSeq->topStat->opt);

  // @implicitNotFound("message")
  // trait F[A] extends W[A] with R[A]
  // trait OF[A] extends OW[A] with R[A] with F[A]"
  ASSERT_EQ(2, parser.compUnit->topStatSeq->pairs.size());
  auto topStat = parser.compUnit->topStatSeq->pairs[0].second;
}

/**
 * -----------------------------------------------------------------------------
 * trait OF
 * object OF
 * -----------------------------------------------------------------------------
 * CompilationUnit
 *   TopStatSeq(1)
 *     TopStat(1)
 *       TmplDef(3)
 *         'trait'
 *         TraitDef
 *           id          <-- OF
 *   TopStatSeq[0](1)
 *     TopStat(1)
 *       TmplDef(2)
 *         'object'
 *           ObjectDef
 *             id         <-- OF
 *             ClassTemplateOpt
 */
TEST(ScalaParser, TraitAndObject) {
  std::string filename = "Example.scala";
  std::string buffer =
    "trait OF\n"
    "object OF\n";

  ScalaParser parser(filename, buffer);
  parser.parse();

  // trait OFormat
  ASSERT_EQ(TopStat::Opt::TMPL_DEF, parser.compUnit->topStatSeq->topStat->opt);

  // object OFormat
  ASSERT_EQ(1, parser.compUnit->topStatSeq->pairs.size());
}

/**
 * -----------------------------------------------------------------------------
 * object O {
 *   def f(): F = new F {
 *     def apply(): Int = 1
 *   }
 * }
 * -----------------------------------------------------------------------------
 * CompilationUnit
 *   TopStatSeq
 *     TopStat
 *       TmplDef(2)
 *         'object'
 *         ObjectDef
 *           id                                   <-- O
 *           ClassTemplateOpt(2)
 *             TemplateBody
 *               '{'
 *               TemplateStat(2)
 *                 Def(2)
 *                   'def'
 *                   FunDef(1)
 *                     FunSig
 *                       id                       <-- f
 *                       ParamClauses
 *                         ParamClause
 *                           '('
 *                           ')'
 *                     ':'
 *                     Type                       <-- F
 *                     '='
 *                     Expr
 *                       Expr1(10)
 *                         PostfixExpr
 *                           InfixExpr
 *                             PrefixExpr
 *                               SimpleExpr(1)
 *                                 'new'
 *                                 ClassTemplate*
 *               '}'
 *
 * *ClassTemplate
 *   ClassParents
 *     Constr
 *       AnnotType
 *         SimpleType
 *           SimpleTypeHead
 *             StableId
 *               StableIdHead
 *                 id                               <-- F
 *       {ArgumentExprs}[0](3)
 *         BlockExpr(2)
 *           '{'
 *           Block
 *             {BlockStat semi}[0](2)
 *               Def(2)
 *                 'def'
 *                 FunDef(1)
 *                   FunSig
 *                   ':'
 *                   Type
 *                   '='
 *                   Expr
 *           '}'
 */
TEST(ScalaParser, AnonymousClass) {
  std::string filename = "Example.scala";
  std::string buffer =
    "object O {\n"
    "  def f(): F = new F {\n"
    "    def apply(): Int = 1\n"
    "  }\n"
    "}\n";

  ScalaParser parser(filename, buffer);
  parser.parse();

  auto topStat = parser.compUnit->topStatSeq->topStat;
  ASSERT_EQ(TopStat::Opt::TMPL_DEF, topStat->opt);
  ASSERT_EQ(TmplDef::Opt::CASE_OBJECT, topStat->tmplDef->opt);

  auto classTmplOpt = topStat->tmplDef->objectDef->classTmplOpt;
  ASSERT_EQ(ClassTemplateOpt::Opt::TEMPLATE_BODY, classTmplOpt->opt);

  auto tmplStat = classTmplOpt->tmplBody->tmplStat;
  ASSERT_EQ(TemplateStat::Opt::DEF, tmplStat->opt);
}
