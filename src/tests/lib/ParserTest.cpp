#include <iostream>
#include "Diagnosis.h"
#include "EmacsOutput.h"
#include "Parser.h"
#include "SymbolTable.h"
#include "gtest/gtest.h"
using namespace djp;

// -----------------------------------------------------------------------------
// @Ann({"1", "2"}) class A {}
// -----------------------------------------------------------------------------
// ClassOrInterfaceDeclaration
//   Modifier
//     Annotation
//     '@'
//     QualifiedIdentifier <-- 'Ann'
//     '('
//     AnnotationElement
//       ElementValue
//         ElementValueArrayInitializer
//           '{'
//           ElementValues
//             ElementValue
//               Expression1
//                 Expression2
//                   Expression3
//                     Primary
//                       Literal
//                         StringLiteral <-- "1"
//           '}'
//     ')'
//   ClassDeclaration
TEST(Parser, AnnotationElementValueArray) {
  std::string filename = "Test.java";
  std::string buffer = "@Ann({\"1\", \"2\"}) class A {}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spClassOrInterfaceDeclaration decl
    = parser.compilationUnit->typeDecls[0]->decl;
  spAnnotationElement elem = decl->modifier->annotations[0]->elem;
  ASSERT_EQ(AnnotationElement::OPT_ELEMENT_VALUE, elem->opt);
  ASSERT_EQ(ElementValue::OPT_ELEMENT_VALUE_ARRAY_INITIALIZER, elem->value->opt);
  ASSERT_EQ(5, elem->value->elemValArrayInit->posLCBrace);
  ASSERT_EQ(14, elem->value->elemValArrayInit->posRCBrace);
}

TEST(Parser, AnnotationElementValuePairs) {
  std::string filename = "Test.java";
  std::string buffer =
    "@myinterface(id=10, group=20L)\n"
    "package com.test;";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  std::vector<spElementValuePair> pairs = parser.compilationUnit->pkgDecl
    ->annotations[0]->elem->pairs;

  ASSERT_EQ(2, pairs.size());

  // Pair 1
  spElementValuePair pair1 = pairs[0];
  spExpression3 expr3Pair1 = pair1->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair1 = expr3Pair1->primary->literal->intLiteral;

  ASSERT_EQ(13, pair1->id->pos);
  ASSERT_EQ("id", pair1->id->value);
  ASSERT_EQ(ElementValue::OPT_EXPRESSION1, pair1->value->opt);
  ASSERT_EQ(Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP, expr3Pair1->opt);
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair1->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair1->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_DECIMAL, intLiteralPair1->opt);
  ASSERT_EQ(16, intLiteralPair1->pos);
  ASSERT_EQ("10", intLiteralPair1->value);
  ASSERT_FALSE(intLiteralPair1->intSuffix);

  // Pair 2
  spElementValuePair pair2 = pairs[1];
  spExpression3 expr3Pair2 = pair2->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair2 = expr3Pair2->primary->literal->intLiteral;

  ASSERT_EQ(20, pair2->id->pos);
  ASSERT_EQ("group", pair2->id->value);
  ASSERT_EQ(ElementValue::OPT_EXPRESSION1, pair2->value->opt);
  ASSERT_EQ(Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP, expr3Pair2->opt);
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair2->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair2->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_DECIMAL, intLiteralPair2->opt);
  ASSERT_EQ(26, intLiteralPair2->pos);
  ASSERT_EQ("20L", intLiteralPair2->value);
  ASSERT_TRUE(intLiteralPair2->intSuffix);
}

TEST(Parser, AnnotationElementValuePairsBoolean) {
  std::string filename = "Test.java";
  std::string buffer =
    "@myinterface(v1=true, v2=false)"
    "package com.test;";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  std::vector<spElementValuePair> pairs = parser.compilationUnit->pkgDecl
    ->annotations[0]->elem->pairs;

  // Pair 1
  spElementValuePair pair1 = pairs[0];
  spExpression3 expr3Pair1 = pair1->value->expr1->expr2->expr3;
  spBooleanLiteral boolLiteralPair1 = expr3Pair1->primary->literal->boolLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair1->primary->opt);
  ASSERT_EQ(Literal::OPT_BOOLEAN, expr3Pair1->primary->literal->opt);
  ASSERT_EQ(16, boolLiteralPair1->pos);
  ASSERT_TRUE(boolLiteralPair1->val);

  // Pair 2
  spElementValuePair pair2 = pairs[1];
  spExpression3 expr3Pair2 = pair2->value->expr1->expr2->expr3;
  spBooleanLiteral boolLiteralPair2 = expr3Pair2->primary->literal->boolLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair2->primary->opt);
  ASSERT_EQ(Literal::OPT_BOOLEAN, expr3Pair2->primary->literal->opt);
  ASSERT_EQ(25, boolLiteralPair2->pos);
  ASSERT_FALSE(boolLiteralPair2->val);
}

TEST(Parser, AnnotationElementValuePairsCharacterLiteral) {
  std::string filename = "Test.java";
  // INFO: We escape the universal character name \uAa89
  // so it's not pre-processed by the c++ compiler.
  std::string buffer =
    "@myinterface(v1='A', v2='\034', v3='\\uAa89')"
    //               16      24      32
    "package com.test;";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  std::vector<spElementValuePair> pairs = parser.compilationUnit->pkgDecl
    ->annotations[0]->elem->pairs;

  // Pair 1
  spElementValuePair pair1 = pairs[0];
  spExpression3 expr3Pair1 = pair1->value->expr1->expr2->expr3;
  spCharacterLiteral charLiteralPair1
    = expr3Pair1->primary->literal->charLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair1->primary->opt);
  ASSERT_EQ(Literal::OPT_CHAR, expr3Pair1->primary->literal->opt);
  ASSERT_EQ(16, charLiteralPair1->pos);
  ASSERT_EQ("'A'", charLiteralPair1->val);

  // Pair 2
  spElementValuePair pair2 = pairs[1];
  spExpression3 expr3Pair2 = pair2->value->expr1->expr2->expr3;
  spCharacterLiteral charLiteralPair2
    = expr3Pair2->primary->literal->charLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair2->primary->opt);
  ASSERT_EQ(Literal::OPT_CHAR, expr3Pair2->primary->literal->opt);
  ASSERT_EQ(24, charLiteralPair2->pos);
  ASSERT_EQ("'\034'", charLiteralPair2->val);

  // Pair 3
  spElementValuePair pair3 = pairs[2];
  spExpression3 expr3Pair3 = pair3->value->expr1->expr2->expr3;
  spCharacterLiteral charLiteralPair3
    = expr3Pair3->primary->literal->charLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair3->primary->opt);
  ASSERT_EQ(Literal::OPT_CHAR, expr3Pair3->primary->literal->opt);
  ASSERT_EQ(32, charLiteralPair3->pos);
  ASSERT_EQ("'\\uAa89'", charLiteralPair3->val);
}

TEST(Parser, AnnotationElementValuePairsDecimalFloatingPointLiterals) {
  std::string filename = "Test.java";
  std::string buffer =
    "@myinterface("
    "v1=12., v2=12.34, v3=12.e34, v4=12.e-34f, v5=12.F,"
    "v6=.1, v7=.1E-23, v8=12e+34d)\n"
    "package com.test;";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  std::vector<spElementValuePair> pairs = parser.compilationUnit->pkgDecl
    ->annotations[0]->elem->pairs;

  // Pair 1 v1=12.
  spElementValuePair pair1 = pairs[0];
  spExpression3 expr3Pair1 = pair1->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair1
    = expr3Pair1->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair1->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair1->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_DECIMAL, fpLiteralPair1->opt);
  ASSERT_EQ(16, fpLiteralPair1->pos);
  ASSERT_EQ("12.", fpLiteralPair1->value);

  // Pair 2 v2=12.34
  spElementValuePair pair2 = pairs[1];
  spExpression3 expr3Pair2 = pair2->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair2
    = expr3Pair2->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair2->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair2->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_DECIMAL, fpLiteralPair2->opt);
  ASSERT_EQ(24, fpLiteralPair2->pos);
  ASSERT_EQ("12.34", fpLiteralPair2->value);

  // Pair 3 v3=12.e34
  spElementValuePair pair3 = pairs[2];
  spExpression3 expr3Pair3 = pair3->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair3
    = expr3Pair3->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair3->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair3->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_DECIMAL, fpLiteralPair3->opt);
  ASSERT_EQ(34, fpLiteralPair3->pos);
  ASSERT_EQ("12.e34", fpLiteralPair3->value);

  // Pair 4 v4=12.e-34f
  spElementValuePair pair4 = pairs[3];
  spExpression3 expr3Pair4 = pair4->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair4
    = expr3Pair4->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair4->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair4->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_DECIMAL, fpLiteralPair4->opt);
  ASSERT_EQ(45, fpLiteralPair4->pos);
  ASSERT_EQ("12.e-34f", fpLiteralPair4->value);

  // Pair 5 v5=12.F
  spElementValuePair pair5 = pairs[4];
  spExpression3 expr3Pair5 = pair5->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair5
    = expr3Pair5->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair5->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair5->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_DECIMAL, fpLiteralPair5->opt);
  ASSERT_EQ(58, fpLiteralPair5->pos);
  ASSERT_EQ("12.F", fpLiteralPair5->value);

  // Pair 6 v6=.1
  spElementValuePair pair6 = pairs[5];
  spExpression3 expr3Pair6 = pair6->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair6
    = expr3Pair6->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair6->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair6->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_DECIMAL, fpLiteralPair6->opt);
  ASSERT_EQ(66, fpLiteralPair6->pos);
  ASSERT_EQ(".1", fpLiteralPair6->value);

  // Pair 7 v7=.1E-23
  spElementValuePair pair7 = pairs[6];
  spExpression3 expr3Pair7 = pair7->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair7
    = expr3Pair7->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair7->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair7->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_DECIMAL, fpLiteralPair7->opt);
  ASSERT_EQ(73, fpLiteralPair7->pos);
  ASSERT_EQ(".1E-23", fpLiteralPair7->value);

  // Pair 8 v8=12e+34d
  spElementValuePair pair8 = pairs[7];
  spExpression3 expr3Pair8 = pair8->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair8
    = expr3Pair8->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair8->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair8->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_DECIMAL, fpLiteralPair8->opt);
  ASSERT_EQ(84, fpLiteralPair8->pos);
  ASSERT_EQ("12e+34d", fpLiteralPair8->value);
}

TEST(Parser, AnnotationElementValuePairsHexadecimalFloatingPointLiterals) {
  std::string filename = "Test.java";
  std::string buffer =
    "@myinterface("
    "v1=0x12p10, v2=0X34.P56, v3=0x.12p+12F"
    "package com.test;";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  std::vector<spElementValuePair> pairs = parser.compilationUnit->pkgDecl
    ->annotations[0]->elem->pairs;

  // Pair 1 v1=0x12p10
  spElementValuePair pair1 = pairs[0];
  spExpression3 expr3Pair1 = pair1->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair1
    = expr3Pair1->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair1->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair1->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_HEX, fpLiteralPair1->opt);
  ASSERT_EQ(16, fpLiteralPair1->pos);
  ASSERT_EQ("0x12p10", fpLiteralPair1->value);

  // Pair 2 v2=0X34.P56
  spElementValuePair pair2 = pairs[1];
  spExpression3 expr3Pair2 = pair2->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair2
    = expr3Pair2->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair2->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair2->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_HEX, fpLiteralPair2->opt);
  ASSERT_EQ(28, fpLiteralPair2->pos);
  ASSERT_EQ("0X34.P56", fpLiteralPair2->value);

  // Pair 3 v3=0x.12p+12F
  spElementValuePair pair3 = pairs[2];
  spExpression3 expr3Pair3 = pair3->value->expr1->expr2->expr3;
  spFloatingPointLiteral fpLiteralPair3
    = expr3Pair3->primary->literal->fpLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair3->primary->opt);
  ASSERT_EQ(Literal::OPT_FLOATING_POINT, expr3Pair3->primary->literal->opt);
  ASSERT_EQ(FloatingPointLiteral::OPT_HEX, fpLiteralPair3->opt);
  ASSERT_EQ(41, fpLiteralPair3->pos);
  ASSERT_EQ("0x.12p+12F", fpLiteralPair3->value);
}

TEST(Parser, AnnotationElementValuePairsIntegerLiterals) {
  std::string filename = "Test.java";
  std::string buffer =
    "@myinterface("
    "v1=0b01,v2=0B1_1L,v3=10,v4=2_0L,v5=0xA0,v6=0XF_0L,v7=001,v8=0_76L)\n"
    "package com.test;";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  std::vector<spElementValuePair> pairs = parser.compilationUnit->pkgDecl
    ->annotations[0]->elem->pairs;

  // Pair 1
  spElementValuePair pair1 = pairs[0];
  spExpression3 expr3Pair1 = pair1->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair1 = expr3Pair1->primary->literal->intLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair1->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair1->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_BINARY, intLiteralPair1->opt);
  ASSERT_EQ(16, intLiteralPair1->pos);
  ASSERT_EQ("0b01", intLiteralPair1->value);
  ASSERT_FALSE(intLiteralPair1->intSuffix);

  // Pair 2
  spElementValuePair pair2 = pairs[1];
  spExpression3 expr3Pair2 = pair2->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair2 = expr3Pair2->primary->literal->intLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair2->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair2->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_BINARY, intLiteralPair2->opt);
  ASSERT_EQ(24, intLiteralPair2->pos);
  ASSERT_EQ("0B1_1L", intLiteralPair2->value);
  ASSERT_TRUE(intLiteralPair2->intSuffix);

  // Pair 3
  spElementValuePair pair3 = pairs[2];
  spExpression3 expr3Pair3 = pair3->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair3 = expr3Pair3->primary->literal->intLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair3->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair3->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_DECIMAL, intLiteralPair3->opt);
  ASSERT_EQ(34, intLiteralPair3->pos);
  ASSERT_EQ("10", intLiteralPair3->value);
  ASSERT_FALSE(intLiteralPair3->intSuffix);

  // Pair 4
  spElementValuePair pair4 = pairs[3];
  spExpression3 expr3Pair4 = pair4->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair4 = expr3Pair4->primary->literal->intLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair4->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair4->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_DECIMAL, intLiteralPair4->opt);
  ASSERT_EQ(40, intLiteralPair4->pos);
  ASSERT_EQ("2_0L", intLiteralPair4->value);
  ASSERT_TRUE(intLiteralPair4->intSuffix);

  // Pair 5
  spElementValuePair pair5 = pairs[4];
  spExpression3 expr3Pair5 = pair5->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair5 = expr3Pair5->primary->literal->intLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair5->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair5->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_HEX, intLiteralPair5->opt);
  ASSERT_EQ(48, intLiteralPair5->pos);
  ASSERT_EQ("0xA0", intLiteralPair5->value);
  ASSERT_FALSE(intLiteralPair5->intSuffix);

  // Pair 6
  spElementValuePair pair6 = pairs[5];
  spExpression3 expr3Pair6 = pair6->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair6 = expr3Pair6->primary->literal->intLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair6->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair6->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_HEX, intLiteralPair6->opt);
  ASSERT_EQ(56, intLiteralPair6->pos);
  ASSERT_EQ("0XF_0L", intLiteralPair6->value);
  ASSERT_TRUE(intLiteralPair6->intSuffix);

  // Pair 7
  spElementValuePair pair7 = pairs[6];
  spExpression3 expr3Pair7 = pair7->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair7 = expr3Pair7->primary->literal->intLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair7->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair7->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_OCTAL, intLiteralPair7->opt);
  ASSERT_EQ(66, intLiteralPair7->pos);
  ASSERT_EQ("001", intLiteralPair7->value);
  ASSERT_FALSE(intLiteralPair7->intSuffix);

  // Pair 8
  spElementValuePair pair8 = pairs[7];
  spExpression3 expr3Pair8 = pair8->value->expr1->expr2->expr3;
  spIntegerLiteral intLiteralPair8 = expr3Pair8->primary->literal->intLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair8->primary->opt);
  ASSERT_EQ(Literal::OPT_INTEGER, expr3Pair8->primary->literal->opt);
  ASSERT_EQ(IntegerLiteral::OPT_OCTAL, intLiteralPair8->opt);
  ASSERT_EQ(73, intLiteralPair8->pos);
  ASSERT_EQ("0_76L", intLiteralPair8->value);
  ASSERT_TRUE(intLiteralPair8->intSuffix);
}

TEST(Parser, AnnotationElementValuePairsNull) {
  std::string filename = "Test.java";
  std::string buffer =
    "@myinterface(v1=null)"
    "package com.test;";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  std::vector<spElementValuePair> pairs = parser.compilationUnit->pkgDecl
    ->annotations[0]->elem->pairs;

  // Pair
  spElementValuePair pair = pairs[0];
  spExpression3 expr3Pair = pair->value->expr1->expr2->expr3;
  spNullLiteral nullLiteralPair = expr3Pair->primary->literal->nullLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair->primary->opt);
  ASSERT_EQ(Literal::OPT_NULL, expr3Pair->primary->literal->opt);
  ASSERT_EQ(16, nullLiteralPair->pos);
  ASSERT_EQ(TOK_NULL_LITERAL, nullLiteralPair->type);
}

TEST(Parser, AnnotationElementValuePairsStringLiteral) {
  std::string filename = "Test.java";
  // INFO: We escape the universal character name \uAa89
  // so it's not pre-processed by the c++ compiler.
  std::string buffer =
    "@myinterface(v1=\"Hello, I'm a String!\")"
    "package com.test;";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  std::vector<spElementValuePair> pairs = parser.compilationUnit->pkgDecl
    ->annotations[0]->elem->pairs;

  // Pair
  spElementValuePair pair1 = pairs[0];
  spExpression3 expr3Pair1 = pair1->value->expr1->expr2->expr3;
  spStringLiteral strLiteralPair1
    = expr3Pair1->primary->literal->strLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair1->primary->opt);
  ASSERT_EQ(Literal::OPT_STRING, expr3Pair1->primary->literal->opt);
  ASSERT_EQ(16, strLiteralPair1->pos);
  ASSERT_EQ("\"Hello, I'm a String!\"", strLiteralPair1->val);
}

// -----------------------------------------------------------------------------
// class A { void m() { String[] a = new String[0]; }}
// -----------------------------------------------------------------------------
// BlockStatement(1)
//   LocalVariableDeclarationStatement
//     Type
//       ReferenceType <--  'String'
//       '[]'
//     VariableDeclarators
//       VariableDeclarator
//         Identifier <-- 'a'
//         VariableDeclaratorRest
//           '='
//           VariableInitializer(2)
//             Expression
//               Expression1
//                 Expression2
//                   Expression3(3)
//                     Primary(5)
//                       'new'
//                       Creator(2)
//                         CreatedName
//                           Identifier <-- 'String'
//                         ArrayCreatorRest
//                           '['
//                           Expression
//                             Expression1
//                               Expression2
//                                 Expression3
//                                   Primary
//                                     Literal <-- '0'
//                           ']'
//     ';'
TEST(Parser, ArrayCreatorRest) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() { String[] a = new String[0]; }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spBlockStatement blockStmt = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest
    ->block->blockStmts[0];
  ASSERT_EQ(BlockStatement::OPT_LOCAL_VAR, blockStmt->opt);

  ASSERT_EQ(47, blockStmt->localVar->posSemiColon);

  spVariableDeclaratorRest varDeclRest = blockStmt->localVar->varDecls->varDecl
    ->varDeclRest;
  ASSERT_EQ(32, varDeclRest->posEquals);

  ASSERT_EQ(VariableInitializer::OPT_EXPRESSION, varDeclRest->varInit->opt);
  spPrimary primary = varDeclRest->varInit->expr->expr1->expr2->expr3->primary;
  ASSERT_EQ(Primary::OPT_NEW_CREATOR, primary->opt);

  ASSERT_EQ(Creator::OPT_CREATED_NAME, primary->newCreator->creator->opt);
  spArrayCreatorRest arrayCreatorRest = primary->newCreator->creator->opt2
    ->arrayCreatorRest;
  ASSERT_EQ(ArrayCreatorRest::OPT_EXPRESSION, arrayCreatorRest->opt);

  ASSERT_EQ(44, arrayCreatorRest->opt2->exprInBrackets->posLBracket);
  ASSERT_EQ(46, arrayCreatorRest->opt2->exprInBrackets->posRBracket);
}

TEST(Parser, Block) {
  std::string filename = "Test.java";
  std::string buffer =
    "class A { public String hello() {"
    "logger.debug(\"message\", p1);"
    "}";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spBlock block = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->methodOrFieldDecl->methodOrFieldRest->methodDeclRest->block;
  ASSERT_EQ(32, block->posLCBracket);
  ASSERT_EQ(61, block->posRCBracket);
  ASSERT_EQ(1, block->blockStmts.size());

  spBlockStatement blockStmt1 = block->blockStmts[0];
  ASSERT_EQ(BlockStatement::OPT_ID_STMT, blockStmt1->opt);
  ASSERT_EQ(Statement::OPT_STMT_EXPR, blockStmt1->stmt->opt);
  ASSERT_EQ(60, blockStmt1->stmt->posSemiColon);
  ASSERT_EQ(Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP,
    blockStmt1->stmt->stmtExpr->expr->expr1->expr2->expr3->opt);
  ASSERT_EQ(Primary::OPT_IDENTIFIER,
    blockStmt1->stmt->stmtExpr->expr->expr1->expr2->expr3->primary->opt);
}

TEST(Parser, ClassConstructor) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc() {} }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spClassBodyDeclaration classBodyDecl = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0];
  ASSERT_EQ(ClassBodyDeclaration::OPT_MODIFIER_MEMBER_DECL,
    classBodyDecl->opt);
  ASSERT_EQ(MemberDecl::OPT_IDENTIFIER_CONSTRUCTOR_DECLARATOR_REST,
    classBodyDecl->memberDecl->opt);
  ASSERT_EQ(12, classBodyDecl->memberDecl->id->pos);
  ASSERT_EQ("Abc", classBodyDecl->memberDecl->id->value);
}

TEST(Parser, ClassConstructorAnnotationParameter) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc(@myannotation int a) {} }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spFormalParameterDecls formParamDecls = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->constDeclRest->formParams->formParamDecls;
  ASSERT_EQ(1, formParamDecls->varModifier->annotations.size());
  ASSERT_EQ(16, formParamDecls->varModifier->annotations[0]->posTokAt);
}

TEST(Parser, ClassConstructorParameter) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc(int a) {} }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spFormalParameterDecls formParamDecls = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->constDeclRest->formParams->formParamDecls;
  ASSERT_EQ(Type::OPT_BASIC_TYPE, formParamDecls->type->opt);
  ASSERT_EQ(16, formParamDecls->type->basicType->token->pos);
  ASSERT_EQ(TOK_KEY_INT, formParamDecls->type->basicType->token->type);
  ASSERT_EQ(20, formParamDecls->formParamDeclsRest->varDeclId->identifier->pos);
  ASSERT_EQ("a",
    formParamDecls->formParamDeclsRest->varDeclId->identifier->value);
}

TEST(Parser, ClassConstructorParameterArray) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc(int[] a) {} }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spFormalParameterDecls formParamDecls = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->constDeclRest->formParams->formParamDecls;
  ASSERT_EQ(1, formParamDecls->type->arrayDepth.size());
  ASSERT_EQ(0, formParamDecls->formParamDeclsRest->varDeclId->arrayDepth.size());
}

TEST(Parser, ClassConstructorParameterEllipsis) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc(int ... a) {} }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spFormalParameterDeclsRest formParamDeclsRest = parser.compilationUnit
    ->typeDecls[0]->decl->classDecl->nClassDecl->classBody->decls[0]
    ->memberDecl->constDeclRest->formParams->formParamDecls->formParamDeclsRest;
  ASSERT_EQ(FormalParameterDeclsRest::OPT_VAR_ARITY, formParamDeclsRest->opt);
  ASSERT_EQ(24, formParamDeclsRest->varDeclId->identifier->pos);
  ASSERT_EQ("a", formParamDeclsRest->varDeclId->identifier->value);
}

TEST(Parser, ClassConstructorParameters) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc(int a, double b) {} }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spFormalParameterDecls formParamDecls = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->constDeclRest->formParams->formParamDecls;

  // param 1
  ASSERT_EQ(Type::OPT_BASIC_TYPE, formParamDecls->type->opt);
  ASSERT_EQ(16, formParamDecls->type->basicType->token->pos);
  ASSERT_EQ(TOK_KEY_INT, formParamDecls->type->basicType->token->type);
  ASSERT_EQ(20, formParamDecls->formParamDeclsRest->varDeclId->identifier->pos);
  ASSERT_EQ("a",
    formParamDecls->formParamDeclsRest->varDeclId->identifier->value);

  // param 2
  spFormalParameterDecls formParamDecls2
    = formParamDecls->formParamDeclsRest->formParamDecls;
  ASSERT_EQ(Type::OPT_BASIC_TYPE, formParamDecls2->type->opt);
  ASSERT_EQ(23, formParamDecls2->type->basicType->token->pos);
  ASSERT_EQ(TOK_KEY_DOUBLE, formParamDecls2->type->basicType->token->type);
  ASSERT_EQ(30,
    formParamDecls2->formParamDeclsRest->varDeclId->identifier->pos);
  ASSERT_EQ("b",
    formParamDecls2->formParamDeclsRest->varDeclId->identifier->value);
}

TEST(Parser, ClassDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npublic class Abc extends Def { }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spClassOrInterfaceDeclaration decl
    = parser.compilationUnit->typeDecls[0]->decl;

  ASSERT_EQ(1, parser.compilationUnit->typeDecls.size());
  ASSERT_EQ(1, decl->modifier->annotations.size());
  ASSERT_EQ(1, decl->modifier->tokens.size());
  ASSERT_EQ(13, decl->modifier->tokens[0]->pos);
  ASSERT_EQ(TOK_KEY_PUBLIC, decl->modifier->tokens[0]->type);
  ASSERT_EQ(TOK_KEY_CLASS, decl->classDecl->nClassDecl->classTok->type);
  ASSERT_EQ(20, decl->classDecl->nClassDecl->classTok->pos);
  ASSERT_EQ(26, decl->classDecl->nClassDecl->identifier->pos);
  ASSERT_EQ("Abc", decl->classDecl->nClassDecl->identifier->value);
  ASSERT_EQ(30, decl->classDecl->nClassDecl->extendsTok->pos);
  ASSERT_EQ(Type::OPT_REFERENCE_TYPE, decl->classDecl->nClassDecl->type->opt);
  ASSERT_EQ(38, decl->classDecl->nClassDecl->type->refType->id->pos);
  ASSERT_EQ("Def", decl->classDecl->nClassDecl->type->refType->id->value);
}

// -----------------------------------------------------------------------------
// class A<T,U> {}
// -----------------------------------------------------------------------------
// NormalClassDeclaration
//   'class'
//   Identifier <-- 'A'
//   TypeParameters
//     '<'
//     TypeParameter
//       Identifier <-- 'T'
//       ','
//       TypeParameter
//         Identifier <-- 'U'
//     '>'
//   ClassBody
TEST(Parser, ClassTypeParameters) {
  std::string filename = "Test.java";
  std::string buffer = "class A<T,U> {}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spNormalClassDeclaration nClassDecl = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl;

  ASSERT_EQ(7, nClassDecl->typeParams->posLt);
  ASSERT_EQ(11, nClassDecl->typeParams->posGt);
}

TEST(Parser, Comments) {
  std::string filename = "Test.java";
  std::string buffer =
    "import org.test; // a single line comment\n"
    "/** a class comment */\n"
    "class A {}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  ASSERT_EQ(2, parser.comments.size());
  ASSERT_EQ(Comment::OPT_ONE_LINE, parser.comments[0]->opt);
  ASSERT_EQ(17, parser.comments[0]->posIni);
  ASSERT_EQ(41, parser.comments[0]->posEnd);
  ASSERT_EQ(Comment::OPT_MULTIPLE_LINES, parser.comments[1]->opt);
  ASSERT_EQ(42, parser.comments[1]->posIni);
  ASSERT_EQ(63, parser.comments[1]->posEnd);
}

TEST(Parser, Errors) {
  std::string filename = "Test.java";
  std::string buffer = "@";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  ASSERT_EQ(1, diag->errors.size());
}

// -----------------------------------------------------------------------------
// public class A { public void m() { M.p("a" + b + "c"); }}
// -----------------------------------------------------------------------------
// BlockStatement(3)
//   Statement(4)
//     StatementExpression
//       Expression
//         Expression1
//           Expression2
//             Expression3(4)
//               Primary(7)
//                 Identifier <-- 'M'
//                 { . Identifier } <-- 'p'
//                 IdentifierSuffix
//                   Arguments*
//     ';'
//
// Arguments*
//   '('
//   Expression
//     Expression1
//       Expression2
//         Expression3(4)
//           Primary(1)
//             Literal
//               StringLiteral <-- "a"
//         Expression2Rest
//           InfixOp <-- '+'
//           Expression3(4)
//             Primary(7)
//               Identifier <-- b
//           InfixOp <-- '+'
//           Expression3(4)
//             Primary(1)
//               Literal <-- "c"
//   ')'
TEST(Parser, ExpressionInfixOp) {
  std::string filename = "Test.java";
  std::string buffer
    = "public class A { public void m() { M.p(\"a\" + b + \"c\"); }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spStatement stmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt;
  ASSERT_EQ(53, stmt->posSemiColon);

  spArguments args = stmt->stmtExpr->expr->expr1->expr2->expr3
    ->primary->primaryId->idSuffix->args;
  ASSERT_EQ(38, args->posLParen);
  ASSERT_EQ(52, args->posRParen);

  spExpression2Rest expr2Rest = args->expr->expr1->expr2->expr2Rest;
  ASSERT_EQ(Expression2RestHelper::OPT_INFIXOP_EXPR3, expr2Rest->pairs[0]->opt);
  ASSERT_EQ(43, expr2Rest->pairs[0]->tokInfixOp->pos);
  ASSERT_EQ(47, expr2Rest->pairs[1]->tokInfixOp->pos);
}

TEST(Parser, Expression2Rest) {
  std::string filename = "Test.java";
  std::string buffer = "class C { void m() { if (x == null) { return; }}}";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spBlockStatement blockStmt = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->voidMethDeclRest->block->blockStmts[0];

  ASSERT_EQ(BlockStatement::OPT_ID_STMT, blockStmt->opt);
  ASSERT_EQ(Statement::OPT_IF, blockStmt->stmt->opt);

  spExpression2Rest expr2Rest
    = blockStmt->stmt->parExpr->expr->expr1->expr2->expr2Rest;

  ASSERT_EQ(Expression2RestHelper::OPT_INFIXOP_EXPR3, expr2Rest->pairs[0]->opt);
  ASSERT_EQ(27, expr2Rest->pairs[0]->tokInfixOp->pos);
  ASSERT_EQ(TOK_OP_EQUALS_EQUALS, expr2Rest->pairs[0]->tokInfixOp->type);

  spExpression3 expr3 = expr2Rest->pairs[0]->expr3;
  ASSERT_EQ(Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP, expr3->opt);
  ASSERT_EQ(Literal::OPT_NULL, expr3->primary->literal->opt);
  ASSERT_EQ(30, expr3->primary->literal->nullLiteral->pos);
  ASSERT_EQ(TOK_NULL_LITERAL, expr3->primary->literal->nullLiteral->type);
}

// -----------------------------------------------------------------------------
// class A { void m() { u = (U) e.get(); }}
// -----------------------------------------------------------------------------
// Block
//   '{'
//   BlockStatement(3)
//     Statement(4)
//       StatementExpression
//         Expression*
//       ';'
//   '}'
// 
// Expression*
//   Expression1
//     Expression2
//       Expression3
//         Primary(7)
//           Identifier <-- 'u'
//     AssignmentOperator '='
//     Expression1
//       Expression2
//         Expression3(2)
//           '('
//           Type <-- 'U'
//           Expression3(4)
//             Primary(7)
//               Identifier { . Identifier } <-- 'u.get'
//               [IdentifierSuffix](3)
//                 Arguments <-- '(' ')'
//           ')'
TEST(Parser, Expression3Opt2) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() { u = (U) e.get(); }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spStatement stmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt;

  ASSERT_EQ(Statement::OPT_STMT_EXPR, stmt->opt);
  ASSERT_EQ(36, stmt->posSemiColon);

  // '='
  ASSERT_EQ(23, stmt->stmtExpr->expr->assignOp->tok->pos);

  // '(U)'
  spExpression3 expr3 = stmt->stmtExpr->expr->assignExpr1->expr2->expr3;
  ASSERT_EQ(Expression3::OPT_TYPE_EXPRESSION3, expr3->opt);
  ASSERT_EQ(25, expr3->opt2->posLParen);
  ASSERT_EQ(27, expr3->opt2->posRParen);

  // 'u.get()'
  ASSERT_EQ(Primary::OPT_IDENTIFIER, expr3->opt2->expr3->primary->opt);
  ASSERT_EQ(2, expr3->opt2->expr3->primary->primaryId->ids.size());
  ASSERT_EQ(34,
    expr3->opt2->expr3->primary->primaryId->idSuffix->args->posLParen);
  ASSERT_EQ(35,
    expr3->opt2->expr3->primary->primaryId->idSuffix->args->posRParen);
}

// -----------------------------------------------------------------------------
// class C { C c = new C() { {} }; }
// -----------------------------------------------------------------------------
// MethodOrFieldDecl
//   Type <-- 'C'
//   Identifier <-- 'c'
//   MethodOrFieldRest(1)
//     FieldDeclaratorsRest
//       VariableDeclaratorRest
//         '='
//         VariableInitializer(2)
//           Expression*
//     ';'
//
// Expression*
//   Expression1
//     Expression2
//       Expression3
//         Primary(5)
//           'new'
//           Creator**
//
// Creator(2)
//   CreatedName
//     Identifier <-- C
//   ClassCreatorRest
//     Arguments <-- '()'
//     ClassBody
//       '{'
//       ClassBodyDeclaration(3)
//         Block
//           '{'
//             BlockStatements <-- empty!
//           '}'
//       '}'
TEST(Parser, FieldDeclaratorWithClassBody) {
  std::string filename = "Test.java";
  std::string buffer = "class C { C c = new C() { {} }; }";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spMethodOrFieldRest methodOrFieldRest = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->methodOrFieldDecl->methodOrFieldRest;
  ASSERT_EQ(MethodOrFieldRest::OPT_FIELD, methodOrFieldRest->opt);
  ASSERT_EQ(30, methodOrFieldRest->posSemiColon);
  ASSERT_EQ(14, methodOrFieldRest->fieldDeclsRest->varDeclRest->posEquals);
  ASSERT_EQ(VariableInitializer::OPT_EXPRESSION,
    methodOrFieldRest->fieldDeclsRest->varDeclRest->varInit->opt);

  spPrimary primary = methodOrFieldRest->fieldDeclsRest->varDeclRest->varInit
    ->expr->expr1->expr2->expr3->primary;
  ASSERT_EQ(Primary::OPT_NEW_CREATOR, primary->opt);
  ASSERT_EQ(Creator::OPT_CREATED_NAME, primary->newCreator->creator->opt);
  spClassCreatorRest classCreatorRest
    = primary->newCreator->creator->opt2->classCreatorRest;

  ASSERT_EQ(21, classCreatorRest->args->posLParen);
  ASSERT_EQ(22, classCreatorRest->args->posRParen);
  ASSERT_EQ(24, classCreatorRest->classBody->posLCBrace);
  ASSERT_EQ(29, classCreatorRest->classBody->posRCBrace);

  ASSERT_EQ(ClassBodyDeclaration::OPT_STATIC_BLOCK,
    classCreatorRest->classBody->decls[0]->opt);
  ASSERT_EQ(26, classCreatorRest->classBody->decls[0]->block->posLCBracket);
  ASSERT_EQ(27, classCreatorRest->classBody->decls[0]->block->posRCBracket);
}

// -----------------------------------------------------------------------------
// class A { void m() { for (int i = 0; i < max; i++) { ; }}}
// -----------------------------------------------------------------------------
// BlockStatement(3)
//   Statement(10)
//     'for'
//     '('
//     ForControl(1)
//       ForVarControl
//         Type <-- 'int'
//         VariableDeclaratorId
//           Identifier <-- 'i'
//         ForVarControlRest(1)
//           ForVariableDeclaratorsRest
//             '='
//             VariableInitializer(2)
//               Expression <-- '0'
//           ';'
//           Expression* <-- 'i < max'
//           ';'
//           ForUpdate
//             StatementExpression
//               Expression** <-- 'i++'
//     ')'
//     Statement(1)
//       Block
//         '{'
//         BlockStattements
//           BlockStatement
//             Statement(2)
//               ';'
//         '}'
//
// Expression*
//   Expression1
//     Expression2
//       Expression3
//         Primary
//           Identifier <-- 'i'
//     Expression2Rest
//       InfixOp <-- '<'
//       Expression3 <-- 'max'
//
// Expression**
//   Expression1
//     Expression2
//       Expression3
//         Primary <-- 'i'
//         PostfixOp <-- '++'
TEST(Parser, For) {
  std::string filename = "Test.java";
  std::string buffer
    = "class A { void m() { for (int i = 0; i < max; i++) { ; }}}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spStatement stmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt;
  ASSERT_EQ(Statement::OPT_FOR, stmt->opt);
  ASSERT_EQ(25, stmt->posLParen);
  ASSERT_EQ(49, stmt->posRParen);

  ASSERT_EQ(ForControl::OPT_FOR_VAR_CTRL, stmt->forCtrl->opt);
  spForVarControlRest forVarCtrlRest = stmt->forCtrl->varCtrl->forVarCtrlRest;
  ASSERT_EQ(ForVarControlRest::OPT_FOR_VAR_DECLS_REST, forVarCtrlRest->opt);
  ASSERT_EQ(35, forVarCtrlRest->posSemiColon1);
  ASSERT_EQ(44, forVarCtrlRest->posSemiColon2);
}

// -----------------------------------------------------------------------------
// class A { void m() { for (int i = 0; (i + 1) < max; i++) {} }}
// -----------------------------------------------------------------------------
// BlockStatement
//   Statement
//     'for'
//     '('
//     ForControl
//       ForVarControl (opt 1)
//         Type <-- 'int'
//         VariableDeclaratorId
//           Identifier <-- 'i'
//         ForVarControlRest
//           ForVariableDeclaratorsRest
//             '='
//             VariableInitializer
//               Expression <-- '0'
//           ';'
//           Expression
//             Expression1
//               Expression2
//                 Expression3
//                   '('
//                   Expression
//                     Expression1
//                       Expression2
//                         Expression3
//                           Primary <-- 'i'
//                         Expression2Rest <-- '+ 1'
//                   ')'
//                   Expression3 <-- '< max'

//                 Expression2Rest
//                   InfixOp <-- '<'
//                   Expression3 <-- 'max'
//           ';'
//           ForUpdate
//     ')'
//     Statement
TEST(Parser, ForExpr) {
  std::string filename = "Test.java";
  std::string buffer
    = "class A { void m() { for (int i = 0; (i + 1) < max; i++) {} }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spStatement stmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt;

  ASSERT_EQ(25, stmt->posLParen);
  ASSERT_EQ(55, stmt->posRParen);

  ASSERT_EQ(Statement::OPT_FOR, stmt->opt);
  ASSERT_EQ(ForControl::OPT_FOR_VAR_CTRL, stmt->forCtrl->opt);
  ASSERT_EQ(ForVarControlRest::OPT_FOR_VAR_DECLS_REST,
    stmt->forCtrl->varCtrl->forVarCtrlRest->opt);

  ASSERT_EQ(50, stmt->forCtrl->varCtrl->forVarCtrlRest->posSemiColon2);
  ASSERT_EQ(0, diag->errors.size());
}

// -----------------------------------------------------------------------------
// class A { private <E> E m(List<T> l) { return l.get(0); }}
// -----------------------------------------------------------------------------
// ClassBodyDeclaration(3)
//   Modifier <-- 'private'
//   MemberDecl(4)
//     GenericMethodOrConstructorDecl
//       TypeParameters
//         '<'
//         TypeParameter <-- 'E'
//         '>'
//       GenericMethodOrConstructorRest(1)
//         Type <-- 'E'
//         Identifier <-- 'm'
//         MethodDeclaratorRest
//         FormalParameters
//           '('
//           FormalParameterDecls
//             Type <-- 'List<T>'
//             FormalParameterDeclsRest
//               VariableDeclaratorId
//                 Identifier
//           ')'
//         Block
//           '{'
//           BlockStatements
//           '}'
TEST(Parser, GenericMethod) {
  std::string filename = "Test.java";
  std::string buffer
    = "class A { private <E> E m(List<T> l) { return l.get(0); }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spMemberDecl memberDecl = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0]->memberDecl;
  ASSERT_EQ(MemberDecl::OPT_GENERIC_METHOD_OR_CONSTRUCTOR_DECL,
    memberDecl->opt);

  // <E>
  ASSERT_EQ(18, memberDecl->genMethodOrConstDecl->typeParams->posLt);
  ASSERT_EQ(20, memberDecl->genMethodOrConstDecl->typeParams->posGt);

  // E m
  ASSERT_EQ(GenericMethodOrConstructorRest::OPT_TYPE_IDENTIFIER,
    memberDecl->genMethodOrConstDecl->rest->opt);
  ASSERT_EQ(Type::OPT_REFERENCE_TYPE,
    memberDecl->genMethodOrConstDecl->rest->type->opt);
  ASSERT_EQ("m", memberDecl->genMethodOrConstDecl->rest->id->value);
}

// -----------------------------------------------------------------------------
// u = m.r(j, new A<B<T>>() {});
// -----------------------------------------------------------------------------
// Block
//   '{'
//   BlockStatement(3)
//     Statement(4)
//       StatementExpression
//         Expression*
//       ';'
//   '}'
//
// Expression1*
//   Expression2
//     Expression3(3)
//       Primary(7)
//         Identifier <-- 'u'
// AssignmentOperator <-- '='
// Expression1
//   Expression2
//     Expression3(3)
//       Primary(7)
//         Identifier { . Identifier } <-- m.r
//         IdentifierSuffix
//           Arguments
//             '('
//             Expression <-- 'j' *
//             ','
//             Expression
//               Expression1
//                 Expression2
//                  Expression3
//                    Primary(5)
//                      'new'
//                      Creator**
//             ')'
//
// Expression*
//   Expression2
//     Expression3(3)
//       Primary(7)
//
// Creator**(2)
//   CreatedName
//     Identifier <-- 'A'
//     TypeArgumentsOrDiamond(2)
//       TypeArguments
//         '<'
//         TypeArgument(1)
//           ReferenceType
//             Identifier <-- 'B'
//             TypeArguments
//               '<'
//               TypeArgument(1)
//                 ReferenceType
//                   Identifier <-- 'T'
//               '>'
//         '>'
//   ClassCreatorRest
//     Arguments '()'
//     ClassBody '{}'
TEST(Parser, Generics) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() { u = m.r(j, new T<L<G>>() {}); }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spExpression expr = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt->stmtExpr->expr;
  ASSERT_EQ(23, expr->assignOp->tok->pos);

  spArguments args = expr->assignExpr1->expr2->expr3->primary->primaryId
    ->idSuffix->args;
  ASSERT_EQ(28, args->posLParen);
  ASSERT_EQ(48, args->posRParen);
  ASSERT_EQ(30, args->exprs[0].first);

  spCreator creator = args->exprs[0].second->expr1->expr2->expr3->primary
    ->newCreator->creator;
  spTypeArguments typeArgs
    = creator->opt2->createdName->typeArgsOrDiam->typeArgs;
  ASSERT_EQ(37, typeArgs->posLt);
  ASSERT_EQ(42, typeArgs->posGt);

  ASSERT_EQ(39, typeArgs->typeArg->type->refType->typeArgs->posLt);
  ASSERT_EQ(41, typeArgs->typeArg->type->refType->typeArgs->posGt);

  ASSERT_EQ(43, creator->opt2->classCreatorRest->args->posLParen);
  ASSERT_EQ(44, creator->opt2->classCreatorRest->args->posRParen);
}

// -----------------------------------------------------------------------------
// class A { void m() { p = s[i]; }}
// -----------------------------------------------------------------------------
// BlockStatement(3)
//   Statement(4)
//     StatementExpression
//       Expression
//         Expression1
//           Expression2
//             Expression3(3)
//               Primary(7)
//                 Identifier <-- 'p'
//         AssignmentOperator <-- '='
//         Expression1
//           Expression2
//             Expression3(3)
//               Primary(7)
//                 Identifier <-- 's'
//                 IdentifierSuffix(2)
//                   '['
//                   Expression
//                     Expression1
//                       Expression2
//                         Expression3(3)
//                           Primary(7)
//                             Identifier <-- 'i'
//                   ']'
//     ';'
TEST(Parser, IdentifierSuffix) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() { p = s[i]; }}";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spBlockStatement blockStmt = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest
    ->block->blockStmts[0];

  ASSERT_EQ(BlockStatement::OPT_ID_STMT, blockStmt->opt);
  ASSERT_EQ(Statement::OPT_STMT_EXPR, blockStmt->stmt->opt);
  ASSERT_EQ(29, blockStmt->stmt->posSemiColon);

  spExpression expr = blockStmt->stmt->stmtExpr->expr;
  ASSERT_EQ(23, expr->assignOp->tok->pos);

  spPrimary primary = expr->assignExpr1->expr2->expr3->primary;
  ASSERT_EQ(Primary::OPT_IDENTIFIER, primary->opt);

  spIdentifierSuffix idSuffix = primary->primaryId->idSuffix;
  ASSERT_EQ(IdentifierSuffix::OPT_ARRAY_EXPRESSION, idSuffix->opt);
  ASSERT_EQ(26, idSuffix->arrayPair.first);
  ASSERT_EQ(28, idSuffix->arrayPair.second);
}

TEST(Parser, ImportDeclarations) {
  std::string filename = "Test.java";
  std::string buffer =
    "import com.test1.Test1;\n"
    "import com.test2.*;\n"
    "import static com.test3.Test3;\n"
    "import static com.test4.*;\n";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  ASSERT_EQ(4, parser.compilationUnit->impDecls->imports.size());

  // import 1
  ASSERT_EQ(0, parser.compilationUnit->impDecls->imports[0]->posTokImport);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[0]->posTokStatic);
  ASSERT_EQ(0, parser.compilationUnit->impDecls->imports[0]->iniOnDemand);
  ASSERT_EQ(0, parser.compilationUnit->impDecls->imports[0]->endOnDemand);
  ASSERT_FALSE(parser.compilationUnit->impDecls->imports[0]->err);
  ASSERT_EQ(SINGLE_TYPE_IMPORT_DECLARATION,
    parser.compilationUnit->impDecls->imports[0]->type);

  // import 2
  ASSERT_EQ(24, parser.compilationUnit->impDecls->imports[1]->posTokImport);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[1]->posTokStatic);
  ASSERT_EQ(40, parser.compilationUnit->impDecls->imports[1]->iniOnDemand);
  ASSERT_EQ(41, parser.compilationUnit->impDecls->imports[1]->endOnDemand);
  ASSERT_FALSE(parser.compilationUnit->impDecls->imports[0]->err);
  ASSERT_EQ(TYPE_IMPORT_ON_DEMAND_DECLARATION,
    parser.compilationUnit->impDecls->imports[1]->type);

  // import 3
  ASSERT_EQ(44, parser.compilationUnit->impDecls->imports[2]->posTokImport);
  ASSERT_EQ(51, parser.compilationUnit->impDecls->imports[2]->posTokStatic);
  ASSERT_EQ(0, parser.compilationUnit->impDecls->imports[2]->iniOnDemand);
  ASSERT_EQ(0, parser.compilationUnit->impDecls->imports[2]->endOnDemand);
  ASSERT_FALSE(parser.compilationUnit->impDecls->imports[2]->err);
  ASSERT_EQ(SINGLE_STATIC_IMPORT_DECLARATION,
    parser.compilationUnit->impDecls->imports[2]->type);

  // import 4
  ASSERT_EQ(75, parser.compilationUnit->impDecls->imports[3]->posTokImport);
  ASSERT_EQ(82, parser.compilationUnit->impDecls->imports[3]->posTokStatic);
  ASSERT_EQ(98, parser.compilationUnit->impDecls->imports[3]->iniOnDemand);
  ASSERT_EQ(99, parser.compilationUnit->impDecls->imports[3]->endOnDemand);
  ASSERT_FALSE(parser.compilationUnit->impDecls->imports[3]->err);
  ASSERT_EQ(STATIC_IMPORT_ON_DEMAND_DECLARATION,
    parser.compilationUnit->impDecls->imports[3]->type);
}

// -----------------------------------------------------------------------------
// class A { class B {} }
// -----------------------------------------------------------------------------
// TypeDeclaration
//   ClassOrInterfaceDeclaration               +ST_CLASS (pop)
//     ClassDeclaration
//       NormalClassDeclaration                +ST_IDENTIFIER 'A'
//         'class'
//         Identifier <-- 'A'
//         ClassBody
//           '{'
//           ClassBodyDeclaration
//             MemberDecl                      +ST_MEMBER_DECL -> ST_CLASS (pop)
//               ClassDeclaration
//                 NormalClassDeclaration      +ST_IDENTIFIER 'B'
//           '}'
TEST(Parser, InnerClass) {
  std::string filename = "Test.java";
  std::string buffer = "class A { class B {} }";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spMemberDecl memberDecl = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0]->memberDecl;
  ASSERT_EQ(MemberDecl::OPT_CLASS_DECLARATION, memberDecl->opt);
}

// -----------------------------------------------------------------------------
// class A { void test() { Exec exe = createExec(); }}
// -----------------------------------------------------------------------------
// BlockStatement
//   LocalVariableDeclarationStatement
//     Type <-- 'Exec'
//     VariableDeclarators
//       VariableDeclarator
//         Identifier <-- 'exe'
//         VariableDeclaratorRest
//           '='
//           VariableInitializer
//             Expression
//               Expression1
//                 Expression2
//                   Expression3(3)
//                     Primary(7)
//                       Identifier <-- createExec
//                       IdentifierSuffix
//                         Arguments
//                           '('
//                           ')'
//     ';'
TEST(Parser, LocalVariableDeclarationStatement) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void test() { Exec exe = createExec(); }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spMemberDecl memberDecl = parser.compilationUnit
    ->typeDecls[0]->decl->classDecl->nClassDecl->classBody->decls[0]
    ->memberDecl;

  ASSERT_EQ(MemberDecl::OPT_VOID_IDENTIFIER_VOID_METHOD_DECLARATOR_REST,
    memberDecl->opt);

  spVariableDeclaratorRest varDeclRest = memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->localVar->varDecls->varDecl->varDeclRest;

  ASSERT_EQ(33, varDeclRest->posEquals);

  spPrimary primary = varDeclRest->varInit->expr->expr1->expr2->expr3->primary;
  ASSERT_EQ(35, primary->primaryId->ids[0]->pos);
  ASSERT_EQ("createExec",  primary->primaryId->ids[0]->value);
  ASSERT_EQ(45, primary->primaryId->idSuffix->args->posLParen);
  ASSERT_EQ(46, primary->primaryId->idSuffix->args->posRParen);
}

// -----------------------------------------------------------------------------
// class C { void m() { List<A> lst = new ArrayList<A>(); }}
// -----------------------------------------------------------------------------
// BlockStatement(1)
//   LocalVariableDeclarationStatement
//     Type
//     VariableDeclarators
//       ReferenceType
//         Identifier <-- 'List'
//           TypeArguments
//             '<'
//             Type
//               ReferenceType <-- 'A'
//             '>'
//     ';'
TEST(Parser, LocalVariableTypeArguments) {
  std::string filename = "Test.java";
  std::string buffer
    = "class C { void m() { List<A> lst = new ArrayList<A>(); }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spBlockStatement blockStmt = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest
    ->block->blockStmts[0];
  ASSERT_EQ(BlockStatement::OPT_LOCAL_VAR, blockStmt->opt);
  spLocalVariableDeclarationStatement localVar = blockStmt->localVar;
  ASSERT_EQ(Type::OPT_REFERENCE_TYPE, localVar->type->opt);
  ASSERT_EQ(21, localVar->type->refType->id->pos);
  ASSERT_EQ("List", localVar->type->refType->id->value);
  ASSERT_EQ(25, localVar->type->refType->typeArgs->posLt);
  ASSERT_EQ(27, localVar->type->refType->typeArgs->posGt);
  ASSERT_EQ(TypeArgument::OPT_TYPE,
    localVar->type->refType->typeArgs->typeArg->opt);
  ASSERT_EQ(Type::OPT_REFERENCE_TYPE,
    localVar->type->refType->typeArgs->typeArg->type->opt);
  ASSERT_EQ(26, localVar->type->refType->typeArgs->typeArg
    ->type->refType->id->pos);
  ASSERT_EQ("A", localVar->type->refType->typeArgs->typeArg
    ->type->refType->id->value);
  ASSERT_EQ(0, localVar->type->refType->typeArgs->typeArg
    ->type->arrayDepth.size());
  ASSERT_EQ(53, localVar->posSemiColon);
}

// -----------------------------------------------------------------------------
// class C { void m() { List<A[]> lst = new ArrayList<A[]>(); }}
// -----------------------------------------------------------------------------
// BlockStatement(1)
//   LocalVariableDeclarationStatement
//     Type
//     VariableDeclarators
//       ReferenceType
//         Identifier <-- 'List'
//           TypeArguments
//             '<'
//             Type
//               ReferenceType <-- 'A'
//               '[]'
//             '>'
//     ';'
TEST(Parser, LocalVariableTypeArgumentsArray) {
  std::string filename = "Test.java";
  std::string buffer
    = "class C { void m() { List<A[]> lst = new ArrayList<A>(); }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spBlockStatement blockStmt = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest
    ->block->blockStmts[0];
  ASSERT_EQ(BlockStatement::OPT_LOCAL_VAR, blockStmt->opt);
  spLocalVariableDeclarationStatement localVar = blockStmt->localVar;
  ASSERT_EQ(Type::OPT_REFERENCE_TYPE, localVar->type->opt);
  ASSERT_EQ(21, localVar->type->refType->id->pos);
  ASSERT_EQ("List", localVar->type->refType->id->value);
  ASSERT_EQ(25, localVar->type->refType->typeArgs->posLt);
  ASSERT_EQ(29, localVar->type->refType->typeArgs->posGt);
  ASSERT_EQ(TypeArgument::OPT_TYPE,
    localVar->type->refType->typeArgs->typeArg->opt);
  ASSERT_EQ(Type::OPT_REFERENCE_TYPE,
    localVar->type->refType->typeArgs->typeArg->type->opt);
  ASSERT_EQ(26, localVar->type->refType->typeArgs->typeArg
    ->type->refType->id->pos);
  ASSERT_EQ("A", localVar->type->refType->typeArgs->typeArg
    ->type->refType->id->value);
  ASSERT_EQ(1, localVar->type->refType->typeArgs->typeArg
    ->type->arrayDepth.size());
  ASSERT_EQ(55, localVar->posSemiColon);
}

// -----------------------------------------------------------------------------
// class A { int m1() { return 1; }};
// -----------------------------------------------------------------------------
// ClassBody
//   '{'
//   ClassBodyDeclaration(2)
//     MemberDecl(1)
//       MethodOrFieldDecl
//         Type <-- 'int'
//         Identifier <-- 'm1'
//         MethodOrFieldRest(2)
//           MethodDeclaratorRest
//             FormalParameters <-- '()'
//             Block <-- '{ return 1; }
//   '}'
TEST(Parser, MemberDeclOpt1) {
  std::string filename = "Test.java";
  std::string buffer = "class A { int m1() { return 1; }}";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spClassBody classBody = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody;
  ASSERT_EQ(8, classBody->posLCBrace);
  ASSERT_EQ(32, classBody->posRCBrace);

  ASSERT_EQ(ClassBodyDeclaration::OPT_MODIFIER_MEMBER_DECL,
    classBody->decls[0]->opt);
  ASSERT_EQ(MemberDecl::OPT_METHOD_OR_FIELD_DECL,
    classBody->decls[0]->memberDecl->opt);

  spMethodOrFieldRest methodOrFieldRest = classBody->decls[0]->memberDecl
    ->methodOrFieldDecl->methodOrFieldRest;
  ASSERT_EQ(MethodOrFieldRest::OPT_METHOD, methodOrFieldRest->opt);

  ASSERT_EQ(16, methodOrFieldRest->methodDeclRest->formParams->posLParen);
  ASSERT_EQ(17, methodOrFieldRest->methodDeclRest->formParams->posRParen);
}

TEST(Parser, MethodOrFieldRestOptField) {
  std::string filename = "Test.java";
  std::string buffer =
    "class A { protected static Logger l = LoggerFactory.getLogger(A.class);";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spMemberDecl memberDecl = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl;
  ASSERT_EQ(MemberDecl::OPT_METHOD_OR_FIELD_DECL, memberDecl->opt);

  // Logger l
  ASSERT_EQ(Type::OPT_REFERENCE_TYPE, memberDecl->methodOrFieldDecl->type->opt);
  ASSERT_EQ(27, memberDecl->methodOrFieldDecl->type->refType->id->pos);
  ASSERT_EQ("Logger",
    memberDecl->methodOrFieldDecl->type->refType->id->value);
  ASSERT_EQ(34, memberDecl->methodOrFieldDecl->id->pos);
  ASSERT_EQ("l", memberDecl->methodOrFieldDecl->id->value);

  // = LoggerFactory.getLogger
  ASSERT_EQ(MethodOrFieldRest::OPT_FIELD,
    memberDecl->methodOrFieldDecl->methodOrFieldRest->opt);
  ASSERT_EQ(70, memberDecl->methodOrFieldDecl->methodOrFieldRest->posSemiColon);

  spVariableDeclaratorRest varDeclRest = memberDecl->methodOrFieldDecl
    ->methodOrFieldRest->fieldDeclsRest->varDeclRest;
  ASSERT_EQ(0, varDeclRest->arrayDepth.size());
  ASSERT_EQ(36, varDeclRest->posEquals);
  ASSERT_EQ(VariableInitializer::OPT_EXPRESSION, varDeclRest->varInit->opt);
  ASSERT_EQ(Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP,
    varDeclRest->varInit->expr->expr1->expr2->expr3->opt);
  spPrimary primary = varDeclRest->varInit->expr->expr1->expr2->expr3->primary;
  ASSERT_EQ(Primary::OPT_IDENTIFIER, primary->opt);
  ASSERT_EQ(2, primary->primaryId->ids.size());
  ASSERT_EQ(IdentifierSuffix::OPT_ARGUMENTS, primary->primaryId->idSuffix->opt);

  // (A.class)
  spArguments args = primary->primaryId->idSuffix->args;
  ASSERT_EQ(61, args->posLParen);
  ASSERT_EQ(69, args->posRParen);
  ASSERT_EQ(Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP,
    args->expr->expr1->expr2->expr3->opt);
  ASSERT_EQ(Primary::OPT_IDENTIFIER,
    args->expr->expr1->expr2->expr3->primary->opt);

  spPrimaryIdentifier primaryId
    = args->expr->expr1->expr2->expr3->primary->primaryId;
  ASSERT_EQ(1, primaryId->ids.size());
  ASSERT_EQ(62,primaryId->ids[0]->pos);
  ASSERT_EQ("A", primaryId->ids[0]->value);
  ASSERT_EQ(IdentifierSuffix::OPT_PERIOD_CLASS,
    primaryId->idSuffix->opt);
  ASSERT_EQ(63, primaryId->idSuffix->posPeriod);
  ASSERT_EQ(64, primaryId->idSuffix->tokClass->pos);
  ASSERT_EQ(TOK_KEY_CLASS, primaryId->idSuffix->tokClass->type);
}

TEST(Parser, MethodOrFieldRestOptMethod) {
  std::string filename = "Test.java";
  std::string buffer =
    "abstract class A { "
    "abstract public String hello("
    "@A1 Integer year, @A2(\"name\") String name);"
    "}";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spMethodOrFieldRest methodOrFieldRest = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->methodOrFieldDecl->methodOrFieldRest;
  ASSERT_EQ(MethodOrFieldRest::OPT_METHOD, methodOrFieldRest->opt);
  ASSERT_EQ(47, methodOrFieldRest->methodDeclRest->formParams->posLParen);
  ASSERT_EQ(89, methodOrFieldRest->methodDeclRest->formParams->posRParen);
  ASSERT_EQ(Type::OPT_REFERENCE_TYPE,
    methodOrFieldRest->methodDeclRest->formParams->formParamDecls->type->opt);
  ASSERT_EQ(90, methodOrFieldRest->methodDeclRest->posSemiColon);
}

TEST(Parser, PackageDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npackage com.test;";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  ASSERT_EQ(1, parser.compilationUnit->pkgDecl->annotations.size());
  ASSERT_EQ(0, parser.compilationUnit->pkgDecl->annotations[0]->posTokAt);
  ASSERT_EQ(1,
    parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId->id->pos);
}

TEST(Parser, ParExpression) {
  std::string filename = "Test.java";
  std::string buffer
    = "class A { void h() { if (s.equals(\"x\")) { return; }}}";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spBlockStatement blockStmt = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest
    ->block->blockStmts[0];
  ASSERT_EQ(BlockStatement::OPT_ID_STMT, blockStmt->opt);
  ASSERT_EQ(Statement::OPT_IF, blockStmt->stmt->opt);
  ASSERT_EQ(21, blockStmt->stmt->tokIf->pos);
  ASSERT_EQ(TOK_KEY_IF, blockStmt->stmt->tokIf->type);
  ASSERT_EQ(24, blockStmt->stmt->parExpr->posLParen);
  ASSERT_EQ(38, blockStmt->stmt->parExpr->posRParen);

  spStringLiteral strLiteral = blockStmt->stmt->parExpr->expr->expr1->expr2
    ->expr3->primary->primaryId->idSuffix->args->expr->expr1->expr2->expr3
    ->primary->literal->strLiteral;
  ASSERT_EQ(34, strLiteral->pos);
  ASSERT_EQ("\"x\"", strLiteral->val);
}

/// Primary: new Creator
/// Creator:
///   NonWildcardTypeArguments CreatedName ClassCreatorRest
///   CreatedName ( ClassCreatorRest | ArrayCreatorRest )
/// NonWildcardTypeArguments: < TypeList >
/// TypeList: ReferenceType {, ReferenceType }
/// CreatedName:
///   Identifier [TypeArgumentsOrDiamond]
///     { . Identifier [TypeArgumentsOrDiamond] }
/// ClassCreatorRest: Arguments [ClassBody]
/// Arguments: ( [ Expression { , Expression }] )
/// ArrayCreatorRest:
///   '['
///     ( ']' { '[]' } ArrayInitializer |
///       Expression ']' { '[' Expression ']' } { '[]' } )
///
/// Non-terminals are enclosed in square brackets.
TEST(Parser, PrimaryNewCreator) {
  std::string filename = "Test.java";
  std::string buffer =
    "@myinterface("
    "k1 = new <Integer> MyClass<>(),"
    "k2 = new <String, Integer> MyClass<Long>(),"
    "k4 = new MyClass(),"
    "k5 = new MyClass[]"
    ")"
    "package com.test;";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  std::vector<spElementValuePair> pairs = parser.compilationUnit->pkgDecl
    ->annotations[0]->elem->pairs;

  // Pair 1
  // "k1 = new <Integer> MyClass<>(),"
  spCreator creator1 = pairs[0]->value->expr1->expr2->expr3->primary
    ->newCreator->creator;
  ASSERT_EQ(Creator::OPT_NON_WILDCARD_TYPE_ARGUMENTS, creator1->opt);
  ASSERT_EQ(22, creator1->opt1->nonWildcardTypeArguments->posLt);
  ASSERT_EQ(30, creator1->opt1->nonWildcardTypeArguments->posGt);
  ASSERT_EQ(23, creator1->opt1->nonWildcardTypeArguments
    ->typeList2->type->refType->id->pos);
  ASSERT_EQ("Integer", creator1->opt1->nonWildcardTypeArguments
    ->typeList2->type->refType->id->value);
  ASSERT_EQ(32, creator1->opt1->createdName->id->pos);
  ASSERT_EQ("MyClass", creator1->opt1->createdName->id->value);
  ASSERT_EQ(TypeArgumentsOrDiamond::OPT_DIAMOND,
    creator1->opt1->createdName->typeArgsOrDiam->opt);
  ASSERT_EQ(39,
    creator1->opt1->createdName->typeArgsOrDiam->posLt);
  ASSERT_EQ(40,
    creator1->opt1->createdName->typeArgsOrDiam->posGt);

  // Pair 2
  // "k2 = new <String, Integer> MyClass<Long>(),"
  spCreator creator2 = pairs[1]->value->expr1->expr2->expr3->primary
    ->newCreator->creator;
  ASSERT_EQ(Creator::OPT_NON_WILDCARD_TYPE_ARGUMENTS, creator2->opt);
  ASSERT_EQ(53, creator2->opt1->nonWildcardTypeArguments->posLt);
  ASSERT_EQ(69, creator2->opt1->nonWildcardTypeArguments->posGt);
  ASSERT_EQ(54, creator2->opt1->nonWildcardTypeArguments
    ->typeList2->type->refType->id->pos);
  ASSERT_EQ("String", creator2->opt1->nonWildcardTypeArguments
    ->typeList2->type->refType->id->value);
  std::pair<unsigned int, spType> pair = creator2->opt1
    ->nonWildcardTypeArguments->typeList2->pairs[0];
  ASSERT_EQ(62, pair.second->refType->id->pos);
  ASSERT_EQ("Integer", pair.second->refType->id->value);
  ASSERT_EQ(71, creator2->opt1->createdName->id->pos);
  ASSERT_EQ("MyClass", creator2->opt1->createdName->id->value);
  ASSERT_EQ(TypeArgumentsOrDiamond::OPT_TYPE_ARGUMENTS,
    creator2->opt1->createdName->typeArgsOrDiam->opt);
  ASSERT_EQ(78,
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->posLt);
  ASSERT_EQ(83,
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->posGt);
  ASSERT_EQ(TypeArgument::OPT_TYPE,
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->typeArg->opt);
  ASSERT_EQ(79,
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->typeArg
    ->type->refType->id->pos);
  ASSERT_EQ("Long",
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->typeArg
    ->type->refType->id->value);

  // Pair 3
  // "k4 = new MyClass()"
  spCreator creator3 = pairs[2]->value->expr1->expr2->expr3->primary
    ->newCreator->creator;
  ASSERT_EQ(Creator::OPT_CREATED_NAME, creator3->opt);
  ASSERT_EQ(96, creator3->opt2->createdName->id->pos);
  ASSERT_EQ("MyClass", creator3->opt2->createdName->id->value);
  ASSERT_EQ(103, creator3->opt2->classCreatorRest->args->posLParen);
  ASSERT_EQ(104, creator3->opt2->classCreatorRest->args->posRParen);

  // Pair 4
  // k5 = new MyClass[]
  spCreator creator4 = pairs[3]->value->expr1->expr2->expr3->primary
    ->newCreator->creator;
  ASSERT_EQ(Creator::OPT_CREATED_NAME, creator4->opt);
  ASSERT_EQ(115, creator4->opt2->createdName->id->pos);
  ASSERT_EQ("MyClass", creator4->opt2->createdName->id->value);
  ASSERT_EQ(ArrayCreatorRest::OPT_ARRAY_INITIALIZER,
    creator4->opt2->arrayCreatorRest->opt);
  ASSERT_EQ(1, creator4->opt2->arrayCreatorRest->opt1->arrayDepth.size());
}

// -----------------------------------------------------------------------------
// class A { public void m() { super.m(); }}
// -----------------------------------------------------------------------------
// Statement(4)
//   StatementExpression
//     Expression
//       Expression1
//         Expression2
//           Expression3
//             Primary(4)
//               'super'
//               SuperSuffix
//                 '.'
//                 Identifier <-- 'm'
//                 Arguments '()'
//   ';'
TEST(Parser, PrimarySuper) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() { super.m(); }}";

  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spStatement stmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt;

  ASSERT_EQ(Statement::OPT_STMT_EXPR, stmt->opt);
  spPrimary primary = stmt->stmtExpr->expr->expr1->expr2->expr3->primary;
  ASSERT_EQ(Primary::OPT_SUPER_SUPER_SUFFIX, primary->opt);
  ASSERT_EQ(26, primary->superSuperSuffix->superSuffix->posPeriod);
  ASSERT_EQ(28, primary->superSuperSuffix->superSuffix->args->posLParen);
  ASSERT_EQ(29, primary->superSuperSuffix->superSuffix->args->posRParen);
}

// -----------------------------------------------------------------------------
// class A { void m() { long l = (new Long(i)).longValue(); }}
// -----------------------------------------------------------------------------
// BlockStatement(1)
//   LocalVariableDeclarationStatement
//     Type <-- 'long'
//     VariableDeclarators
//       VariableDeclarator
//         Identifier <-- 'l'
//         VariableDeclaratorRest
//           '='
//           VariableInitializer
//             Expression
//               Expression1
//                 Expression2
//                   Expression3(3)
//                     Primary
//                       ParExpression
//                         '('
//                         Expression*
//                         ')'
//                     Selector(1)
//                       '.'
//                       Identifier <-- 'longValue'
//                       Arguments
//                         '('
//                         ')'
//     ';'
//
// Expression*
//   Expression1
//     Expression2
//       Expression3
//         Primary(5)
//           'new'
//           Creator(1)
//             CreatedName
//               Identifier <-- 'Long'
//             ClassCreatorRest
//               Arguments
//                 '('
//                 Expression <-- 'i'
//                 ')'
TEST(Parser, Selector) {
  std::string filename = "Test.java";
  std::string buffer
    = "class A { void m() { long l = (new Long(i)).longValue(); }}";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spBlockStatement blockStmt = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest
    ->block->blockStmts[0];
  ASSERT_EQ(BlockStatement::OPT_LOCAL_VAR, blockStmt->opt);
  ASSERT_EQ(55, blockStmt->localVar->posSemiColon);

  spVariableDeclaratorRest varDeclRest
    = blockStmt->localVar->varDecls->varDecl->varDeclRest;
  ASSERT_EQ(28, varDeclRest->posEquals);

  spExpression3 expr3 = varDeclRest->varInit->expr->expr1->expr2->expr3;
  ASSERT_EQ(Primary::OPT_PAR_EXPRESSION, expr3->primary->opt);
  ASSERT_EQ(30, expr3->primary->parExpr->posLParen);
  ASSERT_EQ(42, expr3->primary->parExpr->posRParen);

  spSelector selector = expr3->selectors[0];
  ASSERT_EQ(Selector::OPT_IDENTIFIER_ARGUMENTS, selector->opt);
  ASSERT_EQ(53, selector->args->posLParen);
  ASSERT_EQ(54, selector->args->posRParen);
}

/// Primary:
///   NonWildcardTypeArguments
///     ( ExplicitGenericInvocationSuffix | this Arguments )
/// NonWildcardTypeArguments: < TypeList >
/// ExplicitGenericInvocationSuffix:
///   super SuperSuffix
///   Identifier Arguments
/// Arguments: '(' [ Expression { , Expression }] ')'
/// SuperSuffix:
///   Arguments
///   . Identifier [Arguments]
///
/// TODO: It seems that this production rule does not apply when inside of
///       annotations.
//TEST(Parser, PrimaryNonWildcardTypeArguments) {}

TEST(Parser, Statement) {
  std::string filename = "Test.java";
  std::string buffer = "class X { void x() { "
    "try {} catch (Exception e) {} }}";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spStatement stmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt;

  ASSERT_EQ(stmt->opt, Statement::OPT_TRY_BLOCK);
  ASSERT_EQ(21, stmt->tokTry->pos);
  ASSERT_EQ(TOK_KEY_TRY, stmt->tokTry->type);
  ASSERT_EQ(28, stmt->catches->catchClause->tokCatch->pos);
  ASSERT_EQ(TOK_KEY_CATCH, stmt->catches->catchClause->tokCatch->type);
  ASSERT_EQ(34, stmt->catches->catchClause->posLParen);
  ASSERT_EQ(46, stmt->catches->catchClause->posRParen);
  ASSERT_EQ(45, stmt->catches->catchClause->id->pos);
  ASSERT_EQ("e", stmt->catches->catchClause->id->value);
}

// -----------------------------------------------------------------------------
// class A { void m() { while (true) { continue; }}}
// -----------------------------------------------------------------------------
// BlockStatement(3)
//   Statement(8)
//     'while'
//     ParExpression <-- '(true)'
//     Statement(1)
//       Block
//         '{'
//         BlockStatement[0](3)
//           Statement(12)
//             'continue'
//             ';'
//         '}'
TEST(Parser, StatementContinue) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() { while (true) { continue; }}}";

  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spStatement stmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt;

  ASSERT_EQ(Statement::OPT_WHILE, stmt->opt);
  ASSERT_EQ(27, stmt->parExpr->posLParen);
  ASSERT_EQ(32, stmt->parExpr->posRParen);
  ASSERT_EQ(Statement::OPT_BLOCK, stmt->stmtWhile->opt);
  ASSERT_EQ(34, stmt->stmtWhile->block->posLCBracket);
  ASSERT_EQ(46, stmt->stmtWhile->block->posRCBracket);
  spBlockStatement blockStmt = stmt->stmtWhile->block->blockStmts[0];
  ASSERT_EQ(BlockStatement::OPT_ID_STMT, blockStmt->opt);
  ASSERT_EQ(Statement::OPT_CONTINUE, blockStmt->stmt->opt);
  ASSERT_EQ(44, blockStmt->stmt->posSemiColon);
}

// -----------------------------------------------------------------------------
// class A { void m() { synchronized(x) { }}}
// -----------------------------------------------------------------------------
// BlockStatement(3)
//   Statement(15)
//     'synchronized'
//     ParExpression
//     Block
TEST(Parser, StatementSynchronized) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() { synchronized(x) { }}}";

  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spStatement stmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt;

  ASSERT_EQ(stmt->opt, Statement::OPT_SYNC);
  ASSERT_EQ(33, stmt->parExpr->posLParen);
  ASSERT_EQ(35, stmt->parExpr->posRParen);
}

// -----------------------------------------------------------------------------
// class A { int a; static { a = 10; }}
// -----------------------------------------------------------------------------
// ClassBodyDeclaration[0](1)
//   MemberDecl(1)
//     Type <-- int
//     Identifier <-- a
//     MethodOrFieldRest
//       FieldDeclaratorsRest
//         VariableDeclaratorRest <-- nothing to see here
//       ';'
// ClassBodyDeclaration[1](3)
//   'static'
//   Block
//     '{'
//     BlockStatements
//     '}'
TEST(Parser, StaticInitializer) {
  std::string filename = "Test.java";
  std::string buffer = "class A { int a; static { a = 10; }}";

  spDiagnosis diag = spDiagnosis(new Diagnosis);
  Parser parser(filename, buffer, diag);
  parser.parse();

  spClassBodyDeclaration decl0 = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[0];
  ASSERT_EQ(ClassBodyDeclaration::OPT_MODIFIER_MEMBER_DECL, decl0->opt);
  ASSERT_EQ(MemberDecl::OPT_METHOD_OR_FIELD_DECL, decl0->memberDecl->opt);
  ASSERT_EQ(15,
    decl0->memberDecl->methodOrFieldDecl->methodOrFieldRest->posSemiColon);

  spClassBodyDeclaration decl1 = parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classBody->decls[1];
  ASSERT_EQ(ClassBodyDeclaration::OPT_STATIC_BLOCK, decl1->opt);
  ASSERT_EQ(17, decl1->tokStatic->pos);
  ASSERT_EQ(24, decl1->block->posLCBracket);
  ASSERT_EQ(34, decl1->block->posRCBracket);
}

// -----------------------------------------------------------------------------
// class A { void m() { int a = x == 0 ? 1 : 2; }}
// -----------------------------------------------------------------------------
// BlockStatement(1)
//   LocalVariableDeclarationStatement
//     Type <-- 'int'
//     VariableDeclarators
//       VariableDeclarator
//         Identifier <-- a
//         VariableDeclaratorRest
//           '='
//           VariableInitializer
//             Expression
//               Expression1
//                 Expression2
//                   Expression3
//                     Primary <-- 'x'
//                   Expression2Rest
//                     InfixOp <-- '=='
//                     Expression3(3)
//                       Primary <-- '0'
//                 Expression1Rest
//                   '?'
//                   Expression <-- '1'
//                   ':'
//                   Expression1 <-- '2'
//     ';'
TEST(Parser, TernaryCond) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() { int a = x == 0 ? 1 : 2; }}";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spBlockStatement blockStmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0];
  ASSERT_EQ(BlockStatement::OPT_LOCAL_VAR, blockStmt->opt);
  ASSERT_EQ(43, blockStmt->localVar->posSemiColon);
  ASSERT_EQ(27, blockStmt->localVar->varDecls->varDecl->varDeclRest->posEquals);

  spVariableInitializer varInit
    = blockStmt->localVar->varDecls->varDecl->varDeclRest->varInit;
  ASSERT_EQ(VariableInitializer::OPT_EXPRESSION, varInit->opt);

  spPrimary primary = varInit->expr->expr1->expr2->expr3->primary;
  ASSERT_EQ(Primary::OPT_IDENTIFIER, primary->opt);

  spExpression1Rest expr1Rest = varInit->expr->expr1->expr1Rest;
  ASSERT_EQ(36, expr1Rest->posQuestionMark);
  ASSERT_EQ(40, expr1Rest->posColon);
}

// -----------------------------------------------------------------------------
// class A { void m() { throw new E(R.Err, "m"); }}
// -----------------------------------------------------------------------------
// BlockStatement
//   Statement(14)
//     'throw'
//     Expression
//       Expression1
//         Expression2
//           Expression3
//             Primary(5)
//               'new'
//               Creator(2)
//                 CreatedName
//                   Identifier <-- 'E'
//                 ClassCreatorRest
//                   Arguments
//                   '('
//                   Expression
//                   ','
//                   Expression
//                   ')'
//     ';'
TEST(Parser, Throw) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() { throw new E(R.Err, \"m\"); }}";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spStatement stmt = parser.compilationUnit->typeDecls[0]->decl->classDecl
    ->nClassDecl->classBody->decls[0]->memberDecl->voidMethDeclRest->block
    ->blockStmts[0]->stmt;

  ASSERT_EQ(Statement::OPT_THROW, stmt->opt);
  ASSERT_EQ(44, stmt->posSemiColon);

  spPrimary primary = stmt->throwExpr->expr1->expr2->expr3->primary;
  ASSERT_EQ(Primary::OPT_NEW_CREATOR, primary->opt);

  ASSERT_EQ(Creator::OPT_CREATED_NAME, primary->newCreator->creator->opt);
  ASSERT_EQ(32,
    primary->newCreator->creator->opt2->classCreatorRest->args->posLParen);
  ASSERT_EQ(43,
    primary->newCreator->creator->opt2->classCreatorRest->args->posRParen);
}

// -----------------------------------------------------------------------------
// class A { void m() { n = (Integer[]) r(); }}
// -----------------------------------------------------------------------------
// BlockStatement(3)
//   Statement(4)
//   StatementExpression
//     Expression
//       Expression1
//         Expression2
//           Expression3(2?)
//             Expression
//               Expression1
//                 Expression2
//                   Expression3
//                     Primary(7)
//                       Identifier <-- 'n'
//               AssignmentOperator <-- '='
//               Expression1
//                 Expression2
//                   Expression3(3)
//                     Primary(2)
//                       ParExpression
//                         '('
//                         Expression
//                           Expression1
//                             Expression2
//                               Expression3
//                                 Primary
//                                   Identifier <-- Integer
//                                   IdentifierSuffix
//                                     '[' ERR missing '. class' or Expression
//                         ')'
//             Expression3(?)
//   ';'

// -----------------------------------------------------------------------------
// class A { void m() throws E { ; }}
// -----------------------------------------------------------------------------
// MemberDecl(2)
//   'void'
//   Identifier
//   VoidMethodDeclaratorRest
//     FormalParameters
//     'throws'
//     QualifiedIdentifierList
//       QualifiedIdentifier <-- 'E'
//     Block
TEST(Parser, Throws) {
  std::string filename = "Test.java";
  std::string buffer = "class A { void m() throws E { ; }}";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  spVoidMethodDeclaratorRest voidMethDeclRest = parser.compilationUnit
    ->typeDecls[0]->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->voidMethDeclRest;

  ASSERT_EQ(19, voidMethDeclRest->tokThrows->pos);
  ASSERT_EQ(26, voidMethDeclRest->qualifiedIdList->qualifiedId->id->pos);
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
