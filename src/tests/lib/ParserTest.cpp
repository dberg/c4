#include <iostream>
#include "Diagnosis.h"
#include "Output.h"
#include "Parser.h"
#include "SymbolTable.h"
#include "gtest/gtest.h"
using namespace djp;

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

TEST(Parser, AnnotationElementValuePairsCharacterLiteral) {
  std::string filename = "Test.java";
  // INFO: We escape the universal character name \uAa89
  // so it's not pre-processed by the c++ compiler.
  std::string buffer =
    "@myinterface(v1='A', v2='\n', v3='\034', v4='\\uAa89')"
    //               16      24       33         44
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
  ASSERT_EQ("'\n'", charLiteralPair2->val);

  // Pair 3
  spElementValuePair pair3 = pairs[2];
  spExpression3 expr3Pair3 = pair3->value->expr1->expr2->expr3;
  spCharacterLiteral charLiteralPair3
    = expr3Pair3->primary->literal->charLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair3->primary->opt);
  ASSERT_EQ(Literal::OPT_CHAR, expr3Pair3->primary->literal->opt);
  ASSERT_EQ(32, charLiteralPair3->pos);
  ASSERT_EQ("'\034'", charLiteralPair3->val);

  // Pair 4
  spElementValuePair pair4 = pairs[3];
  spExpression3 expr3Pair4 = pair4->value->expr1->expr2->expr3;
  spCharacterLiteral charLiteralPair4
    = expr3Pair4->primary->literal->charLiteral;
  ASSERT_EQ(Primary::OPT_LITERAL, expr3Pair4->primary->opt);
  ASSERT_EQ(Literal::OPT_CHAR, expr3Pair4->primary->literal->opt);
  ASSERT_EQ(40, charLiteralPair4->pos);
  ASSERT_EQ("'\\uAa89'", charLiteralPair4->val);
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

TEST(Parser, PackageDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npackage com.test;";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  ASSERT_EQ(1, parser.compilationUnit->pkgDecl->annotations.size());
  ASSERT_EQ(0, parser.compilationUnit->pkgDecl->annotations[0]->posTokAt);
  ASSERT_EQ(1,
    parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId->ini);
  ASSERT_EQ(11,
    parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId->end);
  ASSERT_EQ(1,
    parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId
    ->identifiers[0]->pos);
  ASSERT_EQ(21,
    parser.compilationUnit->pkgDecl->qualifiedId->ini);
  ASSERT_EQ(28,
    parser.compilationUnit->pkgDecl->qualifiedId->end);
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
  ASSERT_EQ(23,
    creator1->opt1->nonWildcardTypeArguments->typeList->refType->id->pos);
  ASSERT_EQ("Integer",
    creator1->opt1->nonWildcardTypeArguments->typeList->refType->id->value);
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
  ASSERT_EQ(54,
    creator2->opt1->nonWildcardTypeArguments->typeList->refType->id->pos);
  ASSERT_EQ("String",
    creator2->opt1->nonWildcardTypeArguments->typeList->refType->id->value);
  ASSERT_EQ(62,
    creator2->opt1->nonWildcardTypeArguments->typeList->refTypes[0]->id->pos);
  ASSERT_EQ("Integer",
    creator2->opt1->nonWildcardTypeArguments->typeList->refTypes[0]->id->value);
  ASSERT_EQ(71, creator2->opt1->createdName->id->pos);
  ASSERT_EQ("MyClass", creator2->opt1->createdName->id->value);
  ASSERT_EQ(TypeArgumentsOrDiamond::OPT_TYPE_ARGUMENTS,
    creator2->opt1->createdName->typeArgsOrDiam->opt);
  ASSERT_EQ(78,
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->posLt);
  ASSERT_EQ(83,
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->posGt);
  ASSERT_EQ(TypeArgument::OPT_REFERENCE_TYPE,
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->typeArg->opt);
  ASSERT_EQ(79,
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->typeArg
    ->refType->id->pos);
  ASSERT_EQ("Long",
    creator2->opt1->createdName->typeArgsOrDiam->typeArgs->typeArg
    ->refType->id->value);

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

/// Primary:
///   NonWildcardTypeArguments
///     ( ExplicitGenericInvocationSuffix | this Arguments )
/// NonWildcardTypeArguments: < TypeList >
/// ExplicitGenericInvocationSuffix:
///   super SuperSuffix
///   Identifier Arguments
/// Arguments: '(' [ Expression { , Expression }] ')'w
/// SuperSuffix:
///   Arguments
///   . Identifier [Arguments]
///
/// TODO: It seems that this production rule does not apply when inside of
///       annotations.
//TEST(Parser, PrimaryNonWildcardTypeArguments) {}

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
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[0]->iniOnDemand);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[0]->endOnDemand);
  ASSERT_EQ("com.test1.Test1",
    parser.compilationUnit->impDecls->imports[0]->getImport());
  ASSERT_FALSE(parser.compilationUnit->impDecls->imports[0]->err);
  ASSERT_EQ(SINGLE_TYPE_IMPORT_DECLARATION,
    parser.compilationUnit->impDecls->imports[0]->type);

  // import 2
  ASSERT_EQ(24, parser.compilationUnit->impDecls->imports[1]->posTokImport);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[1]->posTokStatic);
  ASSERT_EQ(40, parser.compilationUnit->impDecls->imports[1]->iniOnDemand);
  ASSERT_EQ(41, parser.compilationUnit->impDecls->imports[1]->endOnDemand);
  ASSERT_EQ("com.test2.*",
    parser.compilationUnit->impDecls->imports[1]->getImport());
  ASSERT_FALSE(parser.compilationUnit->impDecls->imports[0]->err);
  ASSERT_EQ(TYPE_IMPORT_ON_DEMAND_DECLARATION,
    parser.compilationUnit->impDecls->imports[1]->type);

  // import 3
  ASSERT_EQ(44, parser.compilationUnit->impDecls->imports[2]->posTokImport);
  ASSERT_EQ(51, parser.compilationUnit->impDecls->imports[2]->posTokStatic);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[2]->iniOnDemand);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[2]->endOnDemand);
  ASSERT_EQ("com.test3.Test3",
    parser.compilationUnit->impDecls->imports[2]->getImport());
  ASSERT_FALSE(parser.compilationUnit->impDecls->imports[2]->err);
  ASSERT_EQ(SINGLE_STATIC_IMPORT_DECLARATION,
    parser.compilationUnit->impDecls->imports[2]->type);

  // import 4
  ASSERT_EQ(75, parser.compilationUnit->impDecls->imports[3]->posTokImport);
  ASSERT_EQ(82, parser.compilationUnit->impDecls->imports[3]->posTokStatic);
  ASSERT_EQ(98, parser.compilationUnit->impDecls->imports[3]->iniOnDemand);
  ASSERT_EQ(99, parser.compilationUnit->impDecls->imports[3]->endOnDemand);
  ASSERT_EQ("com.test4.*",
    parser.compilationUnit->impDecls->imports[3]->getImport());
  ASSERT_FALSE(parser.compilationUnit->impDecls->imports[3]->err);
  ASSERT_EQ(STATIC_IMPORT_ON_DEMAND_DECLARATION,
    parser.compilationUnit->impDecls->imports[3]->type);
}

TEST(Parser, ClassDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npublic class Abc { }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  ASSERT_EQ(1, parser.compilationUnit->typeDecls.size());
  ASSERT_EQ(1, parser.compilationUnit->typeDecls[0]->decl
    ->modifier->annotations.size());
  ASSERT_EQ(1, parser.compilationUnit->typeDecls[0]->decl
    ->modifier->tokens.size());
  ASSERT_EQ(13, parser.compilationUnit->typeDecls[0]->decl
    ->modifier->tokens[0]->pos);
  ASSERT_EQ(TOK_KEY_PUBLIC, parser.compilationUnit->typeDecls[0]->decl
    ->modifier->tokens[0]->type);
  ASSERT_EQ(TOK_KEY_CLASS, parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classTok->type);
  ASSERT_EQ(20, parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classTok->pos);
  ASSERT_EQ(26, parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->identifier->pos);
  ASSERT_EQ("Abc", parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->identifier->value);

  ASSERT_EQ(2, parser.st.symbols.size());
  spSymbol sym = parser.st.scopePeek();
  ASSERT_EQ(ST_CLASS, sym->type);
  ASSERT_EQ(0, sym->scope);
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
  ASSERT_EQ(12, classBodyDecl->memberDecl->identifier->pos);
  ASSERT_EQ("Abc", classBodyDecl->memberDecl->identifier->value);
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

TEST(Parser, Errors) {
  std::string filename = "Test.java";
  std::string buffer = "@";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  ASSERT_EQ(1, diag->errors.size());
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
