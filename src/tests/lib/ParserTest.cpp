#include <iostream>
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
  Parser parser(filename, buffer);
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
  Parser parser(filename, buffer);
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

TEST(Parser, PackageDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npackage com.test;";

  Parser parser(filename, buffer);
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

TEST(Parser, ImportDeclarations) {
  std::string filename = "Test.java";
  std::string buffer =
    "import com.test1.Test1;\n"
    "import com.test2.*;\n"
    "import static com.test3.Test3;\n"
    "import static com.test4.*;\n";

  Parser parser(filename, buffer);
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
  Parser parser(filename, buffer);
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
  Parser parser(filename, buffer);
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
  Parser parser(filename, buffer);
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
  Parser parser(filename, buffer);
  parser.parse();

  spFormalParameterDecls formParamDecls = parser.compilationUnit->typeDecls[0]
    ->decl->classDecl->nClassDecl->classBody->decls[0]->memberDecl
    ->constDeclRest->formParams->formParamDecls;
  ASSERT_EQ(1, formParamDecls->type->arrayDepth);
  ASSERT_EQ(0, formParamDecls->formParamDeclsRest->varDeclId->arrayDepth);
}

TEST(Parser, ClassConstructorParameters) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc(int a, double b) {} }";
  Parser parser(filename, buffer);
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
  Parser parser(filename, buffer);
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
  Parser parser(filename, buffer);
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
  Parser parser(filename, buffer);
  parser.parse();
  ASSERT_EQ(1, parser.compilationUnit->errors.size());
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
