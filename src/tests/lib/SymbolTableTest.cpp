#include <iostream>
#include "Diagnosis.h"
#include "Output.h"
#include "Parser.h"
#include "SymbolTable.h"
#include "gtest/gtest.h"
using namespace djp;

TEST(SymbolTable, Class) {
  std::string filename = "Test.java";
  std::string buffer =
    "class A {"
    "int m1() { return 1; }" // MemberDecl(1) MethodOrFieldDecl
    "void m2() {}" // MemberDecl(2) void Identifier VoidMethodDeclaratorRest
    "A() {}" // MemberDecl(3) Identifier ConstructorDeclaratorRest
    //TODO:  MemberDecl(4) GenericMethodOrConstructorDecl
    //TODO:  MemberDecl(5) ClassDeclaration
    //TODO:  MemberDecl(6) InterfaceDeclaration
    "}";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  ASSERT_EQ(5, parser.st.symbols.size());
  ASSERT_EQ(ST_COMPILATION_UNIT, parser.st.symbols[0]->type);
}
