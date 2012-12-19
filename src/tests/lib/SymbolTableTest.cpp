#include <iostream>
#include "Diagnosis.h"
#include "Output.h"
#include "Parser.h"
#include "SymbolTable.h"
#include "gtest/gtest.h"
using namespace djp;

// |-----------+-------+-------+-----+------+------|
// | type      | scope | token | pos | line | name |
// |-----------+-------+-------+-----+------+------|
// | comp_unit |     0 |       |   0 |    0 |      |
// | class     |     0 |       |   6 |    0 | A    |
// | method    |     1 |       |  14 |    1 | m1   |
// | method    |     1 |       |  38 |    2 | m2   |
// | method    |     1 |       |  46 |    3 | A    |
// |-----------+-------+-------+-----+------+------|
TEST(SymbolTable, Class) {
  std::string filename = "Test.java";
  std::string buffer =
    "class A {\n"
    // MemberDecl(1) MethodOrFieldDecl
    "int m1() { return 1; }\n"
    // MemberDecl(2) void Identifier VoidMethodDeclaratorRest
    "void m2() {}\n"
    // MemberDecl(3) Identifier ConstructorDeclaratorRest
    "A() {}\n"
    //TODO:  MemberDecl(4) GenericMethodOrConstructorDecl
    //TODO:  MemberDecl(5) ClassDeclaration
    //TODO:  MemberDecl(6) InterfaceDeclaration
    "}";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  ASSERT_EQ(5, parser.st.symbols.size());
  ASSERT_EQ(ST_COMPILATION_UNIT, parser.st.symbols[0]->type);
  ASSERT_EQ(ST_CLASS, parser.st.symbols[1]->type);
  ASSERT_EQ(0, parser.st.symbols[1]->line);
  ASSERT_EQ(ST_METHOD, parser.st.symbols[2]->type);
  ASSERT_EQ(1, parser.st.symbols[2]->line);
  ASSERT_EQ(ST_METHOD, parser.st.symbols[3]->type);
  ASSERT_EQ(2, parser.st.symbols[3]->line);
  ASSERT_EQ(ST_METHOD, parser.st.symbols[4]->type);
  ASSERT_EQ(3, parser.st.symbols[4]->line);
}
