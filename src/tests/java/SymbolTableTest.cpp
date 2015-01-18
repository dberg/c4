#include <string>

#include "c4/java/Parser.h"
#include "c4/java/SymbolTable.h"

#include "gtest/gtest.h"

using namespace c4j;
using std::u32string;

// |------------+-------+-----+-----+------+----+----------|
// | type       | scope | pos | end | line | id | metadata |
// |------------+-------+-----+-----+------+----+----------|
// | _comp_unit |     0 |   0 |  53 |    0 |  0 |          |
// | _class     |     0 |   0 |  53 |    0 |  1 |          |
// | id         |     1 |   6 |   7 |    0 |  2 | A        |
// | _method    |     1 |  10 |  32 |    1 |  3 |          |
// | id         |     3 |  14 |  16 |    1 |  4 | m1       |
// | _method    |     1 |  33 |  45 |    2 |  5 |          |
// | id         |     5 |  38 |  40 |    2 |  6 | m2       |
// | _method    |     1 |  46 |  52 |    3 |  7 |          |
// | id         |     7 |  46 |  47 |    3 |  8 | A        |
// |------------+-------+-----+-----+------+----+----------|
TEST(SymbolTable, Class) {
  u32string filename = U"Test.java";
  u32string buffer =
    U"class A {\n"
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

  Parser parser(filename, buffer);
  parser.parse();

  ASSERT_EQ(9, parser.st.symbols.size());

  // compilation unit
  ASSERT_EQ(ST_COMPILATION_UNIT, parser.st.symbols[0]->type);
  ASSERT_EQ(0, parser.st.symbols[0]->pos);
  //ASSERT_EQ(53, parser.st.symbols[0]->end);

  // class A
  ASSERT_EQ(ST_CLASS, parser.st.symbols[1]->type);
  ASSERT_EQ(0, parser.st.symbols[1]->scope);
  ASSERT_EQ(0, parser.st.symbols[1]->line);
  ASSERT_EQ(0, parser.st.symbols[1]->pos);
  //ASSERT_EQ(53, parser.st.symbols[1]->pos);

  ASSERT_EQ(ST_IDENTIFIER, parser.st.symbols[2]->type);
  ASSERT_EQ(1, parser.st.symbols[2]->scope);
  ASSERT_EQ(0, parser.st.symbols[2]->line);
  ASSERT_EQ(6, parser.st.symbols[2]->pos);
  ASSERT_EQ(7, parser.st.symbols[2]->end);

  // int m1()
  ASSERT_EQ(ST_METHOD, parser.st.symbols[3]->type);
  ASSERT_EQ(1, parser.st.symbols[3]->scope);
  ASSERT_EQ(1, parser.st.symbols[3]->line);
  ASSERT_EQ(10, parser.st.symbols[3]->pos);
  ASSERT_EQ(32, parser.st.symbols[3]->end);

  ASSERT_EQ(ST_IDENTIFIER, parser.st.symbols[4]->type);
  ASSERT_EQ(3, parser.st.symbols[4]->scope);
  ASSERT_EQ(1, parser.st.symbols[4]->line);
  ASSERT_EQ(14, parser.st.symbols[4]->pos);
  ASSERT_EQ(16, parser.st.symbols[4]->end);

  // void m2()
  ASSERT_EQ(ST_METHOD, parser.st.symbols[5]->type);
  ASSERT_EQ(1, parser.st.symbols[5]->scope);
  ASSERT_EQ(2, parser.st.symbols[5]->line);
  ASSERT_EQ(33, parser.st.symbols[5]->pos);
  ASSERT_EQ(45, parser.st.symbols[5]->end);

  ASSERT_EQ(ST_IDENTIFIER, parser.st.symbols[6]->type);
  ASSERT_EQ(5, parser.st.symbols[6]->scope);
  ASSERT_EQ(2, parser.st.symbols[6]->line);
  ASSERT_EQ(38, parser.st.symbols[6]->pos);
  ASSERT_EQ(40, parser.st.symbols[6]->end);

  // A()
  ASSERT_EQ(ST_METHOD, parser.st.symbols[7]->type);
  ASSERT_EQ(1, parser.st.symbols[7]->scope);
  ASSERT_EQ(3, parser.st.symbols[7]->line);
  ASSERT_EQ(46, parser.st.symbols[7]->pos);
  ASSERT_EQ(52, parser.st.symbols[7]->end);

  ASSERT_EQ(ST_IDENTIFIER, parser.st.symbols[8]->type);
  ASSERT_EQ(7, parser.st.symbols[8]->scope);
  ASSERT_EQ(3, parser.st.symbols[8]->line);
  ASSERT_EQ(46, parser.st.symbols[8]->pos);
  ASSERT_EQ(47, parser.st.symbols[8]->end);
}
