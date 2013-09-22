#include <iostream>
#include <string>
#include "c4/EmacsOutput.h"
#include "c4/Parser.h"
#include "gtest/gtest.h"
using namespace c4;

TEST(EmacsOutput, PackageDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npackage com.test;";

  Parser parser(filename, buffer);
  parser.parse();
  EmacsOutput output(parser);
  output.build();
  std::string expected =
    "["
    "(c4-sh-annotation-tok-at 1 2)"
    "(c4-sh-identifier 2 13)"
    "(c4-sh-keyword 14 21)"
    "(c4-sh-identifier 22 25)"
    "(c4-sh-op 25 26)"
    "(c4-sh-identifier 26 30)]";
  ASSERT_EQ(expected, output.outSH.str());
}

TEST(EmacsOutput, ImportDeclaration) {
  std::string filename = "Test.java";
  std::string buffer =
    "import com.test1.Test1;\n"
    "import com.test2.*;\n"
    "import static com.test3.Test3;\n"
    "import static com.test4.*;\n";

  Parser parser(filename, buffer);
  parser.parse();
  EmacsOutput output(parser);
  output.build();
  std::string expected =
    "["
    "(c4-sh-keyword 1 7)"
    "(c4-sh-identifier 8 11)"
    "(c4-sh-op 11 12)"
    "(c4-sh-identifier 12 17)"
    "(c4-sh-op 17 18)"
    "(c4-sh-identifier 18 23)"
    "(c4-sh-keyword 25 31)"
    "(c4-sh-identifier 32 35)"
    "(c4-sh-op 35 36)"
    "(c4-sh-identifier 36 41)"
    "(c4-sh-op 41 43)"
    "(c4-sh-keyword 45 51)"
    "(c4-sh-keyword 52 58)"
    "(c4-sh-identifier 59 62)"
    "(c4-sh-op 62 63)"
    "(c4-sh-identifier 63 68)"
    "(c4-sh-op 68 69)"
    "(c4-sh-identifier 69 74)"
    "(c4-sh-keyword 76 82)"
    "(c4-sh-keyword 83 89)"
    "(c4-sh-identifier 90 93)"
    "(c4-sh-op 93 94)"
    "(c4-sh-identifier 94 99)"
    "(c4-sh-op 99 101)]";
  ASSERT_EQ(expected, output.outSH.str());
}

TEST(EmacsOutput, NormalClassDeclaration) {
  std::string filename = "Test.java";
  std::string buffer =
    "public class Test extends Base {}\n";
  Parser parser(filename, buffer);
  parser.parse();
  EmacsOutput output(parser);
  output.build();
  std::string expected =
    "["
    "(c4-sh-keyword 1 7)"
    "(c4-sh-keyword 8 13)"
    "(c4-sh-reference-type-id 14 18)"
    "(c4-sh-keyword 19 26)"
    "(c4-sh-reference-type-id 27 31)]";
  ASSERT_EQ(expected, output.outSH.str());
}

TEST(EmacsOutput, ClassConstructor) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc() {} }";
  Parser parser(filename, buffer);
  parser.parse();
  EmacsOutput output(parser);
  output.build();
  std::string expected =
    "["
    "(c4-sh-keyword 1 6)"
    "(c4-sh-reference-type-id 7 10)"
    "(c4-sh-identifier 13 16)"
    "(c4-sh-op 19 20)"
    "(c4-sh-op 20 21)]";
  ASSERT_EQ(expected, output.outSH.str());
}

TEST(EmacsOutput, ClassConstructorParameters) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc(int a, double b) {} }";
  Parser parser(filename, buffer);
  parser.parse();
  EmacsOutput output(parser);
  output.build();
  std::string expected =
    "["
    "(c4-sh-keyword 1 6)"
    "(c4-sh-reference-type-id 7 10)"
    "(c4-sh-identifier 13 16)"
    "(c4-sh-keyword 17 20)"
    "(c4-sh-identifier 21 22)"
    "(c4-sh-keyword 24 30)"
    "(c4-sh-identifier 31 32)"
    "(c4-sh-op 34 35)"
    "(c4-sh-op 35 36)]";
  ASSERT_EQ(expected, output.outSH.str());
}

TEST(EmacsOutput, Expression2Rest) {
  std::string filename = "Test.java";
  std::string buffer = "class C { void m() { if (x == null) { return; }}}";
  Parser parser(filename, buffer);
  parser.parse();
  EmacsOutput output(parser);
  output.build();
  std::string expected =
    "["
    "(c4-sh-keyword 1 6)"
    "(c4-sh-reference-type-id 7 8)"
    "(c4-sh-keyword 11 15)" // void
    "(c4-sh-identifier 16 17)" // m
    "(c4-sh-op 17 18)" // (
    "(c4-sh-op 18 19)" // )
    "(c4-sh-op 20 21)" // {
    "(c4-sh-keyword 22 24)" // if
    "(c4-sh-op 25 26)" // (
    "(c4-sh-identifier 26 27)" // x
    "(c4-sh-op 28 30)" // ==
    "(c4-sh-keyword 31 35)" // null
    "(c4-sh-op 35 36)" // )
    "(c4-sh-op 37 38)" // {
    "(c4-sh-keyword 39 45)" // return
    "(c4-sh-op 45 46)" // ;
    "(c4-sh-op 47 48)" // }
    "(c4-sh-op 48 49)]"; // }

  ASSERT_EQ(expected, output.outSH.str());
}

