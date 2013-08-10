#include <iostream>
#include <string>
#include "djp/EmacsOutput.h"
#include "djp/Parser.h"
#include "gtest/gtest.h"
using namespace djp;

TEST(EmacsOutput, PackageDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npackage com.test;";

  Parser parser(filename, buffer);
  parser.parse();
  EmacsOutput output(parser);
  output.build();
  std::string expected =
    "["
    "(djp-sh-annotation-tok-at 1 2)"
    "(djp-sh-identifier 2 13)"
    "(djp-sh-keyword 14 21)"
    "(djp-sh-identifier 22 25)"
    "(djp-sh-op 25 26)"
    "(djp-sh-identifier 26 30)]";
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
    "(djp-sh-keyword 1 7)"
    "(djp-sh-identifier 8 11)"
    "(djp-sh-op 11 12)"
    "(djp-sh-identifier 12 17)"
    "(djp-sh-op 17 18)"
    "(djp-sh-identifier 18 23)"
    "(djp-sh-keyword 25 31)"
    "(djp-sh-identifier 32 35)"
    "(djp-sh-op 35 36)"
    "(djp-sh-identifier 36 41)"
    "(djp-sh-op 41 43)"
    "(djp-sh-keyword 45 51)"
    "(djp-sh-keyword 52 58)"
    "(djp-sh-identifier 59 62)"
    "(djp-sh-op 62 63)"
    "(djp-sh-identifier 63 68)"
    "(djp-sh-op 68 69)"
    "(djp-sh-identifier 69 74)"
    "(djp-sh-keyword 76 82)"
    "(djp-sh-keyword 83 89)"
    "(djp-sh-identifier 90 93)"
    "(djp-sh-op 93 94)"
    "(djp-sh-identifier 94 99)"
    "(djp-sh-op 99 101)]";
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
    "(djp-sh-keyword 1 7)"
    "(djp-sh-keyword 8 13)"
    "(djp-sh-reference-type-id 14 18)"
    "(djp-sh-keyword 19 26)"
    "(djp-sh-reference-type-id 27 31)]";
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
    "(djp-sh-keyword 1 6)"
    "(djp-sh-reference-type-id 7 10)"
    "(djp-sh-identifier 13 16)"
    "(djp-sh-op 19 20)"
    "(djp-sh-op 20 21)]";
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
    "(djp-sh-keyword 1 6)"
    "(djp-sh-reference-type-id 7 10)"
    "(djp-sh-identifier 13 16)"
    "(djp-sh-keyword 17 20)"
    "(djp-sh-identifier 21 22)"
    "(djp-sh-keyword 24 30)"
    "(djp-sh-identifier 31 32)"
    "(djp-sh-op 34 35)"
    "(djp-sh-op 35 36)]";
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
    "(djp-sh-keyword 1 6)"
    "(djp-sh-reference-type-id 7 8)"
    "(djp-sh-keyword 11 15)" // void
    "(djp-sh-identifier 16 17)" // m
    "(djp-sh-op 17 18)" // (
    "(djp-sh-op 18 19)" // )
    "(djp-sh-op 20 21)" // {
    "(djp-sh-keyword 22 24)" // if
    "(djp-sh-op 25 26)" // (
    "(djp-sh-identifier 26 27)" // x
    "(djp-sh-op 28 30)" // ==
    "(djp-sh-keyword 31 35)" // null
    "(djp-sh-op 35 36)" // )
    "(djp-sh-op 37 38)" // {
    "(djp-sh-keyword 39 45)" // return
    "(djp-sh-op 45 46)" // ;
    "(djp-sh-op 47 48)" // }
    "(djp-sh-op 48 49)]"; // }

  ASSERT_EQ(expected, output.outSH.str());
}

