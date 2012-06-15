#include <iostream>
#include <string>
#include "Output.h"
#include "Parser.h"
#include "gtest/gtest.h"
using namespace djp;

TEST(EmacsOutput, PackageDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npackage com.test;";
  Parser parser(filename, buffer);
  parser.parse();
  Output output(parser.compilationUnit);
  output.build();
  std::string expected =
    "((djp-package-declaration "
    "(djp-node-annotation 1 0 "
    "(djp-node-qualified-id 2 13))"
    "(djp-node-keyword 14 21)"
    "(djp-node-qualified-id 22 30)))";
  ASSERT_EQ(expected, output.output);
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
  Output output(parser.compilationUnit);
  output.build();
  std::string expected = "((djp-import-declarations "
    "(djp-import-declaration "
    "(djp-node-keyword 1 7)"
    "(djp-node-qualified-id 8 23))"
    "(djp-import-declaration "
    "(djp-node-keyword 25 31)"
    "(djp-node-qualified-id 32 43))"
    "(djp-import-declaration "
    "(djp-node-keyword 45 51)"
    "(djp-node-keyword 52 58)"
    "(djp-node-qualified-id 59 74))"
    "(djp-import-declaration "
    "(djp-node-keyword 76 82)"
    "(djp-node-keyword 83 89)"
    "(djp-node-qualified-id 90 101))))";
  ASSERT_EQ(expected, output.output);
}

TEST(EmacsOutput, NormalClassDeclaration) {
  std::string filename = "Test.java";
  std::string buffer =
    "public class Test {}\n";
  Parser parser(filename, buffer);
  parser.parse();
  Output output(parser.compilationUnit);
  output.build();
  std::string expected =
    "((djp-class-or-interface-declaration "
    "(djp-node-keyword 1 7)"
    "(djp-normal-class-declaration "
    "(djp-node-keyword 8 13)"
    "(djp-node-identifier 14 18))))";
  ASSERT_EQ(expected, output.output);
}

TEST(EmacsOutput, ClassConstructor) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc() {} }";
  Parser parser(filename, buffer);
  parser.parse();
  Output output(parser.compilationUnit);
  output.build();
  std::string expected =
    "((djp-class-or-interface-declaration "
    "(djp-normal-class-declaration "
    "(djp-node-keyword 1 6)"
    "(djp-node-identifier 7 10)"
    "(djp-member-decl-1 "
    "(djp-node-identifier 13 16)))))";
  ASSERT_EQ(expected, output.output);
}
