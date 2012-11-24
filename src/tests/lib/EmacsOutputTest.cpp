#include <iostream>
#include <string>
#include "Diagnosis.h"
#include "Output.h"
#include "Parser.h"
#include "gtest/gtest.h"
using namespace djp;

TEST(EmacsOutput, PackageDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npackage com.test;";

  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  Output output(parser.compilationUnit, parser.comments, diag);
  output.build();
  std::string expected =
    "((djp-package-declaration "
    "(djp-node-annotation-tok-at 1)"
    "(djp-node-qualified-id 2 13)"
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
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  Output output(parser.compilationUnit, parser.comments, diag);
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
    "public class Test extends Base {}\n";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  Output output(parser.compilationUnit, parser.comments, diag);
  output.build();
  std::string expected =
    "((djp-class-or-interface-declaration "
    "(djp-node-keyword 1 7)"
    "(djp-normal-class-declaration "
    "(djp-node-keyword 8 13)"
    "(djp-node-reference-type-id 14 18)"
    "(djp-node-keyword 19 26)"
    "(djp-node-reference-type-id 27 31))))";
  ASSERT_EQ(expected, output.output);
}

TEST(EmacsOutput, ClassConstructor) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc() {} }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  Output output(parser.compilationUnit, parser.comments, diag);
  output.build();
  std::string expected =
    "((djp-class-or-interface-declaration "
    "(djp-normal-class-declaration "
    "(djp-node-keyword 1 6)"
    "(djp-node-reference-type-id 7 10)"
    "(djp-member-decl-modifier-member-decl "
    "(djp-node-identifier 13 16)"
    "(djp-constructor-declarator-rest )))))";
  ASSERT_EQ(expected, output.output);
}

TEST(EmacsOutput, ClassConstructorParameters) {
  std::string filename = "Test.java";
  std::string buffer = "class Abc { Abc(int a, double b) {} }";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  Output output(parser.compilationUnit, parser.comments, diag);
  output.build();
  std::string expected =
    "((djp-class-or-interface-declaration "
    "(djp-normal-class-declaration "
    "(djp-node-keyword 1 6)"
    "(djp-node-reference-type-id 7 10)"
    "(djp-member-decl-modifier-member-decl "
    "(djp-node-identifier 13 16)"
    "(djp-constructor-declarator-rest "
    "(djp-node-keyword 17 20)"
    "(djp-node-identifier 21 22)"
    "(djp-node-keyword 24 30)"
    "(djp-node-identifier 31 32))))))";
  ASSERT_EQ(expected, output.output);
}

TEST(EmacsOutput, Expression2Rest) {
  std::string filename = "Test.java";
  std::string buffer = "class C { void m() { if (x == null) { return; }}}";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();
  Output output(parser.compilationUnit, parser.comments, diag);
  output.build();
  std::string expected =
    "((djp-class-or-interface-declaration "
    "(djp-normal-class-declaration "
    "(djp-node-keyword 1 6)"
    "(djp-node-reference-type-id 7 8)"
    "(djp-member-decl-modifier-member-decl "
    "(djp-node-keyword 11 15)" // void
    "(djp-node-identifier 16 17)" // m
    "(djp-node-op 17 18)" // (
    "(djp-node-op 18 19)" // )
    "(djp-node-op 20 21)" // {
    "(djp-node-keyword 22 24)" // if
    "(djp-node-op 25 26)" // (
    "(djp-node-identifier 26 27)" // x
    "(djp-node-op 28 30)" // ==
    "(djp-node-keyword 31 35)" // null
    "(djp-node-op 35 36)" // )
    "(djp-node-op 37 38)" // {
    "(djp-node-keyword 39 45)" // return
    "(djp-node-op 45 46)" // ;
    "(djp-node-op 47 48)" // }
    "(djp-node-op 48 49)))))"; // }

  ASSERT_EQ(expected, output.output);
}

