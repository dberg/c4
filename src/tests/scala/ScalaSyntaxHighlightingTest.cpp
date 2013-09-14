#include <iostream>
#include "djp/ScalaEmacsOutput.h"
#include "djp/ScalaParser.h"
#include "gtest/gtest.h"
using namespace djp;

TEST(ScalaSyntaxHighlighting, HelloWorld) {
  std::string filename = "HelloWorld.scala";
  std::string buffer =
    "object HelloWorld extends App { println(\"Hello world\"); }";
  ScalaParser parser(filename, buffer);
  parser.parse();

  ScalaEmacsOutput output(parser.compUnit, parser.diag, parser.indentMap);
  output.build();

  std::string expected =
    "["
    "(djp-sh-keyword 1 7)"           // object
    "(djp-sh-identifier 8 18)"       // HelloWorld
    "(djp-sh-keyword 19 26)"         // extends
    "(djp-sh-identifier 27 30)"      // App
    "(djp-sh-op 31 32)"              // {
    "(djp-sh-identifier 33 40)"      // println
    "(djp-sh-op 40 41)"              // (
    "(djp-sh-string-literal 41 54)"  // "Hello world
    "(djp-sh-op 54 55)"              // )"
    "(djp-sh-op 55 56)"              // ;
    "(djp-sh-op 57 58)"              // }
    "]";

  ASSERT_EQ(expected, output.sh);
}

TEST(ScalaSyntaxHighlighting, Trait) {
  std::string filename = "HelloWorld.scala";
  std::string buffer =
    "package test\n"
    "import com.company.utils._\n"
    "trait X extends Y with Z";

  ScalaParser parser(filename, buffer);
  parser.parse();

  ScalaEmacsOutput output(parser.compUnit, parser.diag, parser.indentMap);
  output.build();

  // TODO:
  // output.sh
}