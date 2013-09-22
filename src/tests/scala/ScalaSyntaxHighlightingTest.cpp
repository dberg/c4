#include <iostream>
#include "c4/ScalaEmacsOutput.h"
#include "c4/ScalaParser.h"
#include "gtest/gtest.h"
using namespace c4;

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
    "(c4-sh-keyword 1 7)"           // object
    "(c4-sh-identifier 8 18)"       // HelloWorld
    "(c4-sh-keyword 19 26)"         // extends
    "(c4-sh-identifier 27 30)"      // App
    "(c4-sh-op 31 32)"              // {
    "(c4-sh-identifier 33 40)"      // println
    "(c4-sh-op 40 41)"              // (
    "(c4-sh-string-literal 41 54)"  // "Hello world
    "(c4-sh-op 54 55)"              // )"
    "(c4-sh-op 55 56)"              // ;
    "(c4-sh-op 57 58)"              // }
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

  std::string expected =
    "["
    "(c4-sh-keyword 1 8)"        // package
    "(c4-sh-identifier 9 13)"    // test
    "(c4-sh-keyword 14 20)"      // import
    "(c4-sh-identifier 21 24)"   // com
    "(c4-sh-op 24 25)"           // .
    "(c4-sh-identifier 25 32)"   // company
    "(c4-sh-op 32 33)"           // .
    "(c4-sh-identifier 33 38)"   // utils
    "(c4-sh-op 38 39)"           // .
    "(c4-sh-op 39 40)"           // _
    "(c4-sh-keyword 41 46)"      // trait
    "(c4-sh-identifier 47 48)"   // X
    "(c4-sh-keyword 49 56)"      // extends
    "(c4-sh-identifier 57 58)"   // Y
    "(c4-sh-keyword 59 63)"      // with
    "(c4-sh-identifier 65 66)"   // Z
    "]";

  ASSERT_EQ(expected, output.sh);
}
