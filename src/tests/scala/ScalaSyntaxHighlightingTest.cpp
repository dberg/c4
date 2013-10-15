#include <iostream>
#include "c4/ScalaEmacsOutput.h"
#include "c4/ScalaParser.h"
#include "gtest/gtest.h"
using namespace c4s;

TEST(ScalaSyntaxHighlighting, HelloWorld) {
  std::string filename = "HelloWorld.scala";
  std::string buffer =
    "object HelloWorld extends App { println(\"Hello world\"); }";
  ScalaParser parser(filename, buffer);
  parser.parse();

  ScalaEmacsOutput output(parser);
  output.build();

  std::string expected =
    "["
    "(c4s-sh-keyword 1 7)"           // object
    "(c4s-sh-identifier 8 18)"       // HelloWorld
    "(c4s-sh-keyword 19 26)"         // extends
    "(c4s-sh-identifier 27 30)"      // App
    "(c4s-sh-op 31 32)"              // {
    "(c4s-sh-identifier 33 40)"      // println
    "(c4s-sh-op 40 41)"              // (
    "(c4s-sh-string-literal 41 54)"  // "Hello world
    "(c4s-sh-op 54 55)"              // )"
    "(c4s-sh-op 55 56)"              // ;
    "(c4s-sh-op 57 58)"              // }
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

  ScalaEmacsOutput output(parser);
  output.build();

  std::string expected =
    "["
    "(c4s-sh-keyword 1 8)"        // package
    "(c4s-sh-identifier 9 13)"    // test
    "(c4s-sh-keyword 14 20)"      // import
    "(c4s-sh-identifier 21 24)"   // com
    "(c4s-sh-op 24 25)"           // .
    "(c4s-sh-identifier 25 32)"   // company
    "(c4s-sh-op 32 33)"           // .
    "(c4s-sh-identifier 33 38)"   // utils
    "(c4s-sh-op 38 39)"           // .
    "(c4s-sh-op 39 40)"           // _
    "(c4s-sh-keyword 41 46)"      // trait
    "(c4s-sh-identifier 47 48)"   // X
    "(c4s-sh-keyword 49 56)"      // extends
    "(c4s-sh-identifier 57 58)"   // Y
    "(c4s-sh-keyword 59 63)"      // with
    "(c4s-sh-identifier 65 66)"   // Z
    "]";

  ASSERT_EQ(expected, output.sh);
}
