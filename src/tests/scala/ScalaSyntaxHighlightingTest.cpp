#include <iostream>
#include "c4/scala/EmacsOutput.h"
#include "c4/scala/Parser.h"
#include "gtest/gtest.h"
using namespace c4s;

TEST(ScalaSyntaxHighlighting, HelloWorld) {
  std::string filename = "HelloWorld.scala";
  std::string buffer =
    "object HelloWorld extends App { println(\"Hello world\"); }";
  Parser parser(filename, buffer);
  parser.parse();

  EmacsOutput output(parser);
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
    "(c4s-sh-string-literal 41 54)"  // "Hello world"
    "(c4s-sh-op 54 55)"              // )
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

  Parser parser(filename, buffer);
  parser.parse();

  EmacsOutput output(parser);
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

TEST(ScalaSyntaxHighlighting, VariantTypeParam) {
  std::string filename = "Example.scala";
  std::string buffer =
    "import scala.annotation.implicitNotFound\n\n"
    "@implicitNotFound(\"message\")\n"
    "trait F[A] extends W[A] with R[A]\n"
    "trait OF[A] extends OW[A] with R[A] with F[A]";

  Parser parser(filename, buffer);
  parser.parse();

  EmacsOutput output(parser);
  output.build();

  std::string expected =
    "["
    "(c4s-sh-keyword 1 7)"               // import
    "(c4s-sh-identifier 8 13)"           // scala
    "(c4s-sh-op 13 14)"                  // .
    "(c4s-sh-identifier 14 24)"          // annotation
    "(c4s-sh-op 24 25)"                  // .
    "(c4s-sh-identifier 25 41)"          // implicitNotFound
    "(c4s-sh-op 43 44)"                  // @
    "(c4s-sh-identifier 44 60)"          // impliticNotFound
    "(c4s-sh-op 60 61)"                  // (
    "(c4s-sh-string-literal 61 70)"      // "message"
    "(c4s-sh-op 70 71)"                  // )"
    "(c4s-sh-keyword 72 77)"             // trait
    "(c4s-sh-identifier 78 79)"          // F
    "(c4s-sh-op 79 80)"                  // [
    "(c4s-sh-identifier 80 81)"          // A
    "(c4s-sh-op 79 80)"                  // ]
    "(c4s-sh-keyword 83 90)"             // extends
    "(c4s-sh-identifier 91 92)"          // W
    "(c4s-sh-op 92 93)"                  // [
    "(c4s-sh-identifier 93 94)"          // A
    "(c4s-sh-op 94 95)"                  // ]
    "(c4s-sh-keyword 96 100)"            // with
    "(c4s-sh-identifier 101 102)"        // R
    "(c4s-sh-op 102 103)"                // [
    "(c4s-sh-identifier 103 104)"        // A
    "(c4s-sh-op 104 105)"                // ]
    "(c4s-sh-keyword 106 111)"           // trait
    "(c4s-sh-identifier 112 114)"        // OF
    "(c4s-sh-op 114 115)"                // [
    "(c4s-sh-identifier 115 116)"        // A
    "(c4s-sh-op 114 115)"                // ]
    "(c4s-sh-keyword 118 125)"           // extends
    "(c4s-sh-identifier 126 128)"        // OW
    "(c4s-sh-op 128 129)"                // [
    "(c4s-sh-identifier 129 130)"        // A
    "(c4s-sh-op 130 131)"                // ]
    "(c4s-sh-keyword 132 136)"           // with
    "(c4s-sh-identifier 137 138)"        // R
    "(c4s-sh-op 138 139)"                // [
    "(c4s-sh-identifier 139 140)"        // A
    "(c4s-sh-op 140 141)"                // ]
    "(c4s-sh-keyword 142 146)"           // with
    "(c4s-sh-identifier 147 148)"        // F
    "(c4s-sh-op 148 149)"                // [
    "(c4s-sh-identifier 149 150)"        // A
    "(c4s-sh-op 150 151)"                // ]
    "]";

  ASSERT_EQ(expected, output.sh);
}
