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

  // TODO:
  // output.sh;
}