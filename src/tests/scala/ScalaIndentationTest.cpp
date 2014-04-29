#include <iostream>
#include "gtest/gtest.h"
#include "c4/scala/Parser.h"
using namespace c4s;

TEST(ScalaIndentation, Class) {
  std::string filename = "HelloWorld.scala";
  std::string buffer =
    "object HelloWorld extends App {\n"
    "  println(\"Hello world\");\n"
    "}\n";

  Parser parser(filename, buffer);
  parser.parse();

  ASSERT_EQ(3, parser.indentMap.size());

  {
    // object HelloWorld extends App {\n
    spIndentation indent = parser.indentMap[0];
    ASSERT_EQ(0, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }

  {
    // println(\"Hello world\");\n
    spIndentation indent = parser.indentMap[1];
    ASSERT_EQ(1, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }

  {
    // }\n
    spIndentation indent = parser.indentMap[2];
    ASSERT_EQ(0, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }
}

