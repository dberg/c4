#include <iostream>
#include "djp/ScalaParser.h"
#include "gtest/gtest.h"
using namespace djp;

TEST(ScalaIndentation, Class) {
  std::string filename = "HelloWorld.scala";
  std::string buffer =
    "object HelloWorld extends App {\n"
    "  println(\"Hello world\");\n"
    "}\n";

  ScalaParser parser(filename, buffer);
  parser.parse();

  /*
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
  */
}

