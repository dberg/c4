#include <iostream>
#include "c4/Indentation.h"
#include "c4/Parser.h"
#include "gtest/gtest.h"
using namespace c4j;

TEST(Indentation, Class) {
  std::string filename = "Test.java";
  std::string buffer =
    "class A{\n"
    "    void m() {}\n"
    "    @Ann\n"
    "    void n() {}\n"
    "    void x(int a,\n"
    "            int b) {\n"
    "    }\n"
    "}\n";

  Parser parser(filename, buffer);
  parser.parse();

  ASSERT_EQ(8, parser.indentMap.size());

  {
    // class A{\n
    spIndentation indent = parser.indentMap[0];
    ASSERT_EQ(0, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }

  {
    // void m() {}\n
    spIndentation indent = parser.indentMap[1];
    ASSERT_EQ(1, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }

  {
    // @Ann\n
    spIndentation indent = parser.indentMap[2];
    ASSERT_EQ(1, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }

  {
    // void n() {}\n
    spIndentation indent = parser.indentMap[3];
    ASSERT_EQ(1, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }

  {
    // void x(int a,\n
    spIndentation indent = parser.indentMap[4];
    ASSERT_EQ(1, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }

  {
    // int b) {\n
    spIndentation indent = parser.indentMap[5];
    ASSERT_EQ(1, indent->level);
    ASSERT_TRUE(indent->lineWrap);
  }

  {
    // }\n
    spIndentation indent = parser.indentMap[6];
    ASSERT_EQ(1, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }

  {
    // }\n
    spIndentation indent = parser.indentMap[7];
    ASSERT_EQ(0, indent->level);
    ASSERT_FALSE(indent->lineWrap);
  }
}
