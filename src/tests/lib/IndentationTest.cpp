#include <iostream>
#include "djp/Indentation.h"
#include "djp/Parser.h"
#include "gtest/gtest.h"
using namespace djp;

TEST(Indentation, Class) {
  std::string filename = "Test.java";
  std::string buffer =
    "class A{\n"
    "    void m() {}\n"
    "}\n";

  Parser parser(filename, buffer);
  parser.parse();

  ASSERT_EQ(3, parser.indentMap.size());

  {
    spIndentation indent = parser.indentMap[0];
    ASSERT_EQ(0, indent->level);
    ASSERT_FALSE(indent->offset);
  }

  {
    spIndentation indent = parser.indentMap[1];
    ASSERT_EQ(1, indent->level);
    ASSERT_FALSE(indent->offset);
  }

  {
    spIndentation indent = parser.indentMap[2];
    ASSERT_EQ(0, indent->level);
    ASSERT_FALSE(indent->offset);
  }
}
