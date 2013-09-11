#include "ScalaParserTest.cpp"
#include "ScalaIndentationTest.cpp"
#include "ScalaSyntaxHighlightingTest.cpp"

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
