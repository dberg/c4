#include "EmacsOutputTest.cpp"
#include "IndentationTest.cpp"
#include "ParserBinTest.cpp"
#include "ParserTest.cpp"
#include "ScalaParserTest.cpp"
#include "SymbolTableTest.cpp"

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
