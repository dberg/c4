#include "EmacsOutputTest.cpp"
#include "IndentationTest.cpp"
#include "ParserTest.cpp"
#include "SymbolTableTest.cpp"

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
