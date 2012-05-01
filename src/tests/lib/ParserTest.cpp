#include <iostream>
#include "Output.h"
#include "Parser.h"
#include "gtest/gtest.h"
using namespace djp;

TEST(Parser, PackageDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npackage com.test;";
  Parser parser(filename, buffer);
  parser.parse();
  ASSERT_EQ(1, parser.compilationUnit->pkgDecl->annotations.size());
  ASSERT_EQ(0, parser.compilationUnit->pkgDecl->annotations[0]->posTokAt);
  ASSERT_EQ(1, parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId
    ->pos);
  ASSERT_EQ(11, parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId
    ->len);
  ASSERT_EQ(1, parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId
    ->identifiers[0]->pos);
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
