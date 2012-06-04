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
  ASSERT_EQ(1,
    parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId->ini);
  ASSERT_EQ(11,
    parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId->end);
  ASSERT_EQ(1,
    parser.compilationUnit->pkgDecl->annotations[0]->qualifiedId
    ->identifiers[0]->pos);
  ASSERT_EQ(21,
    parser.compilationUnit->pkgDecl->qualifiedId->ini);
  ASSERT_EQ(28,
    parser.compilationUnit->pkgDecl->qualifiedId->end);
}

TEST(Parser, ImportDeclarations) {
  std::string filename = "Test.java";
  std::string buffer =
    "import com.test1.Test1;\n"
    "import com.test2.*;\n"
    "import static com.test3.Test3;\n"
    "import static com.test4.*;\n";

  Parser parser(filename, buffer);
  parser.parse();
  ASSERT_EQ(4, parser.compilationUnit->impDecls->imports.size());

  // import 1
  ASSERT_EQ(0, parser.compilationUnit->impDecls->imports[0]->posTokImport);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[0]->posTokStatic);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[0]->iniOnDemand);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[0]->endOnDemand);
  ASSERT_EQ("com.test1.Test1",
    parser.compilationUnit->impDecls->imports[0]->getImport());
  ASSERT_EQ(false, parser.compilationUnit->impDecls->imports[0]->err);
  ASSERT_EQ(SINGLE_TYPE_IMPORT_DECLARATION,
    parser.compilationUnit->impDecls->imports[0]->type);

  // import 2
  ASSERT_EQ(24, parser.compilationUnit->impDecls->imports[1]->posTokImport);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[1]->posTokStatic);
  ASSERT_EQ(40, parser.compilationUnit->impDecls->imports[1]->iniOnDemand);
  ASSERT_EQ(41, parser.compilationUnit->impDecls->imports[1]->endOnDemand);
  ASSERT_EQ("com.test2.*",
    parser.compilationUnit->impDecls->imports[1]->getImport());
  ASSERT_EQ(false, parser.compilationUnit->impDecls->imports[0]->err);
  ASSERT_EQ(TYPE_IMPORT_ON_DEMAND_DECLARATION,
    parser.compilationUnit->impDecls->imports[1]->type);

  // import 3
  ASSERT_EQ(44, parser.compilationUnit->impDecls->imports[2]->posTokImport);
  ASSERT_EQ(51, parser.compilationUnit->impDecls->imports[2]->posTokStatic);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[2]->iniOnDemand);
  ASSERT_EQ(-1, parser.compilationUnit->impDecls->imports[2]->endOnDemand);
  ASSERT_EQ("com.test3.Test3",
    parser.compilationUnit->impDecls->imports[2]->getImport());
  ASSERT_EQ(false, parser.compilationUnit->impDecls->imports[2]->err);
  ASSERT_EQ(SINGLE_STATIC_IMPORT_DECLARATION,
    parser.compilationUnit->impDecls->imports[2]->type);

  // import 4
  ASSERT_EQ(75, parser.compilationUnit->impDecls->imports[3]->posTokImport);
  ASSERT_EQ(82, parser.compilationUnit->impDecls->imports[3]->posTokStatic);
  ASSERT_EQ(98, parser.compilationUnit->impDecls->imports[3]->iniOnDemand);
  ASSERT_EQ(99, parser.compilationUnit->impDecls->imports[3]->endOnDemand);
  ASSERT_EQ("com.test4.*",
    parser.compilationUnit->impDecls->imports[3]->getImport());
  ASSERT_EQ(false, parser.compilationUnit->impDecls->imports[3]->err);
  ASSERT_EQ(STATIC_IMPORT_ON_DEMAND_DECLARATION,
    parser.compilationUnit->impDecls->imports[3]->type);
}

TEST(Parser, ClassDeclaration) {
  std::string filename = "Test.java";
  std::string buffer = "@myinterface\npublic class Abc { }";
  Parser parser(filename, buffer);
  parser.parse();
  ASSERT_EQ(1, parser.compilationUnit->typeDecls.size());
  ASSERT_EQ(1, parser.compilationUnit->typeDecls[0]->decl
    ->modifier->annotations.size());
  ASSERT_EQ(1, parser.compilationUnit->typeDecls[0]->decl
    ->modifier->tokens.size());
  ASSERT_EQ(13, parser.compilationUnit->typeDecls[0]->decl
    ->modifier->tokens[0]->pos);
  ASSERT_EQ(TOK_KEY_PUBLIC, parser.compilationUnit->typeDecls[0]->decl
    ->modifier->tokens[0]->type);
  ASSERT_EQ(TOK_KEY_CLASS, parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classTok->type);
  ASSERT_EQ(20, parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->classTok->pos);
  ASSERT_EQ(26, parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->identifier->pos);
  ASSERT_EQ("Abc", parser.compilationUnit->typeDecls[0]->decl
    ->classDecl->nClassDecl->identifier->value);
}

TEST(Parser, Errors) {
  std::string filename = "Test.java";
  std::string buffer = "@";
  Parser parser(filename, buffer);
  parser.parse();
  ASSERT_EQ(1, parser.compilationUnit->errors.size());
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
