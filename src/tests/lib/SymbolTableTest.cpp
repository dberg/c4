#include <iostream>
#include "Diagnosis.h"
#include "Output.h"
#include "Parser.h"
#include "SymbolTable.h"
#include "gtest/gtest.h"
using namespace djp;

TEST(SymbolTable, Class) {
  std::string filename = "Test.java";
  std::string buffer = "class A {}";
  spDiagnosis diag = spDiagnosis(new Diagnosis());
  Parser parser(filename, buffer, diag);
  parser.parse();

  ASSERT_EQ(2, parser.st.symbols.size());
  spSymbol sym = parser.st.scopePeek();
  ASSERT_EQ(ST_COMPILATION_UNIT, sym->type);
}
