#include <string>
#include <vector>

#include "gtest/gtest.h"
#include "utf8/utf8.h"

#include "c4/common/TypeDefs.h"
#include "c4/scala/SourceFile.h"
#include "c4/scala/Global.h"
#include "c4/scala/CompilationUnits.h"

using namespace c4s;

TEST(ScalaCompiler, HelloWorld) {

  std::string filename = "A.scala";
  std::string sourceCode =
    "package test\n"
    "object A {\n"
    "  def main(args: Array[String]) = {\n"
    "    println(\"Hello world\")\n"
    "  }\n"
    "}\n";

  std::vector<c4::Char> buffer;
  utf8::utf8to16(sourceCode.begin(), sourceCode.end(),
    std::back_inserter(buffer));

  ClientSourceFile *source = new ClientSourceFile(filename, buffer);
  spCompilationUnit unit = spCompilationUnit(new CompilationUnit(source));

  std::vector<spCompilationUnit> units;
  units.push_back(unit);

  Global *global = new Global;
  global->compile(units);

  delete source;
  delete global;
}
