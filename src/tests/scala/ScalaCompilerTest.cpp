#include <string>
#include <vector>

#include "gtest/gtest.h"

#include "c4/scala/SourceFile.h"
#include "c4/scala/Global.h"
#include "utf8/utf8.h"

using namespace c4s;

TEST(ScalaCompiler, HelloWorld) {

  std::string filename = "A.scala";
  std::string sourceCode =
    "object A {"
    "  def main(args: Array[String]) = {"
    "    println(\"Hello world\")"
    "  }"
    "}";

  std::vector<Char> buffer;
  utf8::utf8to16(sourceCode.begin(), sourceCode.end(),
    std::back_inserter(buffer));

  spClientSourceFile source = spClientSourceFile(new ClientSourceFile(
    filename, buffer));
  spCompilationUnit unit = spCompilationUnit(new CompilationUnit(source));

  std::vector<spCompilationUnit> units;
  units.push_back(unit);

  spGlobal global = spGlobal(new Global);
  global->compile(units);
}
