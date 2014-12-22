#include <string>
#include <vector>

#include "gtest/gtest.h"
#include "utf8/utf8.h"

#include "c4/scala/TypeDefs.h"
#include "c4/scala/SourceFile.h"
#include "c4/scala/Global.h"
#include "c4/scala/CompilationUnits.h"

using namespace c4s;
using std::string;
using std::vector;

TEST(ScalaCompiler, HelloWorld) {

  string filename = "A.scala";
  string sourceCode =
    "package test\n"
    "object A {\n"
    "  def main(args: Array[String]) = {\n"
    "    println(\"Hello world\")\n"
    "  }\n"
    "}\n";

  vector<Char> buffer;
  utf8::utf8to16(sourceCode.begin(), sourceCode.end(),
    std::back_inserter(buffer));

  SourceFile *source = new SourceFile(filename, buffer);
  CompilationUnit *unit = new CompilationUnit(source);

  vector<CompilationUnit *> units;
  units.push_back(unit);

  Global *global = new Global;
  global->compile(units);

  delete global;
  for (auto u : units) { delete u; }
  delete source;
}
