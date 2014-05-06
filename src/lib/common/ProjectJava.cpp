#include "c4/common/ProjectJava.h"

namespace c4 {

void ProjectJava::compile(spCompilationUnit unit) {
  c4j::Parser parser(unit->filename, unit->buffer);
  parser.parse();

  c4j::EmacsOutput output(parser);
  output.build();
  std::string body = output.body();
}

}
