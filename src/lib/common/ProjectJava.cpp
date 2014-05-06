#include "c4/common/ProjectJava.h"

namespace c4 {

/**
 * Compile the unit buffer and then assign the output result for the client in
 * the unit member variable "output".
 */
void ProjectJava::compile(spCompilationUnit unit) {
  c4j::Parser parser(unit->filename, unit->buffer);
  parser.parse();

  c4j::EmacsOutput output(parser);
  output.build();
  unit->output = output.body();
}

}
