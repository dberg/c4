#include "c4/common/ProjectJava.h"

namespace c4 {

/**
 * Compile the unit buffer and then assign the output result for the client in
 * the unit member variable "output".
 */
void ProjectJava::compile(spCompilation comp) {
  c4j::Parser parser(comp->filename, comp->bufferStr);
  parser.parse();

  c4j::EmacsOutput output(parser);
  output.build();
  comp->output = output.body();
}

}
