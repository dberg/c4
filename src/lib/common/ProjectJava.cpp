#include "c4/common/ProjectJava.h"

namespace c4 {

/**
 * Compile the unit buffer and then assign the output result for the client in
 * the unit member variable "output".
 */
void ProjectJava::compile(spCompilationRequest compReq) {
  c4j::Parser parser(compReq->filename, compReq->buffer);
  parser.parse();

  c4j::EmacsOutput output(parser);
  output.build();
  // TODO: return the compilation response
  // output.body();
}

}
