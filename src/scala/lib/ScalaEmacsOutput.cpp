#include "c4/ScalaEmacsOutput.h"

namespace c4 {

void ScalaEmacsOutput::build() {
  // Syntax highlighting
  auto shOutput = std::make_shared<ScalaSyntaxHighlighting>(compUnit);
  shOutput->build();
  sh = shOutput->get();

  // Errors
  // TODO:
  errors = "[]";

  // Indentation
  // TODO:
  indentation = "#s(hash-table size 0 data ())";
}

} // namespace
