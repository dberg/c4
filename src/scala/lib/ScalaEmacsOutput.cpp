#include "c4/ScalaEmacsOutput.h"

namespace c4 {

void ScalaEmacsOutput::build() {
  auto shOutput = std::make_shared<ScalaSyntaxHighlighting>(compUnit);
  shOutput->build();
  sh = shOutput->get();
}

} // namespace
