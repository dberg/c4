#include "djp/ScalaEmacsOutput.h"

namespace djp {

void ScalaEmacsOutput::build() {
  auto shOutput = std::make_shared<ScalaSyntaxHighlighting>(compUnit);
  shOutput->build();
  sh = shOutput->get();
}

} // namespace
