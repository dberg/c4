#include "djp/ScalaEmacsOutput.h"

namespace djp {

void ScalaEmacsOutput::build() {
  auto shOutput = ScalaSyntaxHighlighting(compUnit);
  shOutput.build();
  sh = shOutput.get();
}

} // namespace