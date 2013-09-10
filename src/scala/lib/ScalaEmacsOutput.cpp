#include "djp/ScalaEmacsOutput.h"

namespace djp {

void ScalaEmacsOutput::build() {
  auto sh = ScalaSyntaxHighlighting(compUnit);
  sh.build();
}

} // namespace