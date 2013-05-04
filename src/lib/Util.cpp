#include "Util.h"
#include <sstream>

namespace djp {

const std::string itos(int i) {
  std::stringstream s;
  s << i;
  return s.str();
}

} // namespace
