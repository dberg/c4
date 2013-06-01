#include "djp/Util.h"
#include <sstream>

namespace djp {

const std::string itos(unsigned long l) {
  std::stringstream s; s << l;
  return s.str();
}

const std::string itos_hex(unsigned long l) {
  std::stringstream s; s << std::hex << l;
  return s.str();
}

} // namespace
