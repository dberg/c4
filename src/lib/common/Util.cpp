#include "c4/common/Util.h"

namespace c4 {

const string itos(unsigned long l) {
  stringstream s; s << l;
  return s.str();
}

const string itos_hex(unsigned long l) {
  stringstream s;
  s << std::hex << l;
  return s.str();
}

} // namespace
