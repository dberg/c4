#include "c4/common/Util.h"

namespace c4 {

const std::string itos(unsigned long l) {
  std::stringstream s; s << l;
  return s.str();
}

const std::string itos_hex(unsigned long l) {
  std::stringstream s; s << std::hex << l;
  return s.str();
}

/**
 * Check if the string str ends with the string end.
 */
bool endsWith(std::string const &str, std::string const &end) {
  if (str.length() >= end.length()) {
    return (0 == str.compare(str.length() - end.length(), end.length(), end));
  }
  return false;
}

} // namespace
