#include "c4/scala/Names.h"

namespace c4s {

/**
 * Create a term name from the characters in cs[offset..offset+len-1].
 */
spTermName Names::newTermName(std::vector<c4::Char> cs, int offset, int len,
  std::string cachedString) {
  // TODO:
  return spTermName(new TermName);
}

} // namespace
