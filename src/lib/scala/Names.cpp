#include "c4/scala/Names.h"

namespace c4s {

/**
 * The hashcode of a name depends on the first, the last and the middle
 * character, and the length of the name.
 */
int Names::hashValue(std::vector<c4::Char> cs, int offset, int len) {
  if (len > 0) {
    return len * (41 * 41 * 41) +
      cs[offset] * (41 * 41) +
      cs[offset + len - 1] * 41 +
      cs[offset + (len >> 1)];
  } else {
    return 0;
  }
}

/**
 * Create a term name from the characters in cs[offset..offset+len-1].
 */
spTermName Names::newTermName(std::vector<c4::Char> cs, int offset, int len,
  std::string cachedString) {
  // TODO:
  return spTermName(new TermName);
}

} // namespace
