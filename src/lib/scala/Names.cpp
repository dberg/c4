#include "c4/scala/Names.h"

namespace c4s {

/** Constructor */
Names::Names(): chrs(NAME_SIZE), nc(0),
                termHashtable(HASH_SIZE),
                typeHashtable(HASH_SIZE) {}

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
  return spTermName(new TermName(0, 0, nullptr));
}

/** Constructor */
Name::Name(int index, int len): index(index), len(len) {}

/** Constructor */
TermName::TermName(int index, int len, spTermName next)
  : Name(index, len), next(next) {}

/** Constructor */
TypeName::TypeName(int index, int len, spTypeName next)
  : Name(index, len), next(next) {}

} // namespace
