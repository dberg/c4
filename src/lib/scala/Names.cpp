#include "utf8/utf8.h"
#include "c4/scala/Names.h"

namespace c4s {

/** Constructor */
Names::Names(): chrs(NAME_SIZE, 0), nc(0),
                termHashtable(HASH_SIZE, nullptr),
                typeHashtable(HASH_SIZE, nullptr) {}

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
 * Is (the ASCII representation of) name at given index equal to
 * cs[offset..offset+len-1]?
 */
bool Names::equals(int index, std::vector<c4::Char> cs, int offset, int len) {
  int i = 0;
  while ((i < len) && (chrs[index + i] == cs[offset + i])) {
    ++i;
  }
  return i == len;
}

/**
 * Create a term name from the characters in cs[offset..offset+len-1].
 */
spTermName Names::newTermName(std::vector<c4::Char> cs, int offset, int len,
  std::string cachedString) {
  if (len < 0) len = 0;
  int h = hashValue(cs, offset, len) & HASH_MASK;
  spTermName n = termHashtable[h];
  while (n && (n->length() != len || !equals(n->start(), cs, offset, len))) {
    n = n->next;
  }

  if (n) {
    return n;
  } else {
    int startIndex = 0;
    // TODO:
    // if (cs == chrs) {
    //   // Optimize for subName, the new name is already stored in chrs
    //   startIndex = offset;
    // } else {
    spTermName next = termHashtable[h];
    spTermName termName = (cachedString.size())
      ? spTermName(new TermName_S(startIndex, len, next, cachedString))
      : spTermName(new TermName_R(startIndex, len, next));
    termHashtable[h] = termName;
    return termName;
    // }
  }
}

spTermName Names::newTermNameCached(std::string s) {
  std::vector<c4::Char> tmp;
  utf8::utf8to16(s.begin(), s.end(), back_inserter(tmp));
  return newTermName(tmp, 0, tmp.size(), s);
}

/** Constructor */
Name::Name(int index, int len): index(index), len(len) {}

int Name::start() {
  return index;
}

int Name::length() {
  return len;
}

/** Constructor */
TermName::TermName(int index, int len, spTermName next)
  : Name(index, len), next(next) {}

/** Constructor */
TypeName::TypeName(int index, int len, spTypeName next)
  : Name(index, len), next(next) {}

/** Constructor */
TermName_S::TermName_S(int index, int len, spTermName next,
                       std::string cachedString)
  : TermName(index, len, next), cachedString(cachedString) {}

std::string TermName_S::toString() {
  return cachedString;
}

/** Constructor */
TermName_R::TermName_R(int index, int len, spTermName next)
  : TermName(index, len, next) {}

} // namespace
