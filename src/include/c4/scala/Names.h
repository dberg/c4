//-*- C++ -*-
#ifndef __C4_SCALA_NAMES_H__
#define __C4_SCALA_NAMES_H__

#include <string>
#include <vector>
#include "c4/common/TypeDefs.h"
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Names {
private:

  constexpr static int HASH_SIZE = 0x8000;
  constexpr static int HASH_MASK = 0x7FFF;
  constexpr static int NAME_SIZE = 0x20000;

  /** Memory to store all names sequentially. */
  std::vector<c4::Char> chrs;
  unsigned long nc;

  /** Hashtable for finding term names quickly. */
  std::vector<spTermName> termHashtable;

  /** Hashtable for finding type names quickly. */
  std::vector<spTypeName> typeHashtable;

  int hashValue(std::vector<c4::Char> cs, int offset, int len);

public:
  spTermName newTermName(std::vector<c4::Char> cs, int offset, int len,
    std::string cachedString = "");

  Names();
};

class Name {
protected:
  int index;
  int len;

public:
  Name(int index, int len);
};

class TermName : public Name {
private:
  spTermName next;

public:
  TermName(int index, int len, spTermName next);
};

class TypeName : public Name {
private:
  spTypeName next;

public:
  TypeName(int index, int len, spTypeName next);
};

} // namespace

#endif
