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

  bool equals(int index, std::vector<c4::Char> cs, int offset, int len);

  void enterChars(std::vector<c4::Char> cs, int offset, int len);

public:
  spTermName newTermName(std::vector<c4::Char> cs, int offset, int len,
    std::string cachedString = "");

  spTermName newTermNameCached(std::string s);

  Names();
};

class Name {
protected:
  int index;
  int len;

public:
  Name(int index, int len);

  virtual int start();
  virtual int length();
  virtual spThisNameType encode();
};

class TermName : public Name {
public:
  spTermName next;
  TermName(int index, int len, spTermName next);
};

class TypeName : public Name {
public:
  spTypeName next;
  TypeName(int index, int len, spTypeName next);
};

/** TermName_S and TypeName_S have fields containing the string version of the name.
 *  TermName_R and TypeName_R recreate it each time toString is called.
 */
class TermName_S : public TermName {
protected:
  std::string cachedString;

public:
  TermName_S(int index, int len, spTermName next, std::string cachedString);
  virtual std::string toString();
};

class TypeName_S : public TypeName {

};

class TermName_R : public TermName {
public:
  TermName_R(int index, int len, spTermName next);
};

class TypeName_R : public TypeName {

};

} // namespace

#endif
