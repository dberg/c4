//-*- C++ -*-
#ifndef __C4_SCALA_NAMES_H__
#define __C4_SCALA_NAMES_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Names {
private:

  Global *global;

  constexpr static int HASH_SIZE = 0x8000;
  constexpr static int HASH_MASK = 0x7FFF;
  constexpr static int NAME_SIZE = 0x20000;

public:
  /** Memory to store all names sequentially. */
  std::vector<Char> chrs;
  unsigned long nc;

private:
  /** Hashtable for finding term names quickly. */
  std::vector<spTermName> termHashtable;

  /** Hashtable for finding type names quickly. */
  std::vector<spTypeName> typeHashtable;

  int hashValue(std::vector<Char> cs, int offset, int len);

  bool equals(int index, std::vector<Char> cs, int offset, int len);

  void enterChars(std::vector<Char> cs, int offset, int len);

public:
  spTermName newTermName(std::vector<Char> cs, int offset, int len,
    std::string cachedString = "");

  spTermName newTermNameCached(std::string s);

  Names(Global *global);
};

class Name {
protected:
  Global *global;
  int index;
  int len;

public:
  Name(Global *global, int index, int len);

  virtual int start();
  virtual int length();
  virtual spThisNameType encode();

  // NameOps
  //spTermName dropLocal();
  //T stripSuffix(std::string suffix);
};

class NameOps {
public:
  NameOps();
};

class TermName : public Name {
public:
  spTermName next;
  TermName(Global *global, int index, int len, spTermName next);
};

class TypeName : public Name {
public:
  spTypeName next;
  TypeName(Global *global, int index, int len, spTypeName next);
};

/**
 * TermName_S and TypeName_S have fields containing the string version of the name.
 * TermName_R and TypeName_R recreate it each time toString is called.
 */
class TermName_S : public TermName {
protected:
  std::string cachedString;

public:
  TermName_S(Global *global, int index, int len,
             spTermName next, std::string cachedString);
  virtual std::string toString();
};

class TypeName_S : public TypeName {
  // TODO:
};

class TermName_R : public TermName {
public:
  TermName_R(Global *global, int index, int len, spTermName next);
  virtual std::string toString();
};

class TypeName_R : public TypeName {
  // TODO:
};

} // namespace

#endif
