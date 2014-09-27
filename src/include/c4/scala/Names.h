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
  std::vector<TermName *> termHashtable;

  /** Hashtable for finding type names quickly. */
  std::vector<TypeName *> typeHashtable;

  int hashValue(std::vector<Char> cs, int offset, int len);

  bool equals(int index, std::vector<Char> cs, int offset, int len);

  void enterChars(std::vector<Char> cs, int offset, int len);

public:
  TermName* newTermName(std::vector<Char> cs, int offset, int len,
    std::string cachedString = "");

  TermName* newTermNameCached(std::string s);

  Names(Global *global);
  virtual ~Names();
};

class Name {
protected:
  Global *global;
  int index;
  int len;

public:
  Name(Global *global, int index, int len);
  virtual ~Name();

  virtual int start();
  virtual int length();
  virtual ThisNameType * encode();
};

class NameOps {
public:
  Name *name;
  NameOps(Name *name);
  ~NameOps();
};

class TermName : public Name {
public:
  TermName *next;
  TermName(Global *global, int index, int len, TermName *next);
  virtual ~TermName();
};

class TypeName : public Name {
public:
  TypeName *next;
  TypeName(Global *global, int index, int len, TypeName *next);
  virtual ~TypeName();
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
             TermName *next, std::string cachedString);
  virtual ~TermName_S();
  virtual std::string toString();
};

class TypeName_S : public TypeName {
  // TODO:
};

class TermName_R : public TermName {
public:
  TermName_R(Global *global, int index, int len, TermName *next);
  virtual ~TermName_R();
  virtual std::string toString();
};

class TypeName_R : public TypeName {
  // TODO:
};

} // namespace

#endif
