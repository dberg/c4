//-*- C++ -*-
#ifndef __C4_SCALA_NAME_TRANSFORMER_H__
#define __C4_SCALA_NAME_TRANSFORMER_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

using std::u32string;
using std::vector;

namespace c4s {

class OpCodes {
public:
  char32_t op;
  u32string code;
  OpCodes* next;

  OpCodes(char32_t op, u32string code, OpCodes* next);
  ~OpCodes();
};

/** Provides functions to encode and decode Scala symbolic names. */
class NameTransformer {
private:
  unsigned int nops = 128;
  unsigned int ncodes = 26 * 26;

  vector<u32string> op2code;
  vector<OpCodes*> code2op;

  void enterOp(char32_t op, u32string code);

public:
  NameTransformer();
  ~NameTransformer();

  u32string encode(u32string name);
};

} // namespace

#endif
