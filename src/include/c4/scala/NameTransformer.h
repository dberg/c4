//-*- C++ -*-
#ifndef __C4_SCALA_NAME_TRANSFORMER_H__
#define __C4_SCALA_NAME_TRANSFORMER_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

using std::string;
using std::vector;

namespace c4s {

class OpCodes {
public:
  Char op;
  string code;
  OpCodes* next;

  OpCodes(Char op, string code, OpCodes* next);
  ~OpCodes();
};

/** Provides functions to encode and decode Scala symbolic names. */
class NameTransformer {
private:
  unsigned int nops = 128;
  unsigned int ncodes = 26 * 26;

  vector<string> op2code;
  vector<OpCodes*> code2op;

  void enterOp(Char op, string code);

public:
  NameTransformer();
  ~NameTransformer();

  vector<Char> encode(vector<Char> name);
};

} // namespace

#endif
