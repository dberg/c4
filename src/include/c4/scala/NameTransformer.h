//-*- C++ -*-
#ifndef __C4_SCALA_NAME_TRANSFORMER_H__
#define __C4_SCALA_NAME_TRANSFORMER_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class OpCodes {
public:
  Char op;
  std::string code;
  OpCodes* next;

  OpCodes(Char op, std::string code, OpCodes* next);
  ~OpCodes();
};

/** Provides functions to encode and decode Scala symbolic names. */
class NameTransformer {
private:
  unsigned int nops = 128;
  unsigned int ncodes = 26 * 26;

  std::vector<std::string> op2code;
  std::vector<OpCodes*> code2op;

  void enterOp(Char op, std::string code);

public:
  NameTransformer();
  ~NameTransformer();

  std::vector<Char> encode(std::vector<Char> name);
};

} // namespace

#endif
