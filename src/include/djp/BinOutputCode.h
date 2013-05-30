//-*- C++ -*-
#ifndef __BIN_OUTPUT_CODE_H__
#define __BIN_OUTPUT_CODE_H__
#include <iostream>
#include <sstream>
#include "djp/ASTBin.h"
#include "djp/Opcodes.h"

namespace djp {

bool isWideFormat1(u1 opcode);

class BinOutputCode {
  std::vector<u1> &code;
  u4 idx; // index inside the code vector
  unsigned long codeIdxGlobal; // idx of first byte relative to the .class file
  std::stringstream &out;

  OpCodes opcodes;

  u4 getCodeU4();
  u2 getCodeU2();

public:
  BinOutputCode(std::vector<u1> &code, unsigned long codeIdxGlobal,
    std::stringstream &out)
    : code(code), idx(0), codeIdxGlobal(codeIdxGlobal), out(out) {}
  void build();
};

} // namespace

#endif
