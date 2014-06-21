//-*- C++ -*-
#ifndef __C4_BYTECODE_BIN_OUTPUT_CODE_H__
#define __C4_BYTECODE_BIN_OUTPUT_CODE_H__

#include <iomanip>
#include <iostream>
#include <sstream>
#include "c4/bytecode/ASTBin.h"
#include "c4/bytecode/Opcodes.h"

namespace c4 {

bool isWideFormat1(u1 opcode);

class BinOutputCode {
  std::vector<u1> &code;
  u4 idx; // index inside the code vector
  std::stringstream &out;

  OpCodes opcodes;

  u4 getCodeU4();
  u2 getCodeU2();
  u1 getCodeU1();

  std::string codeIdxInfo(size_t maxLineWidth);

public:
  BinOutputCode(std::vector<u1> &code, std::stringstream &out)
    : code(code), idx(0), out(out) {}
  void build();
};

} // namespace

#endif
