//-*- C++ -*-
#ifndef __PARSERBIN_H__
#define __PARSERBIN_H__
#include <stdint.h> // cstdint
#include <string>
#include <vector>
#include "ASTBin.h"

namespace djp {

class ParserBin {

  enum ParserBinErr {
    ERR_INVALID_MAGIC_NUMBER
  };

  unsigned pos; // current buffer position
  int err;
  const std::string filename;
  std::vector<unsigned char> buffer;

  void addErr(int errCode) { err = errCode; }
  u1 getU1();
  u2 getU2();
  u4 getU4();
  void parseClassFile();
  void parseConstantPool(unsigned poolCount,
    std::vector<spCPInfo> &constantPool);

public:
  ParserBin(const std::string filename,
    const std::vector<unsigned char> &buffer)
    : pos(0), err(0), filename(filename), buffer(buffer) {}

  spClassFile classFile;

  void parse();
};

} // namespace

#endif
