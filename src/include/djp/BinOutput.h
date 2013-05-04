//-*- C++ -*-
#ifndef __BIN_OUTPUT_H__
#define __BIN_OUTPUT_H__
#include <iostream>
#include "djp/ParserBin.h"
#include "djp/Util.h"

namespace djp {

class BinOutput {
  ParserBin &parser;

public:
  BinOutput(ParserBin &parser) : parser(parser) {}
  void build();

  std::string out;
};

} // namespace

#endif
