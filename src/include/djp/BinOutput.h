//-*- C++ -*-
#ifndef __BIN_OUTPUT_H__
#define __BIN_OUTPUT_H__
#include <iostream>
#include <sstream>
#include "djp/ParserBin.h"

namespace djp {

class BinOutput {
  ParserBin &parser;

  void buildHeader();

public:
  BinOutput(ParserBin &parser) : parser(parser) {}
  void build();
  std::stringstream out;
};

} // namespace

#endif
