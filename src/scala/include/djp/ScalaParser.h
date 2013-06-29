//-*- C++ -*-
#ifndef __SCALA_PARSER_H__
#define __SCALA_PARSER_H__
#include <string>
#include "djp/SourceCodeStream.h"

namespace djp {

class ScalaParser {
  const std::string filename;
  spSourceCodeStream src;

public:
  ScalaParser(const std::string filename, const std::string &buffer)
    : filename(filename), src(spSourceCodeStream(new SourceCodeStream(buffer)))
  {}
};

} // namespace

#endif
