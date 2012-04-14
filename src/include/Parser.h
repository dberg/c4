//-*- C++ -*-
#ifndef __PARSER_H__
#define __PARSER_H__
#include <string>

namespace djp {
class Parser {
  std::string buffer;
public:
  std::string error;
  int parse(std::string &_buffer);
  
};
}

#endif
