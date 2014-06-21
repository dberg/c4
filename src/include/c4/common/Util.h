//-*- C++ -*-
#ifndef __C4_COMMON_UTIL_H__
#define __C4_COMMON_UTIL_H__

#include <string>
#include <sstream>

namespace c4 {

bool endsWith(std::string const &str, std::string const &end);
const std::string itos(unsigned long l);
const std::string itos_hex(unsigned long l);

} // namespace

#endif
