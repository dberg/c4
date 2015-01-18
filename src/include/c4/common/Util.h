//-*- C++ -*-
#ifndef __C4_COMMON_UTIL_H__
#define __C4_COMMON_UTIL_H__

#include <algorithm>
#include <cstdlib>
#include <string>
#include <sstream>

using std::u32string;
using std::string;
using std::stringstream;

namespace c4 {

template<typename T>
bool endsWith(T const &str, T const &end) {
  if (str.length() >= end.length()) {
    return (0 == str.compare(str.length() - end.length(), end.length(), end));
  }
  return false;
}

u32string itou32s(int i);
u32string itou32s(unsigned int i);
u32string itou32s(long i);
u32string itou32s(unsigned long i);

const string itos(unsigned long l);
const string itos_hex(unsigned long l);

} // namespace

#endif
