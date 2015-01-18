//-*- C++ -*-
#ifndef __C4_SCALA_CHARS_H__
#define __C4_SCALA_CHARS_H__

namespace c4s {

class Chars {
public:
  constexpr static char16_t LF = '\u000A';
  constexpr static char16_t FF = '\u000C';
  constexpr static char16_t CR = '\u000D';
  constexpr static char16_t SU = '\u001A';
  Chars() {}
};

} // namespace

#endif
