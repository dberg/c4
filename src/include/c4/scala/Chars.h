//-*- C++ -*-
#ifndef __C4_SCALA_CHARS_H__
#define __C4_SCALA_CHARS_H__

#include "c4/common/TypeDefs.h"

namespace c4s {

class Chars {
public:
  constexpr static c4::Char LF = '\u000A';
  constexpr static c4::Char FF = '\u000C';
  constexpr static c4::Char CR = '\u000D';
  constexpr static c4::Char SU = '\u001A';
  Chars() {}
};

}

#endif
