//-*- C++ -*-
#ifndef __C4_SCALA_CHAR_ARRAY_READER_H__
#define __C4_SCALA_CHAR_ARRAY_READER_H__

#include <vector>

#include "c4/common/TypeDefs.h"
#include "c4/scala/Chars.h"

namespace c4s {

class CharArrayReaderData {
protected:

  /** the last read character */
  c4::Char ch;

  /** The offset one past the last read character */
  unsigned long charOffset;

public:
  CharArrayReaderData(): charOffset(0) {}

};

class CharArrayReader : public CharArrayReaderData {
protected:
  std::vector<c4::Char> buf;

public:
  CharArrayReader(): CharArrayReaderData() {}
  void nextChar();
};

} // namespace

#endif
