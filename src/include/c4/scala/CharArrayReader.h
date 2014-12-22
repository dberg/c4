//-*- C++ -*-
#ifndef __C4_SCALA_CHAR_ARRAY_READER_H__
#define __C4_SCALA_CHAR_ARRAY_READER_H__

#include <vector>
#include "c4/scala/TypeDefs.h"

using std::vector;

namespace c4s {

class CharArrayReaderData {
public:

  /** the last read character */
  Char ch;

  /** The offset one past the last read character */
  unsigned long charOffset;

  CharArrayReaderData();
  virtual ~CharArrayReaderData();
};

class CharArrayReader : public CharArrayReaderData {
public:
  vector<Char> &buf;

  CharArrayReader(vector<Char> &buf);
  virtual ~CharArrayReader();
  void nextChar();
};

} // namespace

#endif
