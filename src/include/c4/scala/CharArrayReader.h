//-*- C++ -*-
#ifndef __C4_SCALA_CHAR_ARRAY_READER_H__
#define __C4_SCALA_CHAR_ARRAY_READER_H__

#include <memory>

#include "c4/scala/TypeDefs.h"

namespace c4s {

//class CharArrayReaderData;
//typedef std::shared_ptr<CharArrayReaderData> spCharArrayReaderData;

//class CharArrayReader;
//typedef std::shared_ptr<CharArrayReader> spCharArrayReader;

class CharArrayReaderData {
protected:

  /** the last read character */
  Char ch;

  /** The offset one past the last read character */
  int charOffset;

  CharArrayReaderData(): charOffset(0) {}

};

class CharArrayReader : public CharArrayReaderData {};

public:

  virtual void nextChar();

} // namespace

#endif
