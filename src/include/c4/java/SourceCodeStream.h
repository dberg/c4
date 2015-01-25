//-*- C++ -*-
#ifndef __C4_JAVA_SOURCE_CODE_STREAM_H_
#define __C4_JAVA_SOURCE_CODE_STREAM_H_

#include <memory>
#include <string>

using std::shared_ptr;
using std::u32string;

namespace c4j {

class SourceCodeStream;
typedef shared_ptr<SourceCodeStream> spSourceCodeStream;

class SourceCodeStream {

  const u32string buffer;
  unsigned int cursor;
  unsigned int line;
  unsigned int cursorMark;

public:
  char32_t getChar();
  char32_t getChar(int offset);
  char32_t ungetChar(int count);
  char32_t peekChar(int offset = 0);

  unsigned int getCursor();
  void setCursor(unsigned int _cursor);

  unsigned int getLine();
  void setLine(unsigned int _line);

  unsigned int mark();
  unsigned int restore();
  unsigned int getMarkOffset();

  bool lookaheadInterface(int point);

  unsigned int getStreamLength();

  SourceCodeStream(const u32string &_buffer);
};

} // namespace

#endif
