//-*- C++ -*-
#ifndef __C4_JAVA_SOURCE_CODE_STREAM_H_
#define __C4_JAVA_SOURCE_CODE_STREAM_H_

#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

namespace c4 {

class SourceCodeStream;
typedef shared_ptr<SourceCodeStream> spSourceCodeStream;

class SourceCodeStream {

  const string buffer;
  unsigned int cursor;
  unsigned int line;
  unsigned int cursorMark;

public:
  char getChar();
  char getChar(int offset);
  char ungetChar(int count);
  char peekChar(int offset = 0);

  unsigned int getCursor();
  void setCursor(unsigned int _cursor);

  unsigned int getLine();
  void setLine(unsigned int _line);

  unsigned int mark();
  unsigned int restore();
  unsigned int getMarkOffset();

  bool lookaheadInterface(int point);

  unsigned int getStreamLength() { return buffer.length(); }

  SourceCodeStream(const string &_buffer)
   : buffer(_buffer), cursor(0), line(0) {}
};

} // namespace

#endif
