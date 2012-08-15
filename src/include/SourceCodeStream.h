//-*- C++ -*-
#ifndef __SOURCE_CODE_STREAM_H_
#define __SOURCE_CODE_STREAM_H_
#include <string>
#include <boost/shared_ptr.hpp>

namespace djp {

class SourceCodeStream;
typedef boost::shared_ptr<SourceCodeStream> spSourceCodeStream;

class SourceCodeStream {

  const std::string buffer;
  unsigned int cursor;
  unsigned int line;

public:
  const char getChar();
  const char ungetChar(int count);
  const char peekChar(int offset = 0) { return buffer[cursor + offset]; }

  unsigned int getCursor() { return cursor; }
  void setCursor(unsigned int _cursor) { cursor = _cursor; }

  unsigned int getLine() { return line; }
  void setLine(unsigned int _line) { line = _line; }

  bool lookaheadInterface(int point);

  unsigned int getStreamLength() { return buffer.length(); }

  SourceCodeStream(const std::string &_buffer)
   : buffer(_buffer), cursor(0), line(0) {}
};

} // namespace

#endif
