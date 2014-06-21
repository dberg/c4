#include "c4/java/SourceCodeStream.h"

namespace c4 {

/**
 * Return the next char in the buffer or '\0' if we hit the end of the buffer.
 */
char SourceCodeStream::getChar() {
  if (cursor > buffer.length()) {
    return '\0';
  }

  if (buffer[cursor] == '\n') {
    line++;
  }

  return buffer[cursor++];
}

char SourceCodeStream::getChar(int offset) {
  for (int i = 0; i < offset; i++) getChar();
  return buffer[cursor];
}

char SourceCodeStream::ungetChar(int count) {
  for (int i = count; i > 0; i--) {
    if (buffer[--cursor] == '\n') {
      line--;
    }
  }

  return buffer[cursor];
}

char SourceCodeStream::peekChar(int offset) {
  size_t idx = cursor + offset;
  if (idx < buffer.size()) {
    return buffer[cursor + offset];
  }

  return 0;
}

unsigned int SourceCodeStream::getCursor() {
  return cursor;
}

void SourceCodeStream::setCursor(unsigned int _cursor) {
  cursor = _cursor;
}

unsigned int SourceCodeStream::SourceCodeStream::getLine() {
  return line;
}

void SourceCodeStream::setLine(unsigned int _line) {
  line = _line;
}

unsigned int SourceCodeStream::mark() {
  return cursorMark = getCursor();
}

unsigned int SourceCodeStream::restore() {
  ungetChar(getCursor() - cursorMark);
  return getCursor();
}

unsigned int SourceCodeStream::getMarkOffset() {
  return getCursor() - cursorMark;
}

/**
 * We check if the next token is the keyword 'interface'.
 * We assume that any whitespace has been previously consumed.
 */
bool SourceCodeStream::lookaheadInterface(int point) {
  std::string result = std::string(buffer, point, 9);
  if (result == "interface") return true;
  return false;
}
} // namespace
