//-*- C++ -*-
#ifndef __PARSER_H__
#define __PARSER_H__
#include <string>
#include <vector>

namespace djp {
class Parser {
  std::string filename;
  std::string buffer;

  unsigned int cursor;
  unsigned int line;

  int curToken;
  std::string curTokenStr;

  // lexer
  const char getChar();
  void getNextToken();
  int getToken();

  void parseCompilationUnit();
  void parseAnnotations();

public:
  Parser(std::string _filename, std::string &_buffer)
    : filename(_filename), buffer(_buffer), cursor(0), line(0), error(0) {}
  int error;
  std::string error_msg;
  void parse();
};
}

#endif
