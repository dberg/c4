//-*- C++ -*-
#ifndef __PARSER_H__
#define __PARSER_H__
#include <string>
#include <sstream>
#include <vector>
#include "AST.h"
#include "Token.h"

namespace djp {
class Parser {
  std::string filename;
  std::string buffer;

  unsigned int cursor;
  unsigned int line;

  int curToken;
  std::string curTokenStr;

  struct State {
    int cursor, line, token;
    std::string tokenStr;
  };

  TokenUtil tokenUtil;

  // helper methods
  bool isJavaLetter(char c);
  bool isJavaLetterOrDigit(char c);
  void saveState(State &state);
  void restoreState(State &state);

  // lexer
  const char getChar();
  const char ungetChar(int count);
  bool lookaheadInterface(int point);

  void getNextToken();
  int getToken();
  int getAnnotationToken();
  int getTokenIdentifier(char c);

  void parseCompilationUnit();
  spAnnotation parseAnnotation();
  spAnnotationElement parseAnnotationElement();
  void parseAnnotations(std::vector<spAnnotation> &annotations);
  spPackageDeclaration parsePackageDeclaration(
    std::vector<spAnnotation> annotations);
  spQualifiedIdentifier parseQualifiedIdentifier();

public:
  Parser(std::string _filename, std::string &_buffer)
    : filename(_filename), buffer(_buffer), cursor(0), line(1),
      compilationUnit(spCompilationUnit(new CompilationUnit)),
      error(0), error_msg("") {}

  spCompilationUnit compilationUnit;
  int error;
  std::string error_msg;
  void parse();
};
} // namespace

#endif
