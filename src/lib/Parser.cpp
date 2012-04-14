#include "Parser.h"
#include "Token.h"

namespace djp {

/// TODO: increase newline counter
const char Parser::getChar() {
  if (cursor > buffer.length())
    return '\0';
  return buffer[cursor++];
}

void Parser::getNextToken() {
  curToken = getToken();
}

int Parser::getToken() {
  char c = getChar();
  if (!c) return TOK_EOF;

  // Skip any space char.
  while (isspace(c)) c = getChar();
  if (!c) return TOK_EOF;

  return c;
}

/// CompilationUnit: Top level parsing
///  PackageDeclaration(opt) ImportDeclaration(opt) typeDeclarations(opt)
void Parser::handleCompilationUnit() {
  if (curToken == TOK_PACKAGE) {

  }

  getNextToken();
}

void Parser::parse() {
  getNextToken();
  while (true) {
    switch (curToken) {
    case TOK_EOF:
      return;
    case TOK_ERROR:
      return;
    default:
      handleCompilationUnit();
    }
  }
}
} // namespace
