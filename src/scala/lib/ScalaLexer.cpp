#include "djp/ScalaLexer.h"

namespace djp {

STok ScalaLexer::getToken() {
  char c = src->getChar();
  if (!c) return STok::END_OF_FILE;

  // TODO:
  return STok::ERROR;
}

void ScalaLexer::getNextToken() {
  // Save current data for indentation processing
  unsigned line = src->getLine();
  STok token = curToken;

  // Consume token
  curToken = getToken();

  // TODO:
  //processIndentation(line, src->getLine(), token, curToken);
}

} // namespace
