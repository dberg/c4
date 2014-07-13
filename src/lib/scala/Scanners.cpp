#include "c4/scala/Scanners.h"

namespace c4s {

void Scanner::init() {
  nextChar();
  // TODO:
  //nextToken();
}

void Scanner::fetchToken() {
  // TODO:
}

void Scanner::nextToken() {
  Token lastToken = token;

  // TODO: sepRegions

  // Read a token or copy it from 'next' tokenData
  if (next->token == Token::T_EMPTY) {
    // TODO:
  }
}

} // namespace
