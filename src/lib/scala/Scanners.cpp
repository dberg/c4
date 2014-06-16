#include "c4/scala/Scanners.h"

namespace c4s {

void Scanner::nextToken() {
  Token lastToken = token;

  // TODO: sepRegions

  // Read a token or copy it from 'next' tokenData
  if (next.token == EMPTY) {
    // TODO:
  }
}

} // namespace
