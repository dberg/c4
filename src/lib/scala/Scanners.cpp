#include "c4/scala/Scanners.h"

namespace c4s {

void Scanner::putChar(c4::Char c) {
  cbuf.push_back(c);
}

void Scanner::finishNamed(Token idToken) {

}

bool Scanner::inStringInterpolation() {
  // TODO:
  // sepRegions.nonEmpty && sepRegions.head == STRINGLIT
  return false;
}

void Scanner::init() {
  nextChar();
  nextToken();
}

/** Read next token, filling TokenData fields of Scanner. */
void Scanner::fetchToken() {
  offset = charOffset - 1;
  switch (offset) {
  case ' ':
  case '\t':
  case Chars::CR:
  case Chars::LF:
  case Chars::FF:
    nextChar();
    fetchToken();
  case 'A': case 'B': case 'C': case 'D': case 'E':
  case 'F': case 'G': case 'H': case 'I': case 'J':
  case 'K': case 'L': case 'M': case 'N': case 'O':
  case 'P': case 'Q': case 'R': case 'S': case 'T':
  case 'U': case 'V': case 'W': case 'X': case 'Y':
  case 'Z': case '$': case '_': case 'a': case 'b':
  case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l':
  case 'm': case 'n': case 'o': case 'p': case 'q':
  case 'r': case 's': case 't': case 'u': case 'v':
  case 'w': case 'x': case 'y': case 'z':
    putChar(ch);
    nextChar();
    getIdentRest();
  }
  // TODO:
}

//------------------------------------------------------------------------------
// Identifiers
// -----------------------------------------------------------------------------
void Scanner::getIdentRest() {
  switch (ch) {
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
  case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
  case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
  case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
  case 'Y': case 'Z': case '$': case 'a': case 'b': case 'c':
  case 'd': case 'e': case 'f': case 'g': case 'h': case 'i':
  case 'j': case 'k': case 'l': case 'm': case 'n': case 'o':
  case 'p': case 'q': case 'r': case 's': case 't': case 'u':
  case 'v': case 'w': case 'x': case 'y': case 'z': case '0':
  case '1': case '2': case '3': case '4': case '5': case '6':
  case '7': case '8': case '9':
    putChar(ch);
    nextChar();
    getIdentRest();
  case '_':
    putChar(ch);
    nextChar();
    // TODO:
    //getIdentOrOperatorRest();
  case Chars::SU:
    finishNamed();
  default:
    // TODO:
    //if (Character::isUnicodeIdentifierPart(ch)) {
    //  putChar(ch);
    //  nextChar();
    //  getIdentRest();
    //} else {
      finishNamed();
    //}
  }
}

/** Produce next token, filling TokenData fields of Scanner. */
void Scanner::nextToken() {
  Token lastToken = token;

  // TODO: sepRegions

  // Read a token or copy it from 'next' tokenData
  if (next->token == Token::T_EMPTY) {
    lastOffset = charOffset - 1;
    if (lastOffset > 0 && buf[lastOffset] == '\n' && buf[lastOffset - 1] == '\r') {
      lastOffset--;
    }

    if (inStringInterpolation()) {
      // TODO: fetchStringPart();
    } else {
      fetchToken();
    }

    if (token == Token::T_ERROR) {
      // TODO:
    }
  } else {
    // TODO:
  }
}

} // namespace
