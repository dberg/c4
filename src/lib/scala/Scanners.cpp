#include "c4/scala/Scanners.h"
#include "c4/scala/Global.h"
#include "c4/scala/CharArrayReader.h"
#include "c4/scala/SourceFile.h"
#include "c4/scala/Chars.h"
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/Names.h"

namespace c4s {

/** Constructor */
TokenData::TokenData()
  : token(Token::T_EMPTY), offset(0), lastOffset(0), base(0) {}

void Scanner::putChar(c4::Char c) {
  cbuf.push_back(c);
}

/** Constructor */
ScannerData::ScannerData()
  : next(spTokenData(new TokenData)), prev(spTokenData(new TokenData)) {}

/** Constructor */
Scanner::Scanner(Global *global, spSourceFile source)
  : source(source), buf(source->content()),
    reader(spCharArrayReader(new CharArrayReader(source->content()))),
    sData(spScannerData(new ScannerData)),
    tData(spTokenData(new TokenData)),
    global(global) {}

/** Clear buffer and set name and token */
void Scanner::finishNamed(Token idtoken) {
  tData->name = global->names->newTermName(cbuf, 0, cbuf.size());
  cbuf.clear();
  tData->token = idtoken;
  if (idtoken == Token::T_IDENTIFIER) {
    // TODO:
    //auto idx = tData->name.start -kwOffset;
  }
}

bool Scanner::inStringInterpolation() {
  // TODO:
  // sepRegions.nonEmpty && sepRegions.head == STRINGLIT
  return false;
}

void Scanner::init() {
  reader->nextChar();
  nextToken();
}

/** Read next token, filling TokenData fields of Scanner. */
void Scanner::fetchToken() {
  tData->offset = reader->charOffset - 1;
  switch (reader->ch) {
  case ' ':
  case '\t':
  case Chars::CR:
  case Chars::LF:
  case Chars::FF:
    reader->nextChar();
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
    putChar(reader->ch);
    reader->nextChar();
    getIdentRest();
  }
  // TODO:
}

//------------------------------------------------------------------------------
// Identifiers
// -----------------------------------------------------------------------------
void Scanner::getIdentRest() {
  switch (reader->ch) {
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
    putChar(reader->ch);
    reader->nextChar();
    getIdentRest();
  case '_':
    putChar(reader->ch);
    reader->nextChar();
    // TODO:
    //getIdentOrOperatorRest();
  case Chars::SU:
    finishNamed();
  default:
    // TODO:
    //if (Character::isUnicodeIdentifierPart(ch)) {
    //  putChar(ch);
    //  reader->nextChar();
    //  getIdentRest();
    //} else {
      finishNamed();
    //}
  }
}

/** Produce next token, filling TokenData fields of Scanner. */
void Scanner::nextToken() {
  Token lastToken = tData->token;

  // TODO: sepRegions

  // Read a token or copy it from 'next' tokenData
  if (sData->next->token == Token::T_EMPTY) {
    tData->lastOffset = reader->charOffset - 1;
    if (tData->lastOffset > 0 && reader->buf[tData->lastOffset] == '\n' &&
        reader->buf[tData->lastOffset - 1] == '\r') {
      tData->lastOffset--;
    }

    if (inStringInterpolation()) {
      // TODO: fetchStringPart();
    } else {
      fetchToken();
    }

    if (tData->token == Token::T_ERROR) {
      // TODO:
    }
  } else {
    // TODO:
  }
}

/** Constructor */
UnitScanner::UnitScanner(Global *global, spCompilationUnit unit)
  : Scanner(global, unit->source), unit(unit) {}


} // namespace
