#include <algorithm>
#include "c4/scala/Scanners.h"
#include "c4/scala/Global.h"
#include "c4/scala/CharArrayReader.h"
#include "c4/scala/SourceFile.h"
#include "c4/scala/Chars.h"
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/Names.h"
#include "c4/scala/StdNames.h"

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
Scanner::Scanner(Global *global, SourceFile *source)
  : reader(new CharArrayReader(source->content())),
    kwOffset(0),
    global(global),
    sData(spScannerData(new ScannerData)),
    tData(spTokenData(new TokenData)),
    source(source), buf(source->content())
{
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->ABSTRACTkw, Token::T_ABSTRACT));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->CASEkw, Token::T_CASE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->CATCHkw, Token::T_CATCH));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->CLASSkw, Token::T_CLASS));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->DEFkw, Token::T_DEF));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->DOkw, Token::T_DO));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->ELSEkw, Token::T_ELSE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->EXTENDSkw, Token::T_EXTENDS));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->FALSEkw, Token::T_FALSE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->FINALkw, Token::T_FINAL));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->FINALLYkw, Token::T_FINALLY));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->FORkw, Token::T_FOR));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->FORSOMEkw, Token::T_FORSOME));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->IFkw, Token::T_IF));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->IMPLICITkw, Token::T_IMPLICIT));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->IMPORTkw, Token::T_IMPORT));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->LAZYkw, Token::T_LAZY));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->MATCHkw, Token::T_MATCH));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->NEWkw, Token::T_NEW));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->NULLkw, Token::T_NULL));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->OBJECTkw, Token::T_OBJECT));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->OVERRIDEkw, Token::T_OVERRIDE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->PACKAGEkw, Token::T_PACKAGE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->PRIVATEkw, Token::T_PRIVATE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->PROTECTEDkw, Token::T_PROTECTED));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->RETURNkw, Token::T_RETURN));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->SEALEDkw, Token::T_SEALED));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->SUPERkw, Token::T_SUPER));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->THISkw, Token::T_THIS));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->THROWkw, Token::T_THROW));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->TRAITkw, Token::T_TRAIT));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->TRUEkw, Token::T_TRUE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->TRYkw, Token::T_TRY));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->TYPEkw, Token::T_TYPE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->VALkw, Token::T_VAL));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->VARkw, Token::T_VAR));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->WHILEkw, Token::T_WHILE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->WITHkw, Token::T_WITH));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->YIELDkw, Token::T_YIELD));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->DOTkw, Token::T_DOT));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->USCOREkw, Token::T_USCORE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->COLONkw, Token::T_COLON));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->EQUALSkw, Token::T_EQUALS));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->ARROWkw, Token::T_ARROW));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->LARROWkw, Token::T_LARROW));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->SUBTYPEkw, Token::T_SUBTYPE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->VIEWBOUNDkw, Token::T_VIEWBOUND));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->SUPERTYPEkw, Token::T_SUPERTYPE));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->HASHkw, Token::T_HASH));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->ATkw, Token::T_AT));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->MACROkw, Token::T_IDENTIFIER));
  allKeywords.push_back(std::make_pair(
    global->stdNames->keywords->THENkw, Token::T_IDENTIFIER));

  auto pair = createKeywordArray(allKeywords, Token::T_IDENTIFIER);
  kwOffset = (int) pair.first;
  kwArray = pair.second;
}

/** Destructor */
Scanner::~Scanner() {
  delete reader;
}

/** @returns (lowest Name.start(), vector of Tokens) */
std::pair<Token, std::vector<Token>> Scanner::createKeywordArray(
  std::vector<std::pair<Name *, Token>> keywords, Token defaultToken) {

  // Sort the keywords by Name.start()
  sort(keywords.begin(), keywords.end(),
    [] (const std::pair<Name *, Token> &p1, const std::pair<Name *, Token> &p2) {
      return p1.first->start() < p2.first->start();
    }
  );

  // name->start(), Token
  std::vector<std::pair<int, Token>> names;
  for (auto pair: keywords) {
    names.push_back(std::make_pair(pair.first->start(), pair.second));
  }

  int low = names[0].first;
  int high = names.empty() ? 0 : names.back().first;

  int size = high - low + 1;
  std::vector<Token> arr(size, defaultToken);
  for (auto pair: names) {
    arr[pair.first + low] = pair.second;
  }

  return std::make_pair((Token) low, arr);
}

/** Clear buffer and set name and token */
void Scanner::finishNamed(Token idtoken) {
  tData->name = global->names->newTermName(cbuf, 0, cbuf.size());
  cbuf.clear();
  tData->token = idtoken;
  if (idtoken == Token::T_IDENTIFIER) {
    int idx = tData->name->start() - kwOffset;
    if (idx >= 0 && idx < (int) kwArray.size()) {
      tData->token = kwArray[idx];
      //if (token == Token::T_IDENTIFIER && allowIdent != name) {
      // TODO:
      //}
    }
  }
}

bool Scanner::inStringInterpolation() {
  // TODO:
  // sepRegions.nonEmpty && sepRegions.head == STRINGLIT
  return false;
}

/** Read next token and return last offset. */
Offset Scanner::skipToken() {
  Offset off = tData->offset;
  nextToken();
  return off;
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
    break;
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
    break;
  }
  // TODO:
}

void Scanner::init() {
  reader->nextChar();
  nextToken();
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
    break;
  case '_':
    putChar(reader->ch);
    reader->nextChar();
    // TODO:
    //getIdentOrOperatorRest();
    break;
  case Chars::SU:
    finishNamed();
    break;
  default:
    // TODO:
    //if (Character::isUnicodeIdentifierPart(ch)) {
    //  putChar(ch);
    //  reader->nextChar();
    //  getIdentRest();
    //} else {
      finishNamed();
      break;
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

  // Insert NEWLINE or NEWLINES if
  // - we are after a newline
  // - we are within a { ... } or on toplevel (wrt sepRegions)
  // - the current token can start a statement and the one before can end it
  // insert NEWLINES if we are past a blank line, NEWLINE otherwise
  // TODO:

  // Join CASE + CLASS => CASECLASS, CASE + OBJECT => CASEOBJECT, SEMI + ELSE => ELSE
  // TODO:
}

/** Constructor */
UnitScanner::UnitScanner(Global *global, CompilationUnit *unit)
  : Scanner(global, unit->source), unit(unit) {}

/** Destructor */
UnitScanner::~UnitScanner() {}

} // namespace
