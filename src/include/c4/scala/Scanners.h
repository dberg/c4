//-*- C++ -*-
#ifndef __C4_SCALA_SCANNERS_H__
#define __C4_SCALA_SCANNERS_H__

#include <string>
#include <vector>
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/CharArrayReader.h"
#include "c4/scala/TypeDefs.h"
#include "c4/scala/Tokens.h"

namespace c4s {

class TokenData {

public:

  /** The next token */
  Token token;

  /** The offset of the first character of the current token */
  Offset offset;

  /** The offset of the character following the token preceding this one */
  Offset lastOffset;

  /** The name of an identifier */
  TermName* name;

  /** The string value of a literal */
  std::string strVal;

  /** The base of a number */
  int base;

  TokenData();
};

class ScannerData {
public:
  TokenData* next;
  TokenData* prev;

  ScannerData();
  ~ScannerData();
};

class Scanner {
private:

  /** A stack of tokens which indicates whether line-ends can be statement
   *  separators. It's also used for keeping track of nesting levels.
   *  We keep track of the closing symbol of a region. This can be
   *  RPAREN    if region starts with '('
   *  RBRACKET  if region starts with '['
   *  RBRACE    if region starts with '{'
   *  ARROW     if region starts with `case'
   *  STRINGLIT if region is a string interpolation expression starting with
   *            '${' (the STRINGLIT appears twice in succession on the stack if
   *             the expression is a multiline string literal).
   */
  std::vector<Token> sepRegions;

  CharArrayReader* reader;

  // Identifiers
  void getIdentRest();
  void finishNamed(Token idtoken = Token::T_IDENTIFIER);

  // Keyword configuration
  std::vector<std::pair<Name*, Token>> allKeywords;
  Offset kwOffset;
  std::vector<Token> kwArray;

  std::pair<Token, std::vector<Token>> createKeywordArray(
    std::vector<std::pair<Name*, Token>> keywords, Token defaultToken);

protected:

  std::vector<c4::Char> cbuf;
  void putChar(c4::Char c);

public:

  Global* global;
  ScannerData* sData;
  TokenData* tData;

  // SourceFile data
  SourceFile* source;
  std::vector<Char> &buf;

  Scanner(Global* global, SourceFile* source);
  virtual ~Scanner();

  //---------------------------------------------------------------------------
  // Get next Token
  //---------------------------------------------------------------------------
  virtual bool inStringInterpolation();
  virtual Offset skipToken();
  virtual void nextToken();
  virtual void fetchToken();

  virtual void init();
};

class UnitScanner : public Scanner {
private:
  CompilationUnit* unit;

public:
  UnitScanner(Global* global, CompilationUnit* unit);
  virtual ~UnitScanner();
};

} // namespace

#endif
