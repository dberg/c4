#include "LiteralSupport.h"

namespace djp {

///-----------------------------------------------------------------------------
/// Helpers
///-----------------------------------------------------------------------------
bool isBinaryDigit(char c) {
  return (c == '0' || c == '1');
}

bool isDecimalDigit(char c) {
  return isdigit(c);
}

bool isHexDigit(char c) {
  if (isdigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
    return true;
  }

  return false;
}

bool isOctalDigit(char c) {
  return (c >= '0' && c <= '7');
}

///-----------------------------------------------------------------------------
/// LiteralSupport Class
///-----------------------------------------------------------------------------
int LiteralSupport::getLiteralNumber(char c, std::stringstream &ss) {
  ss << c;

  // If char c is zero we expect one of:
  // 1. Decimal numeral represented by zero only;
  // 2. HexNumeral;
  // 3. BinaryNumeral;
  // 4. OctalNumeral;
  // Followed by an optional integer suffix.
  if (c == '0') {
    return getTokWithLeadingZero(ss);
  }

  return getDecimalNumeral(ss);
}

/// ----------------------------------------------------------------------------
/// Integer Literal
/// ----------------------------------------------------------------------------

/// The string stream contains the value '0b' or '0B'.
/// Returns TOK_BINARY_NUMERAL | TOK_BINARY_NUMERAL_WITH_INT_TYPE_SUFFIX |
/// TOK_ERROR.
int LiteralSupport::getBinaryNumeral(std::stringstream &ss) {
  // Lookahead and confirm that we have valid binary digit.
  char c = src->getChar();
  if (!isBinaryDigit(c)) {
    src->ungetChar(3);
    return TOK_ERROR;
  }

  ss << c; // append bin digit

  return consumeIntegerLiteral(ss, isBinaryDigit,
    TOK_BINARY_NUMERAL, TOK_BINARY_NUMERAL_WITH_INT_TYPE_SUFFIX);
}

/// Returns TOK_DECIMAL_NUMERAL | TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX
int LiteralSupport::getDecimalNumeral(std::stringstream &ss) {
  return consumeIntegerLiteral(ss, isDecimalDigit,
    TOK_DECIMAL_NUMERAL, TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX);
}

/// The string stream contains the value '0x' or '0X'.
/// Returns TOK_HEX_NUMERAL | TOK_HEX_NUMERAL_WITH_INT_TYPE_SUFFIX |
/// TOK_ERROR
int LiteralSupport::getHexNumeral(std::stringstream &ss) {
  // Lookahead and confirm that we have valid hex digit.
  char c = src->getChar();
  if (!isHexDigit(c)) {
    src->ungetChar(3);
    return TOK_ERROR;
  }

  ss << c; // append bin digit

  return consumeIntegerLiteral(ss, isHexDigit,
    TOK_HEX_NUMERAL, TOK_HEX_NUMERAL_WITH_INT_TYPE_SUFFIX);
}

/// Returns TOK_OCTAL_NUMERAL | TOK_OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX
int LiteralSupport::getOctalNumeral(std::stringstream &ss) {
  return consumeIntegerLiteral(ss, isOctalDigit,
    TOK_OCTAL_NUMERAL, TOK_OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX);
}

// Helpers
/// We consume an IntegerLiteral into the string stream ss. Each character
/// is valid if it's equal too '_' or if the predicate function fnDigitP
/// returns true. For each IntegerLiteral class there are two possible tokens.
int LiteralSupport::consumeIntegerLiteral(std::stringstream &ss,
  bool (*fnDigitP) (char), int tok, int tokWithSuffix) {

  char c;
  std::stringstream stack;

  while ((c = src->getChar()) && ((fnDigitP)(c) || c == '_')) {
    if (c == '_') {
      stack << c;
    } else {
      // pop underscore stream into integer stream
      ss << stack.str();
      stack.str("");
      stack.clear();
      // insert digit into stream
      ss << c;
    }
  }

  // Unget underscore chars if any.
  std::string underscores = stack.str();
  if (underscores.length()) {
    src->ungetChar(underscores.length());
    // The syntax at this point is invalid but we return the valid number
    // we have so far and let the consumer validate the next token.
    return tok;
  }

  // Check int type suffix
  if (c == 'l' || c == 'L') {
    ss << c;
    return tokWithSuffix;
  }

  // We have a decimal number but we have to unget the last call to getChar
  // before exiting.
  src->ungetChar(1);
  return tok;
}

int LiteralSupport::getTokWithLeadingZero(std::stringstream &ss) {
  // Decimal numeral
  char lookahead = src->getChar();
  if (lookahead == 'l' || lookahead == 'L') {
    ss << lookahead;
    return TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX;
  }

  // Hex numeral
  if (lookahead == 'x' || lookahead == 'X') {
    ss << lookahead;
    return getHexNumeral(ss);
  }

  // Binary numeral
  if (lookahead == 'b' || lookahead == 'B') {
    ss << lookahead;
    return getBinaryNumeral(ss);
  }

  // Octal
  if (isdigit(lookahead) || lookahead == '_') {
    ss << lookahead;
    return getOctalNumeral(ss);
  }

  // Decimal numeral, stand alone zero
  src->ungetChar(1);
  return TOK_DECIMAL_NUMERAL;
}

} // namespace
