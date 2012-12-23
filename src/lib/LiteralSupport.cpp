#include "LiteralSupport.h"

namespace djp {

///-----------------------------------------------------------------------------
/// Helpers
///-----------------------------------------------------------------------------
bool isBinaryDigit(char c) {
  return (c == '0' || c == '1');
}

bool isBinaryExponentIndicator(char c) {
  return (c == 'p' || c == 'P');
}

bool isDecimalDigit(char c) {
  return isdigit(c);
}

bool isFloatTypeSuffix(char c) {
  return (c == 'f' || c == 'F' || c == 'd' || c == 'D');
}

bool isFPExponentIndicator(char c) {
  return (c == 'e' || c == 'E');
}

bool isHexDigit(char c) {
  if (isdigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
    return true;
  }

  return false;
}

bool isIntegerTypeSuffix(char c) {
  return (c == 'l' || c == 'L');
}

bool isOctalDigit(char c) {
  return (c >= '0' && c <= '7');
}

bool isSign(char c) {
  return (c == '+' || c == '-');
}

///-----------------------------------------------------------------------------
/// LiteralSupport Class
///-----------------------------------------------------------------------------
int LiteralSupport::getLiteralNumber(char c, std::stringstream &ss) {
  if (c == '0') {
    return getTokWithLeadingZero(ss);
  }

  ss << c;
  return getDecimalNumeralOrDecimalFloatingPoint(c, ss);
}

/// We handle the special case of the production rule:
/// Digits ExponentPart(opt) FloatTypeSuffix(opt)
/// The strings tream already have '.' at this point and we know by
/// previously peeking ahead that we have at least one digit.
int LiteralSupport::getDecimalFloatingPointStartingWithAPeriod(
  std::stringstream &ss) {

  // Consume all digits
  getDecimalNumeral(ss);

  // ExponentPart
  if (isFPExponentIndicator(src->peekChar())) {
    consumeExponentPart(ss);
  }

  // FloatTypeSuffix
  if (isFloatTypeSuffix(src->peekChar())) {
    ss << src->getChar(); // consume suffix
  }

  return TOK_DECIMAL_FLOATING_POINT_LITERAL;
}

/// We identify a DecimalLiteral and the possibility of promoting it to a
/// FloatingPointLiteral. We leave the floating point production rule that
/// starts with a period to be parsed by its own method.
/// See getDecimalFloatingPointStartingWithAPeriod.
int LiteralSupport::getDecimalNumeralOrDecimalFloatingPoint(
  char previous_c, std::stringstream &ss) {

  if (!isdigit(previous_c)) {
    return TOK_ERROR;
  }

  int tok = getDecimalNumeral(ss);

  if (tok == TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX) {
    return tok;
  }

  // We look ahead and check the possibility of a floating point
  char peek = src->peekChar();
  bool isFloatIndicator
    = (peek == '.' || isFPExponentIndicator(peek) || isFloatTypeSuffix(peek));

  if (!isFloatIndicator) {
    // We have a TOK_DECIMAL_NUMERAL
    return tok;
  }

  // We've seen digits and there's a possibility of a floating point;
  // unless there's a syntatic error.
  bool seenPeriod = false;
  bool seenExponentPart = false;

  if (peek == '.') {
    seenPeriod = true;
    ss << src->getChar(); // consume '.'
    peek = src->peekChar();

    // Digits
    if (isdigit(peek)) {
      getDecimalNumeral(ss);
      peek = src->peekChar();
    }
  }

  // ExponentPart
  if (isFPExponentIndicator(peek)) {
    seenExponentPart = true;
    consumeExponentPart(ss);
    peek = src->peekChar();
  }

  // FloatTypeSuffix
  if (isFloatTypeSuffix(peek)) {
    ss << src->getChar(); // consume suffix
    return TOK_DECIMAL_FLOATING_POINT_LITERAL;
  }

  if (seenPeriod == false && seenExponentPart == false) {
    return TOK_ERROR;
  }

  return TOK_DECIMAL_FLOATING_POINT_LITERAL;
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

  // Append bin digit we've just consumed
  ss << c;

  // Consume remaining digits
  consumeDigitsPOrUnderscores(ss, isBinaryDigit);

  // Check int type suffix
  char peek = src->peekChar();
  if (isIntegerTypeSuffix(peek)) {
    ss << src->getChar(); // append and consume suffix
    return TOK_BINARY_NUMERAL_WITH_INT_TYPE_SUFFIX;
  }

  return TOK_BINARY_NUMERAL;
}

/// Returns TOK_DECIMAL_NUMERAL | TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX
int LiteralSupport::getDecimalNumeral(std::stringstream &ss) {
  consumeDigitsPOrUnderscores(ss, isDecimalDigit);

  // Check int type suffix
  char peek = src->peekChar();
  if (isIntegerTypeSuffix(peek)) {
    ss << src->getChar(); // append and consume suffix
    return TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX;
  }

  return TOK_DECIMAL_NUMERAL;
}

/// The string stream contains the value '0x' or '0X'.
/// Returns
/// TOK_HEX_NUMERAL
/// TOK_HEX_NUMERAL_WITH_INT_TYPE_SUFFIX
/// TOK_HEXADECIMAL_FLOATING_POINT_LITERAL
/// TOK_ERROR
int LiteralSupport::getHexNumeral(std::stringstream &ss) {
  // We save the start position of the numeral for error diagnosis.
  int start = src->getCursor() - 2;

  // Lookahead and confirm that we have valid hex digit.
  if (!(isHexDigit(src->peekChar()) || src->peekChar() == '.')) {
    diag->addErr(ERR_NVAL_HEX, start, src->getCursor());
    return TOK_ERROR;
  }

  bool seenPeriod = false;

  if (src->peekChar() == '.') {
    if (!isHexDigit(src->peekChar(1))) {
      src->ungetChar(2);
      diag->addErr(ERR_NVAL_HEX, start, src->getCursor());
      return TOK_ERROR;
    }

    ss << src->getChar(); // consume '.'
    seenPeriod = true;
  }

  // Consume whole or fractional digits
  consumeDigitsPOrUnderscores(ss, isHexDigit);

  if (!seenPeriod) {
    if (src->peekChar() == '.') {
      ss << src->getChar(); // consume '.'
      seenPeriod = true;
    }

    // Consume fractional digits
    consumeDigitsPOrUnderscores(ss, isHexDigit);
  }

  // If we didn't see '.' and the next char is not a binary exponent indicator
  // we know that this is an integer.
  if (!seenPeriod && !isBinaryExponentIndicator(src->peekChar())) {
    // Check int type suffix
    char peek = src->peekChar();
    if (isIntegerTypeSuffix(peek)) {
      ss << src->getChar(); // append and consume suffix
      return TOK_HEX_NUMERAL_WITH_INT_TYPE_SUFFIX;
    }

    return TOK_HEX_NUMERAL;
  }

  // We have a floating point.
  // The binary exponent indicator is mandatory.
  if (!isBinaryExponentIndicator(src->peekChar())) {
    diag->addErr(ERR_NVAL_HEX, start, src->getCursor());
    return TOK_ERROR;
  }

  // Consume the binary exponent indicator: 'p' or 'P'
  ss << src->getChar();

  // Sign(opt)
  if (isSign(src->peekChar())) {
    ss << src->getChar(); // consume '+' or '-'
  }

  // Digits
  int digitCount = consumeDigitsPOrUnderscores(ss, isDecimalDigit);
  if (digitCount <= 0) {
    // Invalid or missing Signed integer
    diag->addErr(ERR_NVAL_HEX, start, src->getCursor());
    return TOK_ERROR;
  }

  // FloatTypeSuffix(opt)
  if (isFloatTypeSuffix(src->peekChar())) {
    ss << src->getChar(); // consume one of: 'f', 'F', 'd' or 'D'
  }

  return TOK_HEXADECIMAL_FLOATING_POINT_LITERAL;
}

/// Returns TOK_OCTAL_NUMERAL | TOK_OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX
int LiteralSupport::getOctalNumeral(std::stringstream &ss) {
  consumeDigitsPOrUnderscores(ss, isOctalDigit);

  // Check int type suffix
  char peek = src->peekChar();
  if (isIntegerTypeSuffix(peek)) {
    ss << src->getChar(); // append and consume suffix
    return TOK_OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX;
  }

  return TOK_OCTAL_NUMERAL;
}

// -----------------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------------

/// We consume any digits or underscores while making sure that the end of the
/// stream contains a digit.
/// Returns the count of chars consumed.
int LiteralSupport::consumeDigitsPOrUnderscores(
  std::stringstream &ss, bool (*fnDigitP) (char)) {

  char c;
  std::stringstream stack;
  src->mark();

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
    return src->getMarkOffset();
  }

  src->ungetChar(1);
  return src->getMarkOffset();
}

/// ExponentPart: ExponentIndicator SignedInteger
/// ExponentIndicator: one of e E
/// SignedInteger: Sign(opt) Digits
/// Sign: one of + -
/// Returns the count of chars consumed.
int LiteralSupport::consumeExponentPart(std::stringstream &ss) {
  src->mark();
  ss << src->getChar(); // consume ExponentIndicator

  if (isSign(src->peekChar())) {
    ss << src->getChar(); // consume Sign: '+' or '-'
  }

  int digitCount = consumeDigitsPOrUnderscores(ss, isDecimalDigit);
  if (digitCount > 0) {
    return digitCount;
  }

  // Error, we consumed the exponent indicator, possibly the sign but no digit
  // was consumed. We restore the source buffer and let the parser handle the
  // error.
  src->restore();
  return 0;
}

int LiteralSupport::getTokWithLeadingZero(std::stringstream &ss) {
  // Decimal numeral
  char lookahead = src->getChar();
  if (lookahead == 'l' || lookahead == 'L') {
    ss << '0'; ss << lookahead;
    return TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX;
  }

  // Hex numeral
  if (lookahead == 'x' || lookahead == 'X') {
    ss << '0'; ss << lookahead;
    return getHexNumeral(ss);
  }

  // Binary numeral
  if (lookahead == 'b' || lookahead == 'B') {
    ss << '0'; ss << lookahead;
    return getBinaryNumeral(ss);
  }

  // Octal
  if (isdigit(lookahead) || lookahead == '_') {
    ss << '0'; ss << lookahead;
    return getOctalNumeral(ss);
  }

  // Float and Double
  if (isFloatTypeSuffix(lookahead)) {
    ss << '0'; ss << lookahead;
    return TOK_DECIMAL_FLOATING_POINT_LITERAL;
  }

  // Decimal numeral, stand alone zero
  if (!isDecimalDigit(lookahead) && lookahead != '.') {
    src->ungetChar(1);
    ss << '0';
    return TOK_DECIMAL_NUMERAL;
  }

  // We might have a floating point number or a series of zeros so we let
  // getDecimalNumberalOrDecimalFloatingPoint resolve it after we backtrack to
  // the initial zero char.
  src->ungetChar(2);
  return getDecimalNumeralOrDecimalFloatingPoint('0', ss);
}
} // namespace
