#include "Lexer.h"

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

bool isJavaLetter(char c) {
  return (isalpha(c) || c == '$' || c == '_');
}

bool isJavaLetterOrDigit(char c) {
  return (isJavaLetter(c) || isdigit(c));
}

///-----------------------------------------------------------------------------
/// Lexer Class
///-----------------------------------------------------------------------------
void Lexer::saveState(State &state) {
  state.cursor = src->getCursor();
  state.line = src->getLine();
  state.token = curToken;
  state.tokenStr = curTokenStr;
}

void Lexer::restoreState(State &state) {
  src->setCursor(state.cursor);
  src->setLine(state.line);
  curToken = state.token;
  curTokenStr = state.tokenStr;
}

void Lexer::getNextToken() {
  curToken = getToken();
}

int Lexer::getToken() {
  char c = src->getChar();
  if (!c) return TOK_EOF;

  // Skip any space char.
  while (isspace(c)) c = src->getChar();
  if (!c) return TOK_EOF;

  // Annotation and Annotation Type Declarations
  if ('@' == c) return getAnnotationToken();
  if ('.' == c) return getPeriodOrEllipsisToken();
  if ('+' == c) return getPlusOrPlusPlusToken();
  if ('-' == c) return getMinusOrMinusMinusToken();
  if ('=' == c) return getEqualsOrEqualsEqualsToken();
  if (',' == c) return TOK_COMMA;
  if (';' == c) return TOK_SEMICOLON;
  if ('*' == c) return TOK_MUL;
  if ('~' == c) return TOK_OP_TILDE;
  if ('!' == c) return TOK_OP_EXCLAMATION;
  if ('{' == c) return TOK_LCURLY_BRACKET;
  if ('}' == c) return TOK_RCURLY_BRACKET;
  if ('(' == c) return TOK_LPAREN;
  if (')' == c) return TOK_RPAREN;
  if ('[' == c) return TOK_LBRACKET;
  if (']' == c) return TOK_RBRACKET;

  if (isdigit(c)) return getNumberToken(c);

  // Identifier
  if (isJavaLetter(c)) return getTokenIdentifier(c);

  return c;
}

/// Return TOK_ANNOTATION | TOK_ANNOTATION_TYPE_DECLARATION
int Lexer::getAnnotationToken() {
  // We look ahead for the keyword 'interface' and if it's a match we
  // have an annotation type declaration, otherwise it's an annotation.
  // We keep track of our look ahead so we can return to our current
  // buffer position.
  char c = src->getChar();
  int lookahead = 1;

  // Consume whitespaces
  while (isspace(c)) {
    lookahead++;
    c = src->getChar();
  }

  // We hit the end of the buffer. At this point we know it's an error but we
  // return TOK_ANNOTATION so the parser can handle this error.
  if (!c) {
    src->ungetChar(lookahead);
    return TOK_ANNOTATION;
  }

  bool isNextTokenInterface = src->lookaheadInterface(src->getCursor() - 1);
  src->ungetChar(lookahead);

  if (isNextTokenInterface) {
    return TOK_ANNOTATION_TYPE_DECLARATION;
  }

  return TOK_ANNOTATION;
}

int Lexer::getEqualsOrEqualsEqualsToken() {
  // We look 1 char ahead to decided if we have '=='.
  if (src->peekChar() == '=') {
    src->getChar(); // conume '='
    return TOK_OP_EQUALS_EQUALS;
  }

  return TOK_OP_EQUALS;
}

/// Returns one of:
///   TOK_DECIMAL_NUMERAL
///   TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX
///   TOK_HEX_NUMERAL
///   TOK_HEX_NUMERAL_WITH_INT_TYPE_SUFFIX
///   TOK_OCTAL_NUMERAL
///   TOK_OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX
///   TOK_BINARY_NUMERAL
///   TOK_BINARY_NUMERAL_WITH_INT_TYPE_SUFFIX
///   TOK_ERROR
/// TODO: floating point
int Lexer::getNumberToken(char c) {
  std::stringstream ss; ss << c;

  // If char c is zero we expect one of:
  // 1. Decimal numeral represented by zero only;
  // 2. HexNumeral;
  // 3. BinaryNumeral;
  // 4. OctalNumeral;
  // Followed by an optional integer suffix.
  if (c == '0') {
    // Decimal numeral
    char lookahead = src->getChar();
    if (lookahead == 'l' || lookahead == 'L') {
      ss << lookahead;
      curTokenStr = ss.str();
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
    curTokenStr = "0";
    return TOK_DECIMAL_NUMERAL;
  }

  return getDecimalNumeral(ss);
}

/// Return TOK_PERIOD | TOK_ELLIPSIS
int Lexer::getPeriodOrEllipsisToken() {
  // We look 2 chars ahead to decide if we have an ellipsis.
  // At this point we have already found one dot char and the
  // cursor is pointing to the next char.
  // .??
  //  ^
  //  cursor
  if (src->getCursor() + 1 <= src->getStreamLength()
      && src->peekChar() == '.'
      && src->peekChar(1) == '.') {
    src->getChar(); // consume 2nd '.'
    src->getChar(); // consume 3rd '.'
    return TOK_ELLIPSIS;
  }

  return TOK_PERIOD;
}

int Lexer::getPlusOrPlusPlusToken() {
  // We look 1 char ahead to decided if we have '++'.
  if (src->peekChar() == '+') {
    src->getChar();
    return TOK_OP_PLUS_PLUS;
  }

  return TOK_OP_PLUS;
}

int Lexer::getMinusOrMinusMinusToken() {
  // We look 1 char ahead to decided if we have '--'.
  if (src->peekChar() == '-') {
    src->getChar();
    return TOK_OP_MINUS_MINUS;
  }

  return TOK_OP_MINUS;
}


/// Return TOK_IDENTIFIER | TOK_INTEGER_TYPE_SUFFIX | TOK_KEY_*
int Lexer::getTokenIdentifier(char c) {
  std::stringstream ss; ss << c;
  while ((c = src->getChar())) {
    if (isJavaLetterOrDigit(c)) {
      ss << c;
    } else {
      src->ungetChar(1);
      break;
    }
  }

  curTokenStr = ss.str();

  // If keyword return the matching token
  if (int keywordToken = tokenUtil.getKeywordToken(curTokenStr)) {
    return keywordToken;
  }

  // 1234L or 1234l
  // TODO: This is the wrong approach as it allows the invalid forms as
  // 1234   L
  if (curToken == TOK_DECIMAL_NUMERAL
    && (curTokenStr.compare("l") == 0 || curTokenStr.compare("L") == 0)) {
    return TOK_INTEGER_TYPE_SUFFIX;
  }

  // BooleanLiteral
  if (curTokenStr.compare("true") == 0 || curTokenStr.compare("false") == 0) {
    return TOK_BOOLEAN_LITERAL;
  }

  // NullLiteral
  if (curTokenStr.compare("null") == 0) {
    return TOK_NULL_LITERAL;
  }

  return TOK_IDENTIFIER;
}

/// Integer Literal
/// ----------------------------------------------------------------------------

/// The string stream contains the value '0b' or '0B'.
/// Returns TOK_BINARY_NUMERAL | TOK_BINARY_NUMERAL_WITH_INT_TYPE_SUFFIX |
/// TOK_ERROR.
int Lexer::getBinaryNumeral(std::stringstream &ss) {
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
int Lexer::getDecimalNumeral(std::stringstream &ss) {
  return consumeIntegerLiteral(ss, isDecimalDigit,
    TOK_DECIMAL_NUMERAL, TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX);
}

/// The string stream contains the value '0x' or '0X'.
/// Returns TOK_HEX_NUMERAL | TOK_HEX_NUMERAL_WITH_INT_TYPE_SUFFIX |
/// TOK_ERROR
int Lexer::getHexNumeral(std::stringstream &ss) {
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
int Lexer::getOctalNumeral(std::stringstream &ss) {
  return consumeIntegerLiteral(ss, isOctalDigit,
    TOK_OCTAL_NUMERAL, TOK_OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX);
}

// Helpers
/// We consume an IntegerLiteral into the string stream ss. Each character
/// is valid if it's equal too '_' or if the predicate function fnDigitP
/// returns true. For each IntegerLiteral class there are two possible tokens.
int Lexer::consumeIntegerLiteral(std::stringstream &ss,
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
    curTokenStr = ss.str();
    return tok;
  }

  // Check int type suffix
  if (c == 'l' || c == 'L') {
    ss << c;
    curTokenStr = ss.str();
    return tokWithSuffix;
  }

  // We have a decimal number but we have to unget the last call to getChar
  // before exiting.
  src->ungetChar(1);
  curTokenStr = ss.str();
  return tok;
}


}; // namespace
