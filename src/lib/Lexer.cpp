#include "Lexer.h"

namespace djp {

///-----------------------------------------------------------------------------
/// Helpers
///-----------------------------------------------------------------------------
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
  if ('.' == c) return getPeriodStartingToken();
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
  std::stringstream ss;
  int tok = litSupport->getLiteralNumber(c, ss);
  curTokenStr = ss.str();
  return tok;
}

/// Return TOK_PERIOD | TOK_ELLIPS | TOK_DECIMAL_FLOATING_POINT_LITERAL
int Lexer::getPeriodStartingToken() {
  std::stringstream ss;
  ss << '.';

  // If we have a digit following '.' this is a decimal floating point literal.
  if (isdigit(src->peekChar())) {
    int tok = litSupport->getDecimalFloatingPointStartingWithAPeriod(ss);
    curTokenStr = ss.str();
    return tok;
  }

  int tok = getPeriodOrEllipsisToken(ss);
  curTokenStr = ss.str();
  return tok;
}

/// Return TOK_PERIOD | TOK_ELLIPSIS
int Lexer::getPeriodOrEllipsisToken(std::stringstream &ss) {
  // We look 2 chars ahead to decide if we have an ellipsis.
  // At this point we have already found one dot char and the
  // cursor is pointing to the next char.
  // .??
  //  ^
  //  cursor
  if (src->getCursor() + 1 <= src->getStreamLength()
      && src->peekChar() == '.'
      && src->peekChar(1) == '.') {
    ss << src->getChar(); // consume 2nd '.'
    ss << src->getChar(); // consume 3rd '.'
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
} // namespace
