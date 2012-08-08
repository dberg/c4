#include "Lexer.h"

namespace djp {

/// Return the next char in the buffer or '\0' if we hit the end of the buffer.
const char Lexer::getChar() {
  if (cursor > buffer.length()) {
    return '\0';
  }

  if (buffer[cursor] == '\n') {
    line++;
  }

  return buffer[cursor++];
}

const char Lexer::ungetChar(int count) {
  cursor -= count;
  return buffer[cursor];
}


/// We check if the next token is the keyword 'interface'.
/// We assume that any whitespace has been previously consumed.
bool Lexer::lookaheadInterface(int point) {
  std::string result = std::string(buffer, point, 9);
  if (result == "interface") return true;
  return false;
}

void Lexer::saveState(State &state) {
  state.cursor = cursor;
  state.line = line;
  state.token = curToken;
  state.tokenStr = curTokenStr;
}

void Lexer::restoreState(State &state) {
  cursor = state.cursor;
  line = state.line;
  curToken = state.token;
  curTokenStr = state.tokenStr;
}

void Lexer::getNextToken() {
  curToken = getToken();
}

int Lexer::getToken() {
  char c = getChar();
  if (!c) return TOK_EOF;

  // Skip any space char.
  while (isspace(c)) c = getChar();
  if (!c) return TOK_EOF;

  // Annotation and Annotation Type Declarations
  if ('@' == c) return getAnnotationToken();
  if ('.' == c) return getPeriodOrEllipsisToken();
  if ('+' == c) return getPlusOrPlusPlusToken();
  if ('-' == c) return getMinusOrMinusMinusToken();
  if ('=' == c) return getEqualsOrEqualsEqualsToken();
  if (',' == c) return TOK_COMMA;
  if (';' == c) return TOK_SEMICOLON;
  if ('*' == c) return TOK_ASTERISK;
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
  char c = getChar();
  int lookahead = 1;

  // Consume whitespaces
  while (isspace(c)) {
    lookahead++;
    c = getChar();
  }

  // We hit the end of the buffer. At this point we know it's an error but we
  // return TOK_ANNOTATION so the parser can handle this error.
  if (!c) {
    ungetChar(lookahead);
    return TOK_ANNOTATION;
  }

  bool isNextTokenInterface = lookaheadInterface(cursor - 1);
  ungetChar(lookahead);

  if (isNextTokenInterface) {
    return TOK_ANNOTATION_TYPE_DECLARATION;
  }

  return TOK_ANNOTATION;
}

int Lexer::getEqualsOrEqualsEqualsToken() {
  // We look 1 char ahead to decided if we have '=='.
  if (buffer[cursor] == '=') {
    cursor++;
    return TOK_EQUALS_EQUALS;
  }

  return TOK_EQUALS;
}

/// Returns one of:
///   TOK_DECIMAL_NUMERAL
///   TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX
/// DecimalIntegerLiteral: DecimalNumeral [IntegerTypeSuffix]
/// DecimalNumeral:
///   0 | NonZeroDigit [Digits] | NonZeroDigit Underscores Digits
/// IntegerTypeSuffix: l | L
int Lexer::getNumberToken(char c) {
  std::stringstream ss; ss << c;

  // We look ahead to decide if we have 0 (zero)
  if (c == '0') {
    // Check if it's zero followed by an integer type suffix
    char lookahead = getChar();
    if (lookahead == 'l' || lookahead == 'L') {
      ss << c;
      return TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX;
    }

    ungetChar(1);
    return TOK_DECIMAL_NUMERAL;
  }

  // While we have a digit or an underscore char we append it to ss.
  std::stringstream ss_;
  while ((c = getChar()) && (isdigit(c) || c == '_')) {
    if (c == '_') {
      ss_ << c;
    } else {
      // pop underscore stream into decimal numeral stream and reset it
      ss << ss_.str();
      ss_.str("");
      ss_.clear();
      // insert digit into stream
      ss << c;
    }
  }

  // Unget underscore chars if any.
  std::string underscores = ss_.str();
  if (underscores.length()) {
    ungetChar(underscores.length());
    // The syntax at this point is invalid but we return the valid number
    // we have so far and let the consumer validate the next token.
    curTokenStr = ss.str();
    return TOK_DECIMAL_NUMERAL;
  }

  // Check int type suffix
  if (c == 'l' || c == 'L') {
    ss << c;
    curTokenStr = ss.str();
    return TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX;
  }

  // We have a decimal number but we have to unget the last call to getChar
  // before exiting.
  ungetChar(1);
  curTokenStr = ss.str();
  return TOK_DECIMAL_NUMERAL;
}

/// Return TOK_PERIOD | TOK_ELLIPSIS
int Lexer::getPeriodOrEllipsisToken() {
  // We look 2 chars ahead to decide if we have an ellipsis.
  // At this point we have already found one dot char and the
  // cursor is pointing to the next char.
  // .??
  //  ^
  //  cursor
  if (cursor + 1 <= buffer.length()
    && buffer[cursor] == '.'
    && buffer[cursor+1] == '.') {
    cursor = cursor + 2;
    return TOK_ELLIPSIS;
  }

  return TOK_PERIOD;
}

int Lexer::getPlusOrPlusPlusToken() {
  // We look 1 char ahead to decided if we have '++'.
  if (buffer[cursor] == '+') {
    cursor++;
    return TOK_OP_PLUS_PLUS;
  }

  return TOK_OP_PLUS;
}

int Lexer::getMinusOrMinusMinusToken() {
  // We look 1 char ahead to decided if we have '--'.
  if (buffer[cursor] == '-') {
    cursor++;
    return TOK_OP_MINUS_MINUS;
  }

  return TOK_OP_MINUS;
}


/// Return TOK_IDENTIFIER | TOK_INTEGER_TYPE_SUFFIX | TOK_KEY_*
int Lexer::getTokenIdentifier(char c) {
  std::stringstream ss; ss << c;
  while ((c = getChar())) {
    if (isJavaLetterOrDigit(c)) {
      ss << c;
    } else {
      ungetChar(1);
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

// Helpers
bool Lexer::isJavaLetter(char c) {
  return (isalpha(c) || c == '$' || c == '_');
}

bool Lexer::isJavaLetterOrDigit(char c) {
  return (isJavaLetter(c) || isdigit(c));
}

}; // namespace
