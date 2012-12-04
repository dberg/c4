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
  if ('\'' == c) return getCharacterLiteral();
  if ('"' == c) return getStringLiteral();
  if ('.' == c) return getPeriodStartingToken();
  if ('+' == c) return getPlusToken();
  if ('-' == c) return getMinusToken();
  if ('=' == c) return getEqualsToken();
  if ('/' == c) return getCommentOrDivToken();
  if ('&' == c) return getAmpersandToken();
  if ('|' == c) return getPipeToken();
  if ('^' == c) return getCarretToken();
  if ('%' == c) return getRemToken();
  if ('>' == c) return getGreaterThenToken();
  if ('<' == c) return getLessThenToken();
  if (',' == c) return TOK_COMMA;
  if (';' == c) return TOK_SEMICOLON;
  if (':' == c) return TOK_OP_COLON;
  if ('*' == c) return getMulToken();
  if ('~' == c) return TOK_OP_TILDE;
  if ('!' == c) return getExclamationToken();
  if ('{' == c) return TOK_LCURLY_BRACKET;
  if ('}' == c) return TOK_RCURLY_BRACKET;
  if ('(' == c) return TOK_LPAREN;
  if (')' == c) return TOK_RPAREN;
  if ('[' == c) return TOK_LBRACKET;
  if (']' == c) return TOK_RBRACKET;
  if ('?' == c) return TOK_OP_QUESTION_MARK;

  if (isdigit(c)) return getNumberToken(c);

  // Identifier
  if (isJavaLetter(c)) return getTokenIdentifier(c);

  return c;
}

int Lexer::getAmpersandToken() {
  // We look 1 char ahead to decided if we have '&=' or '&&'
  if (src->peekChar() == '=') {
    src->getChar(); // conume '='
    return TOK_OP_AMPERSAND_EQUALS;
  }

  if (src->peekChar() == '&') {
    src->getChar(); // consume '&'
    return TOK_OP_AMPERSAND_AMPERSAND;
  }

  return TOK_OP_AMPERSAND;
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

int Lexer::getCarretToken() {
  // We look 1 char ahead to decided if we have '^='.
  if (src->peekChar() == '=') {
    src->getChar(); // conume '='
    return TOK_OP_CARRET_EQUALS;
  }

  return TOK_OP_CARRET;
}

int Lexer::getCharacterLiteral() {
  std::stringstream ss;
  ss << '\''; // opening single quote

  // Check for SingleCharacter:
  // InputCharacter but not ' or \.
  char c = src->peekChar();
  if (c != '\'' && c != '\\') {
    ss << src->getChar(); // consume SingleCharacter
    if (src->peekChar() == '\'') {
      ss << src->getChar(); // consume single quote char
      curTokenStr = ss.str();
      return TOK_CHARACTER_LITERAL;
    } else {
      curTokenStr = ss.str();
      return TOK_ERROR;
    }
  }

  // Escape Sequence
  if (c == '\\') {
    int tok = getEscapeSequence(ss);
    if (tok == TOK_ESCAPE_SEQUENCE) {
      curTokenStr = ss.str();
      return TOK_CHARACTER_LITERAL;
    }

    return tok;
  }

  curTokenStr = ss.str();
  return TOK_ERROR;
}

/// A forward slash character indicates a comment, divisor or an assignment
/// operation. Comments are pre-processed but since we do all parsing in one
//// pass we store the comments data in the lexer and call getToken() again.
int Lexer::getCommentOrDivToken() {
  // We peek 1 char ahead to confirm it's a comment,
  // and which type of comment this is.
  if (src->peekChar() == '/') {
    spComment comment = spComment(new Comment());
    comment->opt = Comment::OPT_ONE_LINE;
    comment->posIni = getCursor() - 1;
    char c = src->getChar(); // consume 2nd '/'
    while ((c = src->getChar()) && c != '\n') ;
    comment->posEnd = getCursor() - 1;
    comments.push_back(comment);
    return getToken();
  }

  if (src->peekChar() == '*') {
    spComment comment = spComment(new Comment());
    comment->opt = Comment::OPT_MULTIPLE_LINES;
    comment->posIni = getCursor() - 1;
    char c = src->getChar(); // consume '*'
    while ((c = src->getChar())) {
      if (c == '*' && src->peekChar() == '/') {
        src->getChar(); // consume final '/'
        comment->posEnd = getCursor() - 1;
        comments.push_back(comment);
        return getToken();
      }
    }

    return TOK_EOF;
  }

  // Divisor
  // We look 1 char ahead to decided if we have '/='.
  if (src->peekChar() == '=') {
    src->getChar(); // conume '='
    return TOK_OP_DIV_EQUALS;
  }

  return TOK_OP_DIV;
}

int Lexer::getEqualsToken() {
  // We look 1 char ahead to decided if we have '=='.
  if (src->peekChar() == '=') {
    src->getChar(); // conume '='
    return TOK_OP_EQUALS_EQUALS;
  }

  return TOK_OP_EQUALS;
}

/// Returns TOK_ESCAPE_SEQUENCE | TOK_ERROR
int Lexer::getEscapeSequence(std::stringstream &ss) {
  ss << src->getChar(); // consume '\'
  switch(src->peekChar()) {
    case 'b': // backspace BS
    case 't': // horizontal tab HT
    case 'n': // linefeed LF
    case 'f': // form feed FF
    case 'r': // carriage return CR
    case '"': // double quote
    case '\'': // single quote
    case '\\': // backslash
      ss << src->getChar(); // consume special char
      if (src->peekChar() == '\'') {
        ss << src->getChar(); // consume ending single quote
        return TOK_ESCAPE_SEQUENCE;
      }
      return TOK_ERROR;
  }

  // Octal Escape:
  //   \ OctalDigit
  //   \ OctalDigit OctalDigit
  //   \ ZeroToThree OctalDigit OctalDigit
  if (isOctalDigit(src->peekChar())) {
    char c = src->getChar(); // consume first octal digit
    ss << c;
    if (isOctalDigit(src->peekChar())) {
      ss << src->getChar(); // consume second octal digit
      if (isOctalDigit(src->peekChar())) {
        ss << src->getChar(); // consume third octal digit
        // at this point the first octal digit must be a number
        // from zero to three
        if (!(c >= '0' && c <= '3')) {
          return TOK_ERROR;
        }
      }
      if (src->peekChar() == '\'') {
        ss << src->getChar(); // consume closing single quote
        return TOK_ESCAPE_SEQUENCE;
      }
      return TOK_ERROR;
    }
  }

  // Special case.
  // The unicode escape \u is processed earlier by the java compiler but
  // we build the AST in one pass.
  // UnicodeEscape:
  //  \ UnicodeMarker HexDigit HexDigit HexDigit HexDigit
  if (src->peekChar() == 'u') {
    // Finish consuming UnicodeMarker:
    //   UnicodeMarker:
    //     u
    //     UnicodeMarker u
    while (src->peekChar() == 'u') {
      ss << src->getChar();
    }

    // HexDigit{4}
    for (int i = 0; i < 4; i++) {
      if (isHexDigit(src->peekChar())) {
        ss << src->getChar(); // consume hex digit
      } else {
        return TOK_ERROR;
      }
    }

    if (src->peekChar() != '\'') {
      return TOK_ERROR;
    }

    ss << src->getChar(); // consume closing single quote
    return TOK_ESCAPE_SEQUENCE;
  }

  return TOK_ERROR;
}

int Lexer::getExclamationToken() {
  // We look 1 char ahead to decided if we have '!='
  if (src->peekChar() == '=') {
    src->getChar(); // conume '='
    return TOK_OP_EXCLAMATION_EQUALS;
  }

  return TOK_OP_EXCLAMATION;
}

int Lexer::getMulToken() {
  // We look 1 char ahead to decided if we have '*' or '*='.
  if (src->peekChar() == '=') {
    src->getChar(); // conume '='
    return TOK_OP_MUL_EQUALS;
  }

  return TOK_OP_MUL;
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
///   TOK_DECIMAL_FLOATING_POINT_LITERAL
///   TOK_HEXADECIMAL_FLOATING_POINT_LITERAL
///   TOK_ERROR
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

int Lexer::getPipeToken() {
  // We look 1 char ahead to decided if we have '|=' or '||'.
  if (src->peekChar() == '=') {
    src->getChar(); // conume '='
    return TOK_OP_PIPE_EQUALS;
  }

  if (src->peekChar() == '|') {
    src->getChar();
    return TOK_OP_PIPE_PIPE;
  }

  return TOK_OP_PIPE;
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

int Lexer::getPlusToken() {
  // We look 1 char ahead to decided if we have '++' or '+='
  if (src->peekChar() == '+') {
    src->getChar();
    return TOK_OP_PLUS_PLUS;
  }

  if (src->peekChar() == '=') {
    src->getChar();
    return TOK_OP_PLUS_EQUALS;
  }

  return TOK_OP_PLUS;
}

int Lexer::getRemToken() {
  // We look 1 char ahead to decided if we have '%='.
  if (src->peekChar() == '=') {
    src->getChar(); // consume '='
    return TOK_OP_REM_EQUALS;
  }

  return TOK_OP_REM;
}

int Lexer::getStringLiteral() {
  std::stringstream ss;
  ss << '"'; // opening double quotes

  // StringCharacter:
  // InputCharacter but not " or \.
  // EscapeSequence
  char c;
  int tok;
  while ((c = src->peekChar())) {
    switch (c) {
      case '\\':
        tok = getEscapeSequence(ss);
        if (tok != TOK_ESCAPE_SEQUENCE) {
          curTokenStr = ss.str();
          return tok; // error
        }
        break;
      case '"':
        ss << src->getChar(); // consume closing double quotes
        curTokenStr = ss.str();
        return TOK_STRING_LITERAL;
      default:
        ss << src->getChar(); // consume StringCharacter
    }
  }

  return TOK_ERROR;
}

int Lexer::getGreaterThenToken() {
  // We should check for '>', '>=', '>>', '>>=', '>>>', '>>>='.
  if (src->peekChar() == '=') {
    src->getChar(); // consume '='
    return TOK_OP_GT_EQUALS;
  }

  if (src->peekChar() == '>') {
    src->getChar(); // consume 2nd '>'
    if (src->peekChar() == '=') {
      src->getChar(); // consume '='
      return TOK_OP_RSHIFT_EQUALS;
    }

    if (src->peekChar() == '>') {
      src->getChar(); // consume 3rd '>'
      if (src->peekChar() == '=') {
	src->getChar(); // consume '='
	return TOK_OP_TRIPLE_RSHIFT_EQUALS;
      }

      return TOK_OP_TRIPLE_RSHIFT;
    }

    return TOK_OP_RSHIFT;
  }

  return TOK_OP_GT;
}

int Lexer::getLessThenToken() {
  // We should check for '<', '<=', '<<' or '<<='.
  if (src->peekChar() == '=') {
    src->getChar(); // consume '='
    return TOK_OP_LT_EQUALS;
  }

  if (src->peekChar() == '<') {
    src->getChar();
    if (src->peekChar() == '=') {
      src->getChar(); // consume '='
      return TOK_OP_LSHIFT_EQUALS;
    }
    return TOK_OP_LSHIFT;
  }

  return TOK_OP_LT;
}

int Lexer::getMinusToken() {
  // We look 1 char ahead to decided if we have '--' or '-='.
  if (src->peekChar() == '-') {
    src->getChar();
    return TOK_OP_MINUS_MINUS;
  }

  if (src->peekChar() == '=') {
    src->getChar();
    return TOK_OP_MINUS_EQUALS;
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
