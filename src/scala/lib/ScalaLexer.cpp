#include "c4/ScalaLexer.h"

namespace c4 {

// -----------------------------------------------------------------------------
// Helper functions
// -----------------------------------------------------------------------------
bool is_idrest(char c) {
  // TODO: check for op ::= opchar {opchar}
  if (std::isalpha(c) || isdigit(c) || c == '_') {
    return true;
  }

  return false;
}

/**
 * upper ::= ‘A’ | ··· | ‘Z’ | ‘$’ | ‘_’
 *           and Unicode category Lu
 *
 * TODO: Unicode category Lu
 */
bool is_upper(char c) {
  if (isupper(c) || c == '$' || c == '_') {
    return true;
  }

  return false;
}

// -----------------------------------------------------------------------------
// ScalaLexer methods
// -----------------------------------------------------------------------------
int ScalaLexer::getCurTokenIni() {
  return src->getCursor() - getCurTokenStr().size();
}

int ScalaLexer::getCurTokenEnd() {
  return src->getCursor();
}

spTokenNode ScalaLexer::getCurTokenNode() {
  spTokenNode tok = spTokenNode(new TokenNode);
  tok->tok = getCurToken();
  tok->ini = getCurTokenIni();
  tok->end = getCurTokenEnd();
  tok->val = getCurTokenStr();

  return tok;
}

STok ScalaLexer::getToken() {
  char c = src->getChar();
  if (!c) return STok::END_OF_FILE;

  // Skip any space char.
  while (isspace(c)) c = src->getChar();
  if (!c) return STok::END_OF_FILE;

  //if ('@' == c) return getAnnotationToken();
  //if ('\'' == c) return getCharacterLiteral();
  if ('"' == c) return getStringLiteral();
  if ('.' == c) { curTokStream << c; return STok::PERIOD; }
  //if ('+' == c) return getPlusToken();
  //if ('-' == c) return getMinusToken();
  //if ('=' == c) return getEqualsToken();
  //if ('/' == c) return getCommentOrDivToken();
  //if ('&' == c) return getAmpersandToken();
  //if ('|' == c) return getPipeToken();
  //if ('^' == c) return getCarretToken();
  //if ('%' == c) return getRemToken();
  //if ('>' == c) return getGreaterThenToken();
  //if ('<' == c) return getLessThenToken();
  if (',' == c) { curTokStream << c; return STok::COMMA; }
  if (';' == c) { curTokStream << c; return STok::SEMICOLON; }
  //if (':' == c) return TOK_OP_COLON;
  //if ('*' == c) return getMulToken();
  //if ('~' == c) return TOK_OP_TILDE;
  //if ('!' == c) return getExclamationToken();
  if ('{' == c) { curTokStream << c; return STok::LCURLYB; }
  if ('}' == c) { curTokStream << c; return STok::RCURLYB; }
  if ('(' == c) { curTokStream << c; return STok::LPAREN; }
  if (')' == c) { curTokStream << c; return STok::RPAREN; }
  if ('[' == c) { curTokStream << c; return STok::LBRACKET; }
  if (']' == c) { curTokStream << c; return STok::RBRACKET; }
  //if ('?' == c) return TOK_OP_QUESTION_MARK;

  //if (isdigit(c)) return getNumberToken(c);

  // Identifier
  //if (isScalaLetter(c)) return getTokenIdentifier(c);

  // id :: = plainid
  // plainid ::= varid
  if (islower(c)) return getLowerToken(c);
  // plainid ::= upper idrest
  if (is_upper(c)) return getUpperToken(c);
  // TODO: plainid ::= op

  // TODO: id ::= `stringLit`

  return STok::ERROR;
}

/**
 * EscapeSequence:
 *   \b
 *   \t
 *   \n
 *   \f
 *   \r
 *   \"
 *   \'
 *   \\
 *   OctalEscape \u0000 to \u00ff: from octal value
 *
 * OctalEscape:
 *   \ OctalDigit
 *   \ OctalDigit OctalDigit
 *   \ ZeroToThree OctalDigit OctalDigit
 * OctalDigit: one of
 *   0 1 2 3 4 5 6 7
 * ZeroToThree: one of
 *   0 1 2 3
 *
 * Returns STok::ESCAPE_SEQUENCE | STok::ERROR
 */
// TODO: Remove duplicate code. See Lexer::getEscapeSequence()
STok ScalaLexer::getEscapeSequence() {
  curTokStream << src->getChar(); // consume '\'
  switch (src->peekChar()) {
    case 'b': // backspace BS
    case 't': // horizontal tab HT
    case 'n': // linefeed LF
    case 'f': // form feed FF
    case 'r': // carriage return CR
    case '"': // double quote
    case '\'': // single quote
    case '\\': // backslash
      curTokStream << src->getChar(); // consume special char
      return STok::ESCAPE_SEQUENCE;
  }

  // Octal Escape:
  //   \ OctalDigit
  //   \ OctalDigit OctalDigit
  //   \ ZeroToThree OctalDigit OctalDigit
  if (isOctalDigit(src->peekChar())) {
    char c = src->getChar(); // consume first octal digit
    curTokStream << c;
    if (isOctalDigit(src->peekChar())) {
      curTokStream << src->getChar(); // consume second octal digit
      if (isOctalDigit(src->peekChar())) {
        curTokStream << src->getChar(); // consume third octal digit
        // at this point the first octal digit must be a number
        // from zero to three
        if (!(c >= '0' && c <= '3')) {
          return STok::ERROR;
        }
      }
    }
    return STok::ESCAPE_SEQUENCE;
  }

  // Special case.
  // The unicode escape \u is processed earlier by the java compiler but
  // we build the AST in one pass.
  // UnicodeEscape:
  //  \ UnicodeMarker HexDigit HexDigit HexDigit HexDigit
  // TODO: check for LINE TERMINATOR since it's invalid inside strings or char
  //       literals
  if (src->peekChar() == 'u') {
    // Finish consuming UnicodeMarker:
    //   UnicodeMarker:
    //     u
    //     UnicodeMarker u
    while (src->peekChar() == 'u') {
      curTokStream << src->getChar();
    }

    // HexDigit{4}
    for (int i = 0; i < 4; i++) {
      if (isHexDigit(src->peekChar())) {
        curTokStream << src->getChar(); // consume hex digit
      } else {
        return STok::ERROR;
      }
    }

    return STok::ESCAPE_SEQUENCE;
  }

  return STok::ERROR;
}

/**
 * STok::* Reserved Words
 */
STok ScalaLexer::getLowerToken(char c) {
  curTokStream << c;

  while ((c = src->getChar())) {
    // TODO: check for 'op'
    // idrest ::= {letter | digit} [‘_’ op]
    if (isalpha(c) || c == '_') {
      curTokStream << c;
    } else {
      src->ungetChar(1);
      break;
    }
  }

  STok tok = tokUtil.getReservedWordToken(curTokStream.str());
  if (tok != STok::ERROR) {
    return tok;
  }

  return STok::ID;
}

/**
 * STok::ID starting with the production rule 'upper'
 * @returns STok::ID || STok::UNDERSCORE
 */
STok ScalaLexer::getUpperToken(char c) {
  curTokStream << c;
  while ((c = src->getChar())) {
    if (is_idrest(c)) {
      curTokStream << c;
    } else {
      src->ungetChar(1);
      break;
    }
  }

  std::string str = curTokStream.str();
  if (str.compare("_") == 0) {
    return STok::UNDERSCORE;
  }

  return STok::ID;
}

/**
 * stringLiteral ::= ‘"’ {stringElement} ‘"’
 *                 | ‘"""’ multiLineChars ‘"""’
 *
 * stringElement ::= printableCharNoDoubleQuote
 *                 | charEscapeSeq
 *
 * @return STok::STRING_LITERAL | STok::ERROR
 */
STok ScalaLexer::getStringLiteral() {
  curTokStream << '"'; // opening double quotes

  // Check if we have """
  if (src->peekChar(0) == '"' && src->peekChar(1) == '"') {
    curTokStream << src->getChar(); // consume 2nd "
    curTokStream << src->getChar(); // consume 3rd "
    return getStringLiteralMultiLine();
  }

  char c;
  STok tok;
  while ((c = src->peekChar())) {
    switch (c) {
      case '\\':
        tok = getEscapeSequence();
        if (tok != STok::ESCAPE_SEQUENCE) {
          curTokStr = curTokStream.str();
          return STok::ERROR;
        }
        break;
      case '\n':
      case '\r':
        curTokStr = curTokStream.str();
        return STok::ERROR;
      case '"':
        curTokStream << src->getChar(); // consume closing double quotes
        curTokStr = curTokStream.str();
        return STok::STRING_LITERAL;
      default:
        curTokStream << src->getChar(); // consume StringCharacter
    }
  }

  curTokStr = curTokStream.str();
  return STok::ERROR;
}

/**
 * @see ScalaLexer::getStringLiteral()
 * @return STok::STRING_LITERAL | STok::ERROR
 */
STok ScalaLexer::getStringLiteralMultiLine() {
  // TODO:
  return STok::ERROR;
}

void ScalaLexer::getNextToken() {
  // clear string stream
  curTokStr = "";
  curTokStream.str("");
  curTokStream.clear();

  // Save current data for indentation processing
  unsigned line = src->getLine();
  STok token = curTok;

  // Consume token
  curTok = getToken();
  curTokStr = curTokStream.str();

  // Flag to indicate if there was a line break between the previous token
  // and the current token.
  seenLinebreak = !(line == src->getLine());

  processIndentation(line, src->getLine(), token, curTok);
}

void ScalaLexer::processIndentation(unsigned prevLine, unsigned curLine,
  STok prevToken, STok curToken) {

  // We should increment or decrement the indentation level when see an opening
  // or closing curly bracket.
  if (curToken == STok::LCURLYB) { increaseIndentLevel(); }
  if (curToken == STok::RCURLYB) { decreaseIndentLevel(); }

  // If there's no line change our job is done. We're only interested in the
  // first token of each line. One special case is the very first token we
  // parse.
  if ((prevLine == curLine && !indentMap.empty())
      || curToken == STok::END_OF_FILE) {
    return;
  }

  // If we have an opening curly brace we shouldn't expand the identation. We
  // assume the syle:
  // void m()
  // {
  //   // ...
  // }
  if (curToken == STok::LCURLYB) {
    addIndentation(
      indentMap, curLine, curIndentationLevel - 1, false, curToken);
    return;
  }

  // If we have a closing curly brace the indentation level has been decreased
  // and we can just add the indent information and exit.
  if (curToken == STok::RCURLYB) {
    addIndentation(indentMap, curLine, curIndentationLevel, false, curToken);
    return;
  }

  bool lineWrap = isLineWrap(prevToken);
  addIndentation(indentMap, curLine, curIndentationLevel, lineWrap, curToken);
}

bool ScalaLexer::isLineWrap(STok prevToken) {
  if (prevToken == STok::SEMICOLON) { return false; }
  if (prevToken == STok::LCURLYB) { return false; }
  if (prevToken == STok::RCURLYB) { return false; }
  if (indentMap.empty()) { return false; }
  return true;
}

void ScalaLexer::saveState(State &state) {
  state.cursor = src->getCursor();
  state.line = src->getLine();
  state.token = getCurToken();
  state.tokenStr = getCurTokenStr();

  state.indentationLevel = curIndentationLevel;
  state.indentationMapSize = indentMap.size();
}

void ScalaLexer::restoreState(State &state) {
  src->setCursor(state.cursor);
  src->setLine(state.line);
  curTok = state.token;
  curTokStr = state.tokenStr;

  curIndentationLevel = state.indentationLevel;
  while (indentMap.size() > state.indentationMapSize) {
    indentMap.erase(std::prev(indentMap.end()));
  }
}

} // namespace
