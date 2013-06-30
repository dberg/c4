#include "djp/ScalaLexer.h"

namespace djp {

STok ScalaLexer::getToken() {
  char c = src->getChar();
  if (!c) return STok::END_OF_FILE;

  // Skip any space char.
  while (isspace(c)) c = src->getChar();
  if (!c) return STok::END_OF_FILE;

  //if ('@' == c) return getAnnotationToken();
  //if ('\'' == c) return getCharacterLiteral();
  //if ('"' == c) return getStringLiteral();
  //if ('.' == c) return getPeriodStartingToken();
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
  //if (',' == c) return TOK_COMMA;
  //if (';' == c) return TOK_SEMICOLON;
  //if (':' == c) return TOK_OP_COLON;
  //if ('*' == c) return getMulToken();
  //if ('~' == c) return TOK_OP_TILDE;
  //if ('!' == c) return getExclamationToken();
  //if ('{' == c) return TOK_LCURLY_BRACKET;
  //if ('}' == c) return TOK_RCURLY_BRACKET;
  //if ('(' == c) return TOK_LPAREN;
  //if (')' == c) return TOK_RPAREN;
  //if ('[' == c) return TOK_LBRACKET;
  //if (']' == c) return TOK_RBRACKET;
  //if ('?' == c) return TOK_OP_QUESTION_MARK;

  //if (isdigit(c)) return getNumberToken(c);

  // Identifier
  //if (isScalaLetter(c)) return getTokenIdentifier(c);

  return STok::ERROR;
}

void ScalaLexer::getNextToken() {
  // Save current data for indentation processing
  unsigned line = src->getLine();
  STok token = curToken;

  // Consume token
  curToken = getToken();

  // TODO:
  //processIndentation(line, src->getLine(), token, curToken);
}

} // namespace
