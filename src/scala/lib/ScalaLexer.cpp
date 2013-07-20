#include "djp/ScalaLexer.h"

namespace djp {

spTokenNode ScalaLexer::getCurTokenNode() {
  spTokenNode tok = spTokenNode(new TokenNode());
  tok->tok = getCurToken();
  tok->ini = src->getCursor() - tokUtil.getTokenLength(getCurToken());
  tok->end = src->getCursor() - 1;
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

  if (islower(c)) return getLowerToken(c);

  return STok::ERROR;
}

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

  // TODO: id?
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

  // TODO:
  //processIndentation(line, src->getLine(), token, curToken);
}

} // namespace
