#include "Parser.h"

namespace djp {

bool Parser::isJavaLetter(char c) {
  return (isalpha(c) || c == '$' || c == '_');
}

bool Parser::isJavaLetterOrDigit(char c) {
  return (isJavaLetter(c) || isdigit(c));
}

/// Return the next char in the buffer or '\0' if we hit the end of the buffer.
const char Parser::getChar() {
  if (cursor > buffer.length()) {
    return '\0';
  }

  if (buffer[cursor] == '\n') {
    line++;
  }

  return buffer[cursor++];
}

const char Parser::ungetChar(int count) {
  cursor -= count;
  return buffer[cursor];
}

/// We check if the next token is the keyword 'interface'.
/// We assume that any whitespace has been previously consumed.
bool Parser::lookaheadInterface(int point) {
  std::string result = std::string(buffer, point, 9);
  if (result == "interface") return true;
  return false;
}

void Parser::getNextToken() {
  curToken = getToken();
}

int Parser::getToken() {
  char c = getChar();
  if (!c) return TOK_EOF;

  // Skip any space char.
  while (isspace(c)) c = getChar();
  if (!c) return TOK_EOF;

  // Annotation and Annotation Type Declarations
  if (c == '@') return getAnnotationToken();

  // Identifier
  if (isJavaLetter(c))
    return getTokenIdentifier(c);

  return c;
}

/// Return TOK_ANNOTATION | TOK_ANNOTATION_TYPE_DECLARATION | TOK_ERROR.
int Parser::getAnnotationToken() {
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

  // We hit the end of the buffer.
  if (!c) {
    return TOK_ERROR;
  }

  bool isNextTokenInterface = lookaheadInterface(cursor - 1);
  ungetChar(lookahead);

  if (isNextTokenInterface) {
    return TOK_ANNOTATION_TYPE_DECLARATION;
  }

  return TOK_ANNOTATION;
}

int Parser::getTokenIdentifier(char c) {
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

  // TODO:
  // If keyword return the matching token
  // If BooleanLiteral return matching token
  // If NullLiteral return matching token

  return TOK_IDENTIFIER;
}

/// Annotation: @ QualifiedIdentifier [ ( [AnnotationElement] ) ]
spAnnotation Parser::parseAnnotation() {
  spAnnotation annotation;
  // Current token is '@' we mark it as the init position of the annotation
  annotation->pos = cursor - 1;
  getNextToken(); // Consume '@'

  // We're now parsing QualifiedIdentifier
  if (curToken != TOK_IDENTIFIER) {
    annotation->len = cursor;
    annotation->err = true;
    return annotation;
  }

  spQualifiedIdentifier qualifiedId = parseQualifiedIdentifier();

  // If the current token is '(' we consume the token and expect
  // an optional AnnotaionElement followed by ')'
  if ('(' == curToken) {
    getNextToken(); // consume ')'

    // Empty annotation element
    if (')' != curToken) {
      spAnnotationElement annotationElem = parseAnnotationElement();
      if (annotationElem->err) {
        annotation->len = cursor;
        annotation->err = true;
        return annotation;
      }

      annotation->elem = annotationElem;
    }

    if (')' != curToken) {
      annotation->len = cursor;
      annotation->err = true;
      return annotation;
    }

    getNextToken(); // consume ')'
  }

  annotation->len = cursor;
  return annotation;
}

/// AnnotationElement: ElementValuePairs | ElementValue
spAnnotationElement Parser::parseAnnotationElement() {
  // TODO:
  getNextToken();
  return spAnnotationElement();
}

/// Annotations.
void Parser::parseAnnotations(std::vector<spAnnotation> &annotations) {
  while (curToken == TOK_ANNOTATION) {
    spAnnotation annotation = parseAnnotation();
    // TODO:
    // If annotation is not null we append annotation to annotations
    // Else we have an error
  }
}

/// QualifiedIdentifier: Identifer { . Identifier }
spQualifiedIdentifier Parser::parseQualifiedIdentifier() {
  std::stringstream ss;
  ss << curTokenStr;

  while (true) {
    int pos = cursor;
    getNextToken();
    if ('.' != curToken) {
      cursor = pos;
      break;
    }
    getNextToken();
    if (TOK_IDENTIFIER != curToken) {
      cursor = pos;
      break;
    }

    ss << curToken;
  }

  curTokenStr = ss.str();
  spQualifiedIdentifier qualifiedId = spQualifiedIdentifier(
    new QualifiedIdentifier(cursor - curTokenStr.length(), curTokenStr));

  return qualifiedId;
}

/// CompilationUnit: Top level parsing.
///   [PackageDeclaration] [ImportDeclaration] [TypeDeclarations]
void Parser::parseCompilationUnit() {
  std::vector<spAnnotation> annotations;
  if (curToken == TOK_ANNOTATION) {
    parseAnnotations(annotations);
  }

  if (curToken == TOK_PACKAGE) {

  }

  getNextToken();
}

void Parser::parse() {
  getNextToken();
  while (true) {
    switch (curToken) {
    case TOK_EOF:
      return;
    case TOK_ERROR:
      return;
    default:
      parseCompilationUnit();
    }
  }
}
} // namespace
