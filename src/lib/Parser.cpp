#include "Parser.h"

namespace djp {

/// ClassModifier: one of
///   Annotation public protected private
///   abstract static final strictfp
/// ConstructorModifier: one of
///   Annotation public protected private
bool Parser::isModifierToken(int token) {
  if (TOK_KEY_PUBLIC == token ||
      TOK_KEY_PROTECTED == token ||
      TOK_KEY_PRIVATE == token ||
      TOK_KEY_ABSTRACT == token ||
      TOK_KEY_STATIC == token ||
      TOK_KEY_FINAL == token ||
      TOK_KEY_STRICTFP == token) {
    return true;
  }

  if (TOK_ANNOTATION == token) {
    return true;
  }

  return false;
}

bool Parser::isJavaLetter(char c) {
  return (isalpha(c) || c == '$' || c == '_');
}

bool Parser::isJavaLetterOrDigit(char c) {
  return (isJavaLetter(c) || isdigit(c));
}

bool Parser::isValidInitTokenOfClassBodyDeclaration(int token) {
  // Prior to a MemberDecl
  if (isModifierToken(token)) {
    return true;
  }

  // MemberDecl
  // TODO: What else?
  if (TOK_IDENTIFIER == token) {
    return true;
  }

  // Prior to a Block
  if (TOK_KEY_STATIC == token) {
    return true;
  }

  // TODO: Block?

  // A happy and lost semicolon
  if (TOK_SEMICOLON == token) {
    return true;
  }

  return false;
}

bool Parser::isValidInitTokenOfTypeDeclaration(int token) {
  if (isModifierToken(token)) {
    return true;
  }

  if (TOK_ANNOTATION == curToken
    || TOK_KEY_CLASS == curToken
    || TOK_KEY_INTERFACE == curToken) {

    return true;
  }

  return false;
}

void Parser::saveState(State &state) {
  state.cursor = cursor;
  state.line = line;
  state.token = curToken;
  state.tokenStr = curTokenStr;
}

void Parser::restoreState(State &state) {
  cursor = state.cursor;
  line = state.line;
  curToken = state.token;
  curTokenStr = state.tokenStr;
}

void Parser::addError(int ini, int end, int err) {
  spError error = spError(new Error(ini, end, err));
  compilationUnit->errors.push_back(error);
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
  if ('@' == c) return getAnnotationToken();
  if ('.' == c) return getPeriodOrEllipsisToken();

  if (';' == c) return TOK_SEMICOLON;
  if ('*' == c) return TOK_ASTERISK;
  if ('{' == c) return TOK_LCURLY_BRACKET;
  if ('}' == c) return TOK_RCURLY_BRACKET;

  // Identifier
  if (isJavaLetter(c))
    return getTokenIdentifier(c);

  return c;
}

/// Return TOK_ANNOTATION | TOK_ANNOTATION_TYPE_DECLARATION
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

/// Return TOK_PERIOD | TOK_ELLIPSIS
int Parser::getPeriodOrEllipsisToken() {
  // We look 2 chars ahead to decide if we have an ellipsis.
  // At this point we have already found one dot char and the
  // cursor is pointing to the next char.
  // .??
  //  ^
  //  cursor
  if (cursor + 1 <= buffer.length()
    && buffer[cursor] == '.'
    && buffer[cursor+1] == '.') {
    return TOK_ELLIPSIS;
  }

  return TOK_PERIOD;
}

/// Return TOK_IDENTIFIER | TOK_KEY_*
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

  // If keyword return the matching token
  if (int keywordToken = tokenUtil.getKeywordToken(curTokenStr)) {
    return keywordToken;
  }

  // TODO:
  // If BooleanLiteral return matching token
  // If NullLiteral return matching token

  return TOK_IDENTIFIER;
}

/// Annotation: @ QualifiedIdentifier [ ( [AnnotationElement] ) ]
spAnnotation Parser::parseAnnotation() {
  spAnnotation annotation = spAnnotation(new Annotation());
  annotation->posTokAt = cursor - 1;
  getNextToken(); // Consume '@'

  // We're now parsing QualifiedIdentifier
  if (curToken != TOK_IDENTIFIER) {
    annotation->err = true;
    addError(annotation->posTokAt, annotation->posTokAt + 1, ERR_EXP_QID);
    return annotation;
  }

  annotation->qualifiedId = parseQualifiedIdentifier();

  // If the current token is '(' we consume the token and expect
  // an optional AnnotaionElement followed by ')'
  if ('(' == curToken) {
    int openParenPos = cursor - 1;
    getNextToken(); // consume ')'

    // Empty annotation element
    if (')' != curToken) {
      spAnnotationElement annotationElem = parseAnnotationElement();
      if (annotationElem->err) {
        annotation->err = true;
	addError(annotation->posTokAt, openParenPos, ERR_NVAL_ANNOT_ELEM);
        return annotation;
      }

      annotation->elem = annotationElem;
    }

    if (')' != curToken) {
      annotation->err = true;
      addError(annotation->posTokAt, openParenPos, ERR_EXP_CLOSE_PAREN);
      return annotation;
    }

    getNextToken(); // consume ')'
  }

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
    annotations.push_back(annotation);
  }
}

/// PackageDeclaration: [ [Annotations]  package QualifiedIdentifier ; ]
spPackageDeclaration Parser::parsePackageDeclaration(
  std::vector<spAnnotation> &annotations) {
  spPackageDeclaration pkgDecl = spPackageDeclaration(new PackageDeclaration());
  // If we have annotations they belong to the package declaration
  if (annotations.size()) {
    pkgDecl->annotations = annotations;
    annotations.clear();
  }
  pkgDecl->pkgTokPos = cursor - tokenUtil.getTokenLength(TOK_KEY_PACKAGE);

  getNextToken(); // Consume 'package'

  if (TOK_IDENTIFIER != curToken) {
    pkgDecl->err = true;
    return pkgDecl;
  }

  pkgDecl->qualifiedId = parseQualifiedIdentifier();
  if (TOK_SEMICOLON != curToken) {
    pkgDecl->err = true;
    return pkgDecl;
  }

  getNextToken(); // Consume ';'
  return pkgDecl;
}

/// ImportDeclarations:
///   ImportDeclaration
///   ImportDeclarations ImportDeclaration
/// ImportDeclaration: import [static] QualifiedId [.*];
spImportDeclarations Parser::parseImportDeclarations() {
  std::vector<spImportDeclaration> imports;
  while (TOK_KEY_IMPORT == curToken) {
    spImportDeclaration import = parseImportDeclaration();
    imports.push_back(import);
    getNextToken(); // consume ';'
  }

  spImportDeclarations impDecls = spImportDeclarations(
    new ImportDeclarations(imports));
  return impDecls;
}

spImportDeclaration Parser::parseImportDeclaration() {
  spImportDeclaration import = spImportDeclaration(new ImportDeclaration());
  import->type = SINGLE_TYPE_IMPORT_DECLARATION;
  import->posTokImport = cursor - tokenUtil.getTokenLength(TOK_KEY_IMPORT);
  getNextToken(); // consume 'import' keyword

  if (TOK_KEY_STATIC == curToken) {
    import->posTokStatic = cursor - tokenUtil.getTokenLength(TOK_KEY_STATIC);
    import->type = SINGLE_STATIC_IMPORT_DECLARATION;
    getNextToken();
  }

  if (TOK_IDENTIFIER != curToken) {
    import->err = true;
    return import;
  }

  import->qualifiedId = parseQualifiedIdentifier();

  // Check [.*]
  if (TOK_PERIOD == curToken) {
    import->iniOnDemand = cursor - 1;
    getNextToken(); // consume '.'
    if (TOK_ASTERISK != curToken) {
      import->err = true;
      return import;
    }

    import->endOnDemand = cursor - 1;
    if (import->posTokStatic > 0) {
      import->type = STATIC_IMPORT_ON_DEMAND_DECLARATION;
    } else {
      import->type = TYPE_IMPORT_ON_DEMAND_DECLARATION;
    }

    getNextToken(); // consume '*'
  }

  if (TOK_SEMICOLON != curToken) {
    import->err = true;
    return import;
  }

  return import;
}

/// QualifiedIdentifier: Identifer { . Identifier }
spQualifiedIdentifier Parser::parseQualifiedIdentifier() {
  std::vector<spIdentifier> identifiers;
  // Save current identifier
  spIdentifier id = spIdentifier(
    new Identifier(cursor - curTokenStr.length(), curTokenStr));
  identifiers.push_back(id);

  State backup;
  while (true) {
    getNextToken();
    if (TOK_PERIOD != curToken) {
      break;
    }

    // We have a period, if the next token is not an identifier we restore
    // the period token state and exit the while loop
    saveState(backup);
    getNextToken();
    if (TOK_IDENTIFIER != curToken) {
      restoreState(backup);
      break;
    }

    // Save the identifier
    spIdentifier id = spIdentifier(
      new Identifier(cursor - curTokenStr.length(), curTokenStr));
    identifiers.push_back(id);
  }

  // We have at least one identifier to build the QualifiedIdentifier
  spQualifiedIdentifier qualifiedId = spQualifiedIdentifier(
    new QualifiedIdentifier(identifiers));
  return qualifiedId;
}

/// TypeDeclarations: { TypeDeclaration }
/// TypeDeclaration: ClassOrInterfaceDeclaration ;
std::vector<spTypeDeclaration> Parser::parseTypeDeclarations(
  std::vector<spAnnotation> &annotations) {

  std::vector<spTypeDeclaration> typeDecls = std::vector<spTypeDeclaration>();

  if (annotations.size() > 0) {
    spModifier modifier = spModifier(new Modifier());
    modifier->annotations = annotations;

    spTypeDeclaration typeDecl = spTypeDeclaration(new TypeDeclaration());
    typeDecl->decl = spClassOrInterfaceDeclaration(
      new ClassOrInterfaceDeclaration());
    typeDecl->decl->modifier = modifier;

    parseClassOrInterfaceDeclaration(typeDecl->decl);
    typeDecls.push_back(typeDecl);
  }

  while (isValidInitTokenOfTypeDeclaration(curToken)) {
    spTypeDeclaration typeDecl = spTypeDeclaration(new TypeDeclaration());
    typeDecl->decl = spClassOrInterfaceDeclaration(
      new ClassOrInterfaceDeclaration());
    parseClassOrInterfaceDeclaration(typeDecl->decl);
    typeDecls.push_back(typeDecl);
  }

  return typeDecls;
}

/// ClassOrInterfaceDeclaration:
///   {Modifier} (ClassDeclaration | InterfaceDeclaration)
void Parser::parseClassOrInterfaceDeclaration(
  spClassOrInterfaceDeclaration& decl) {

  // Modifier
  if (!decl->modifier) {
    decl->modifier = spModifier(new Modifier());
  }

  parseModifier(decl->modifier);

  if (TOK_KEY_CLASS == curToken || TOK_KEY_ENUM == curToken) {
    decl->classDecl = spClassDeclaration(new ClassDeclaration());
    parseClassDeclaration(decl->classDecl);
    return;
  }

  // TODO:
  if (TOK_KEY_INTERFACE == curToken) {
    getNextToken();
    return;
  }

  // TODO: handle error
}

void Parser::parseModifier(spModifier &modifier) {

  while (isModifierToken(curToken)) {
    // Annotations
    if (curToken == TOK_ANNOTATION) {
      parseAnnotations(modifier->annotations);
      continue;
    }

    // Tokens
    spTokenExp token = spTokenExp(new TokenExp(
      cursor - tokenUtil.getTokenLength(curToken), curToken));
    modifier->tokens.push_back(token);
    getNextToken();
  }
}

/// ClassDeclaration: NormalClassDeclaration | EnumDeclaration
void Parser::parseClassDeclaration(spClassDeclaration &classDecl) {
  if (TOK_KEY_CLASS == curToken) {
    classDecl->nClassDecl = spNormalClassDeclaration(
      new NormalClassDeclaration());
    parseNormalClassDeclaration(classDecl->nClassDecl);
    return;
  }

  // TODO:
  if (TOK_KEY_ENUM == curToken) {
    getNextToken();
    return;
  }

  // TODO: handle error
  getNextToken();
}

/// NormalClassDeclaration:
///   class Identifier [TypeParameters] [extends Type] [implements TypeList]
///     ClassBody
void Parser::parseNormalClassDeclaration(spNormalClassDeclaration &nClassDecl) {
  // TODO: Handle error.
  if (TOK_KEY_CLASS != curToken) {
    return;
  }

  nClassDecl->classTok = spTokenExp(new TokenExp(
    cursor - tokenUtil.getTokenLength(TOK_KEY_CLASS), curToken));
  getNextToken(); // consume 'class'

  // TODO: handle error
  // Identifier
  if (TOK_IDENTIFIER != curToken) {
    return;
  }

  int pos = cursor - curTokenStr.length();
  nClassDecl->identifier = spIdentifier(new Identifier(pos, curTokenStr));
  st.addSym(ST_CLASS, curToken, pos, line, curTokenStr);
  getNextToken(); // consume Identifier

  // TODO: [TypeParameters]
  // TODO: [extends Type]
  // TODO: [implements TypeList]

  nClassDecl->classBody = spClassBody(new ClassBody());
  parseClassBody(nClassDecl->classBody);
}

/// ClassBody: '{' { ClassBodyDeclaration } '}'
void Parser::parseClassBody(spClassBody &classBody) {
  // TODO: handle error
  if (TOK_LCURLY_BRACKET != curToken) {
    return;
  }

  getNextToken(); // consume '{'

  // ClassBodyDeclaration
  while (isValidInitTokenOfClassBodyDeclaration(curToken)) {
    if (TOK_SEMICOLON == curToken) {
      getNextToken(); // consume ';'
      continue;
    }

    spClassBodyDeclaration decl = spClassBodyDeclaration(
      new ClassBodyDeclaration());
    parseClassBodyDeclaration(decl);
    classBody->decls.push_back(decl);
  }

  if (TOK_RCURLY_BRACKET != curToken) {
    return;
  }
  getNextToken(); // consume '}'
}

/// ClassBodyDeclaration:
///   ;
///   {Modifier} MemberDecl
///   [static] Block
void Parser::parseClassBodyDeclaration(spClassBodyDeclaration &decl) {
  if (isModifierToken(curToken) || TOK_IDENTIFIER == curToken) {
    decl->opt = ClassBodyDeclaration::OPT_MODIFIER_MEMBER_DECL;
    parseModifier(decl->modifier);
    decl->memberDecl = spMemberDecl(new MemberDecl());
    parseMemberDecl(decl->memberDecl);
    return;
  }

  // TODO: [static] Block

  // TODO:
  getNextToken();
}

/// MemberDecl:
///   MethodOrFieldDecl
///   void Identifier VoidMethodDeclaratorRest
///   Identifier ConstructorDeclaratorRest
///   GenericMethodOrConstructorDecl
///   ClassDeclaration
///   InterfaceDeclaration
void Parser::parseMemberDecl(spMemberDecl &memberDecl) {
  // We have an Identifier but we have to discern between a TypeParameter and a
  // Constructor Identifier. For example, the identifier we found can represent
  // the return type of the following method:
  //     ReturnType method(...) {}
  //     ----------
  // Or, our identifier can represent the constructor of the following class:
  //     class MyClass { MyClass() {...} }
  //                     ---------
  // We consult the symbol to check if the Identifier name is the same as the
  // class name in our current scope.
  if (TOK_IDENTIFIER == curToken) {
    if (st.isConstructor(curTokenStr)) {
      memberDecl->opt = MemberDecl::OPT_IDENTIFIER_CONSTRUCTOR_DECLARATOR_REST;

      // Identifier
      int pos = cursor - curTokenStr.length();
      memberDecl->identifier = spIdentifier(new Identifier(pos, curTokenStr));
      st.addSym(ST_CLASS, curToken, pos, line, curTokenStr);
      getNextToken(); // consume Identifier

      // ConstructorDeclaratorRest
      memberDecl->constDeclRest = spConstructorDeclaratorRest(
        new ConstructorDeclaratorRest());
      parseConstructorDeclaratorRest(memberDecl->constDeclRest);
      return;
    }
  }


  // TODO: MethodOrFieldDecl
  // TODO: void Identifier VoidMethodDeclaratorRest

  // TODO: Identifier ConstructorDeclaratorRest

  // TODO: GenericMethodOrConstructorDecl
  // TODO: ClassDeclaration
  // TODO: InterfaceDeclaration

  // TODO:
  getNextToken();
}

/// CompilationUnit: Top level parsing.
///   [PackageDeclaration] [ImportDeclaration] [TypeDeclarations]
void Parser::parseCompilationUnit() {
  std::vector<spAnnotation> annotations;
  if (curToken == TOK_ANNOTATION) {
    parseAnnotations(annotations);
  }

  if (curToken == TOK_KEY_PACKAGE) {
    compilationUnit->pkgDecl = parsePackageDeclaration(annotations);
  }

  // Import Declaration
  if (curToken == TOK_KEY_IMPORT) {
    // If we still have annotations we're in an invalid state
    if (annotations.size()) {
      // TODO: handle annotation error.
      // We should error flag all annotations and insert an error message.
    }

    compilationUnit->impDecls = parseImportDeclarations();
  }

  // Type Declarations
  compilationUnit->typeDecls = parseTypeDeclarations(annotations);
}

/// ConstructorDeclaratorRest:
///   FormalParameters [throws QualifiedIdentifierList] Block
void Parser::parseConstructorDeclaratorRest(
  spConstructorDeclaratorRest &constDeclRest) {
  // TODO:
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
      return;
    }
  }
}
} // namespace
