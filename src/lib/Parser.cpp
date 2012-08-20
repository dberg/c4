#include "Parser.h"

namespace djp {

// Helper functions
bool isBasicType(int token) {
  if (TOK_KEY_BYTE == token
    || TOK_KEY_SHORT == token
    || TOK_KEY_CHAR == token
    || TOK_KEY_INT == token
    || TOK_KEY_LONG == token
    || TOK_KEY_FLOAT == token
    || TOK_KEY_DOUBLE == token
    || TOK_KEY_BOOLEAN == token) {
    return true;
  }

  return false;
}

bool isDecimalIntegerLiteral(int token) {
  if (token == TOK_DECIMAL_NUMERAL
    || token == TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX) {
    return true;
  }

  return false;
}

bool isHexIntegerLiteral(int token) {
  if (token == TOK_HEX_NUMERAL
    || token == TOK_HEX_NUMERAL_WITH_INT_TYPE_SUFFIX) {
    return true;
  }

  return false;
}

bool isOctalIntegerLiteral(int token) {
  if (token == TOK_OCTAL_NUMERAL
    || token == TOK_OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX) {
    return true;
  }

  return false;
}

bool isBinaryIntegerLiteral(int token) {
  if (token == TOK_BINARY_NUMERAL
    || token == TOK_BINARY_NUMERAL_WITH_INT_TYPE_SUFFIX) {
    return true;
  }

  return false;
}

bool isIntegerLiteral(int token) {
  if (isDecimalIntegerLiteral(token)) { return true; }
  if (isHexIntegerLiteral(token)) { return true; }
  if (isOctalIntegerLiteral(token)) { return true; }
  if (isBinaryIntegerLiteral(token)) { return true; }
  return false;
}

bool isFloatingPointLiteral(int token) {
  return (token == TOK_DECIMAL_FLOATING_POINT_LITERAL
    || token == TOK_HEXADECIMAL_FLOATING_POINT_LITERAL);
}

/// ClassModifier: one of
///   Annotation public protected private
///   abstract static final strictfp
/// ConstructorModifier: one of
///   Annotation public protected private
bool isModifierToken(int token) {
  if (TOK_KEY_PUBLIC == token
    || TOK_KEY_PROTECTED == token
    || TOK_KEY_PRIVATE == token
    || TOK_KEY_ABSTRACT == token
    || TOK_KEY_STATIC == token
    || TOK_KEY_FINAL == token
    || TOK_KEY_STRICTFP == token) {
    return true;
  }

  if (TOK_ANNOTATION == token) {
    return true;
  }

  return false;
}

bool isPrefixOp(int token) {
  if (TOK_OP_PLUS_PLUS == token
    || TOK_OP_MINUS_MINUS == token
    || TOK_OP_EXCLAMATION == token
    || TOK_OP_TILDE == token
    || TOK_OP_PLUS == token
    || TOK_OP_MINUS == token) {
    return true;
  }

  return false;
}

bool isPostfixOp(int token) {
  if (TOK_OP_PLUS_PLUS == token
    || TOK_OP_MINUS_MINUS == token) {
    return true;
  }

  return false;
}

bool isValidInitTokenOfClassBodyDeclaration(int token) {
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

bool isValidInitTokenOfTypeDeclaration(int token) {
  if (isModifierToken(token)) {
    return true;
  }

  if (TOK_ANNOTATION == token
    || TOK_KEY_CLASS == token
    || TOK_KEY_INTERFACE == token) {
    return true;
  }

  return false;
}

// Parser Helper methods
void Parser::addError(int err) {
  addError(lexer->getCurTokenIni(), lexer->getCursor(), err);
}

void Parser::addError(int ini, int end, int err) {
  spError error = spError(new Error(ini, end, err));
  compilationUnit->errors.push_back(error);
}

/// Annotation: @ QualifiedIdentifier [ ( [AnnotationElement] ) ]
spAnnotation Parser::parseAnnotation() {
  spAnnotation annotation = spAnnotation(new Annotation());
  annotation->posTokAt = lexer->getCursor() - 1;
  lexer->getNextToken(); // Consume '@'

  // QualifiedIdentifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    annotation->err = true;
    addError(annotation->posTokAt, annotation->posTokAt + 1, ERR_EXP_QID);
    return annotation;
  }

  annotation->qualifiedId = parseQualifiedIdentifier();

  // If the current token is '(' we consume the token and expect
  // an optional AnnotaionElement followed by ')'
  if (lexer->getCurToken() == TOK_LPAREN) {
    int openParenPos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ')'

    // Empty annotation element
    if (lexer->getCurToken() != TOK_RPAREN) {
      annotation->elem = spAnnotationElement(new AnnotationElement());
      parseAnnotationElement(annotation->elem);
      if (annotation->elem->err) {
        annotation->err = true;
        addError(annotation->posTokAt, openParenPos, ERR_NVAL_ANNOT_ELEM);
        return annotation;
      }
    }

    if (lexer->getCurToken() != TOK_RPAREN) {
      annotation->err = true;
      addError(annotation->posTokAt, openParenPos, ERR_EXP_LPAREN);
      return annotation;
    }

    lexer->getNextToken(); // consume ')'
  }

  return annotation;
}

/// AnnotationElement: ElementValuePairs | ElementValue
void Parser::parseAnnotationElement(spAnnotationElement &elem) {
  // ElementValuePairs
  parseElementValuePairs(elem->pairs);
  if (elem->pairs.size() > 0) {
    elem->opt = AnnotationElement::OPT_ELEMENT_VALUE_PAIRS;
    return;
  }

  // ElementValue
  parseElementValue(elem->value);
  if (elem->value) {
    elem->opt = AnnotationElement::OPT_ELEMENT_VALUE;
  }
}

/// Annotations.
void Parser::parseAnnotations(std::vector<spAnnotation> &annotations) {
  while (lexer->getCurToken() == TOK_ANNOTATION) {
    spAnnotation annotation = parseAnnotation();
    annotations.push_back(annotation);
  }
}

/// {[]}
int Parser::parseArrayDepth() {
  int depth = 0;
  while (lexer->getCurToken() == TOK_LBRACKET) {
    depth += 1;
    lexer->getNextToken(); // consume '['

    if (lexer->getCurToken() != TOK_RBRACKET) {
      addError(ERR_EXP_RBRACKET);
      return depth;
    }

    lexer->getNextToken(); // consume ']'
  }

  return depth;
}

/// ElementValue: Annotation | Expression1 | ElementValueArrayInitializer
void Parser::parseElementValue(spElementValue &value) {
  if (lexer->getCurToken() == TOK_ANNOTATION) {
    value->opt = ElementValue::OPT_ANNOTATION;
    value->annotation = parseAnnotation();
    return;
  }

  // Expression1
  spExpression1 expr1 = spExpression1(new Expression1());
  parseExpression1(expr1);
  if (expr1->isEmpty() == false) {
    value->opt = ElementValue::OPT_EXPRESSION1;
    value->expr1 = expr1;
    return;
  }

  // TODO: ElementValueArrayInitializer
}

/// Expression: Expression1 [ AssignmentOperator Expression1 ]
void Parser::parseExpression(spExpression &expr) {
  spExpression1 expr1 = spExpression1(new Expression1());
  parseExpression1(expr1);
  if (expr1->isEmpty()) {
    return;
  }

  expr->expr1 = expr1;

  // TODO: [ AssignmentOperator Expression1 ]
}

/// Expression1: Expression2 [Expression1Rest]
void Parser::parseExpression1(spExpression1 &expr1) {
  spExpression2 expr2 = spExpression2(new Expression2());
  parseExpression2(expr2);
  if (expr2->isEmpty()) {
    return;
  }

  expr1->expr2 = expr2;

  // TODO:
  // Expression1Rest
}

/// Expression2: Expression3 [ Expression2Rest ]
void Parser::parseExpression2(spExpression2 &expr2) {
  spExpression3 expr3 = spExpression3(new Expression3());
  parseExpression3(expr3);
  if (expr3->isEmpty()) {
    return;
  }

  expr2->expr3 = expr3;

  // TODO:
  // Expression2Rest
}

/// Expression3:
///   PrefixOp Expression3
///   ( Expression | Type ) Expression3
///   Primary { Selector } { PostfixOp }
///
/// The first option is recursive only if the prefixes are '!' and '~'.
/// For example: !!a; ~~b; are legal expressions but ++++a; is invalid
/// while ~++c; is valid;
void Parser::parseExpression3(spExpression3 &expr3) {
  // PrefixOp Expression3
  if (isPrefixOp(lexer->getCurToken())) {
    expr3->opt = Expression3::OPT_PREFIXOP_EXPRESSION3;
    expr3->prefixOp = spPrefixOp(new PrefixOp(
      lexer->getCurTokenIni(), lexer->getCurToken()));
    expr3->expr3 = spExpression3(new Expression3());
    parseExpression3(expr3->expr3);
    return;
  }

  // ( Expression | Type ) Expression3
  /*
  // TODO:
  // Expression will lead us back to Expression3 checks.
  // We should check if we have a token that is a candidate for Expression3
  // before we check for Expression or we can be stuck an infinite loop.
  spExpression expr = spExpression(new Expression());
  parseExpression(expr);
  if (expr->isEmpty() == false) {
    expr3->opt = Expression3::OPT_EXPRESSION_TYPE_EXPRESSION3;
    expr3->expr = expr;
    expr3->expr3 = spExpression3(new Expression3());
    parseExpression3(expr3->expr3);
    return;
  }

  spType type = spType(new Type());
  parseType(type);
  if (type->isEmpty() == false) {
    expr3->opt = Expression3::OPT_EXPRESSION_TYPE_EXPRESSION3;
    expr3->type = type;
    expr3->expr3 = spExpression3(new Expression3());
    parseExpression3(expr3->expr3);
    return;
  }
  */

  // Primary { Selector } { PostfixOp }
  spPrimary primary = spPrimary(new Primary());
  parsePrimary(primary);
  if (primary->isEmpty() == false) {
    primary->opt = Primary::OPT_LITERAL;
    expr3->opt = Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP;
    expr3->primary = primary;

    // TODO: { Selector } { PostfixOp }
  }
}

/// ElementValuePairs: ElementValuePair {, ElementValuePair }
/// ElementValuePair: Identifier = ElementValue
void Parser::parseElementValuePairs(std::vector<spElementValuePair> &pairs) {
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    return;
  }

  // Lookahed for a '='.
  State identifierState;
  lexer->saveState(identifierState);
  lexer->getNextToken(); // consume Identifier
  if (lexer->getCurToken() != TOK_OP_EQUALS) {
    lexer->restoreState(identifierState);
    return;
  }

  // We know we have an identifier in our saved state and the current token is
  // an assignment. We should process the identifier and expect an ElementValue.
  lexer->getNextToken(); // consume '='
  spElementValuePair pair = spElementValuePair(new ElementValuePair());
  pair->id = spIdentifier(new Identifier(
    identifierState.cursor - identifierState.tokenStr.length(),
    identifierState.tokenStr));
  pair->value = spElementValue(new ElementValue());
  parseElementValue(pair->value);
  if (pair->value->opt == ElementValue::OPT_UNDEFINED) {
    addError(ERR_EXP_ELEMENT_VALUE);
  }

  // Even if we have an error while parsing the element value we add the pair
  // indicating this is an ElementValuePair node.
  pairs.push_back(pair);

  if (lexer->getCurToken() == TOK_COMMA) {
    lexer->getNextToken(); // consume ','
    parseElementValuePairs(pairs);
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
  pkgDecl->pkgTokPos = lexer->getCursor()
    - tokenUtil.getTokenLength(TOK_KEY_PACKAGE);

  lexer->getNextToken(); // Consume 'package'

  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    pkgDecl->err = true;
    return pkgDecl;
  }

  pkgDecl->qualifiedId = parseQualifiedIdentifier();
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    pkgDecl->err = true;
    return pkgDecl;
  }

  lexer->getNextToken(); // Consume ';'
  return pkgDecl;
}

/// ImportDeclarations:
///   ImportDeclaration
///   ImportDeclarations ImportDeclaration
/// ImportDeclaration: import [static] QualifiedId [.*];
spImportDeclarations Parser::parseImportDeclarations() {
  std::vector<spImportDeclaration> imports;
  while (lexer->getCurToken() == TOK_KEY_IMPORT) {
    spImportDeclaration import = parseImportDeclaration();
    imports.push_back(import);
    lexer->getNextToken(); // consume ';'
  }

  spImportDeclarations impDecls = spImportDeclarations(
    new ImportDeclarations(imports));
  return impDecls;
}

spImportDeclaration Parser::parseImportDeclaration() {
  spImportDeclaration import = spImportDeclaration(new ImportDeclaration());
  import->type = SINGLE_TYPE_IMPORT_DECLARATION;
  import->posTokImport = lexer->getCursor()
    - tokenUtil.getTokenLength(TOK_KEY_IMPORT);
  lexer->getNextToken(); // consume 'import' keyword

  if (lexer->getCurToken() == TOK_KEY_STATIC) {
    import->posTokStatic =
      lexer->getCursor() - tokenUtil.getTokenLength(TOK_KEY_STATIC);
    import->type = SINGLE_STATIC_IMPORT_DECLARATION;
    lexer->getNextToken();
  }

  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    import->err = true;
    return import;
  }

  import->qualifiedId = parseQualifiedIdentifier();

  // Check [.*]
  if (lexer->getCurToken() == TOK_PERIOD) {
    import->iniOnDemand = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '.'
    if (lexer->getCurToken() != TOK_MUL) {
      import->err = true;
      return import;
    }

    import->endOnDemand = lexer->getCursor() - 1;
    if (import->posTokStatic > 0) {
      import->type = STATIC_IMPORT_ON_DEMAND_DECLARATION;
    } else {
      import->type = TYPE_IMPORT_ON_DEMAND_DECLARATION;
    }

    lexer->getNextToken(); // consume '*'
  }

  if (lexer->getCurToken() != TOK_SEMICOLON) {
    import->err = true;
    return import;
  }

  return import;
}

/// IntegerLiteral:
///   DecimalIntegerLiteral
///   HexIntegerLiteral
///   OctalIntegerLiteral
///   BinaryIntegerLiteral
void Parser::parseIntegerLiteral(spIntegerLiteral &intLiteral) {
  if (isDecimalIntegerLiteral(lexer->getCurToken())) {
    intLiteral->opt = IntegerLiteral::OPT_DECIMAL;
    intLiteral->pos = lexer->getCurTokenIni();
    intLiteral->value = lexer->getCurTokenStr();
    if (lexer->getCurToken() == TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX) {
      intLiteral->intSuffix = true;
    }

    lexer->getNextToken(); // consume decimal
    return;
  }

  if (isHexIntegerLiteral(lexer->getCurToken())) {
    intLiteral->opt = IntegerLiteral::OPT_HEX;
    intLiteral->pos = lexer->getCurTokenIni();
    intLiteral->value = lexer->getCurTokenStr();
    if (lexer->getCurToken() == TOK_HEX_NUMERAL_WITH_INT_TYPE_SUFFIX) {
      intLiteral->intSuffix = true;
    }

    lexer->getNextToken(); // consume hex
    return;
  }

  if (isOctalIntegerLiteral(lexer->getCurToken())) {
    intLiteral->opt = IntegerLiteral::OPT_OCTAL;
    intLiteral->pos = lexer->getCurTokenIni();
    intLiteral->value = lexer->getCurTokenStr();
    if (lexer->getCurToken() == TOK_OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX) {
      intLiteral->intSuffix = true;
    }

    lexer->getNextToken(); // consume octal
    return;
  }

  if (isBinaryIntegerLiteral(lexer->getCurToken())) {
    intLiteral->opt = IntegerLiteral::OPT_BINARY;
    intLiteral->pos = lexer->getCurTokenIni();
    intLiteral->value = lexer->getCurTokenStr();
    if (lexer->getCurToken() == TOK_BINARY_NUMERAL_WITH_INT_TYPE_SUFFIX) {
      intLiteral->intSuffix = true;
    }

    lexer->getNextToken(); // consume bin
    return;
  }
}

/// FloatingPointLiteral:
///   DecimalFloatingPointLiteral
///   HexFloatingPointLiteral
void Parser::parseFloatingPointLiteral(spFloatingPointLiteral &fpLiteral) {
  if (lexer->getCurToken() == TOK_DECIMAL_FLOATING_POINT_LITERAL) {
    fpLiteral->opt = FloatingPointLiteral::OPT_DECIMAL;
    fpLiteral->pos = lexer->getCurTokenIni();
    fpLiteral->value = lexer->getCurTokenStr();
    lexer->getNextToken(); // consume decimal floating point
    return;
  }

  if (lexer->getCurToken() == TOK_HEXADECIMAL_FLOATING_POINT_LITERAL) {
    fpLiteral->opt = FloatingPointLiteral::OPT_HEX;
    fpLiteral->pos = lexer->getCurTokenIni();
    fpLiteral->value = lexer->getCurTokenStr();
    lexer->getNextToken(); // consume hex floating point
    return;
  }
}

/// Literal:
///   IntegerLiteral
///   FloatingPointLiteral
///   CharacterLiteral
///   StringLiteral
///   BooleanLiteral
///   NullLiteral
void Parser::parseLiteral(spLiteral &literal) {
  if (isIntegerLiteral(lexer->getCurToken())) {
    literal->opt = Literal::OPT_INTEGER;
    literal->intLiteral = spIntegerLiteral(new IntegerLiteral());
    parseIntegerLiteral(literal->intLiteral);
    return;
  }

  if (isFloatingPointLiteral(lexer->getCurToken())) {
    literal->opt = Literal::OPT_FLOATING_POINT;
    literal->fpLiteral = spFloatingPointLiteral(new FloatingPointLiteral());
    parseFloatingPointLiteral(literal->fpLiteral);
    return;
  }

  // TODO:
}

/// Primary:
///   Literal
///   ParExpression
///   this [Arguments]
///   super SuperSuffix
///   new Creator
///   NonWildcardTypeArguments
///     ( ExplicitGenericInvocationSuffix | this Arguments )
///   Identifier { . Identifier } [IdentifierSuffix]
///   BasicType {[]} . class
///   void . class
void Parser::parsePrimary(spPrimary &primary) {
  spLiteral literal = spLiteral(new Literal());
  parseLiteral(literal);
  if (literal->isEmpty() == false) {
    primary->opt = Primary::OPT_LITERAL;
    primary->literal = literal;
    return;
  }

  // TODO:
}

/// QualifiedIdentifier: Identifer { . Identifier }
spQualifiedIdentifier Parser::parseQualifiedIdentifier() {
  std::vector<spIdentifier> identifiers;
  // Save current identifier
  spIdentifier id = spIdentifier(
    new Identifier(lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  identifiers.push_back(id);

  State backup;
  while (true) {
    lexer->getNextToken();
    if (lexer->getCurToken() != TOK_PERIOD) {
      break;
    }

    // We have a period, if the next token is not an identifier we restore
    // the period token state and exit the while loop
    lexer->saveState(backup);
    lexer->getNextToken();
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      lexer->restoreState(backup);
      break;
    }

    // Save the identifier
    spIdentifier id = spIdentifier(
      new Identifier(lexer->getCurTokenIni(), lexer->getCurTokenStr()));
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

  while (isValidInitTokenOfTypeDeclaration(lexer->getCurToken())) {
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

  if (lexer->getCurToken() == TOK_KEY_CLASS
    || lexer->getCurToken() == TOK_KEY_ENUM) {

    decl->classDecl = spClassDeclaration(new ClassDeclaration());
    parseClassDeclaration(decl->classDecl);
    return;
  }

  // TODO:
  if (lexer->getCurToken() == TOK_KEY_INTERFACE) {
    lexer->getNextToken();
    return;
  }

  // TODO: handle error
}

void Parser::parseModifier(spModifier &modifier) {

  while (isModifierToken(lexer->getCurToken())) {
    // Annotations
    if (lexer->getCurToken() == TOK_ANNOTATION) {
      parseAnnotations(modifier->annotations);
      continue;
    }

    // Tokens
    spTokenExp token = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    modifier->tokens.push_back(token);
    lexer->getNextToken();
  }
}

/// ClassDeclaration: NormalClassDeclaration | EnumDeclaration
void Parser::parseClassDeclaration(spClassDeclaration &classDecl) {
  if (lexer->getCurToken() == TOK_KEY_CLASS) {
    classDecl->nClassDecl = spNormalClassDeclaration(
      new NormalClassDeclaration());
    parseNormalClassDeclaration(classDecl->nClassDecl);
    return;
  }

  // TODO:
  if (lexer->getCurToken() == TOK_KEY_ENUM) {
    lexer->getNextToken();
    return;
  }

  // TODO: handle error
  lexer->getNextToken();
}

/// NormalClassDeclaration:
///   class Identifier [TypeParameters] [extends Type] [implements TypeList]
///     ClassBody
void Parser::parseNormalClassDeclaration(spNormalClassDeclaration &nClassDecl) {
  // TODO: Handle error.
  if (lexer->getCurToken() != TOK_KEY_CLASS) {
    return;
  }

  nClassDecl->classTok = spTokenExp(new TokenExp(lexer->getCursor()
    - tokenUtil.getTokenLength(TOK_KEY_CLASS), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'class'

  // TODO: handle error
  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    return;
  }

  int pos = lexer->getCurTokenIni();
  nClassDecl->identifier = spIdentifier(new Identifier(
    pos, lexer->getCurTokenStr()));
  st.addSym(ST_CLASS, lexer->getCurToken(), pos, src->getLine(),
    lexer->getCurTokenStr());
  lexer->getNextToken(); // consume Identifier

  // TODO: [TypeParameters]
  // TODO: [extends Type]
  // TODO: [implements TypeList]

  nClassDecl->classBody = spClassBody(new ClassBody());
  parseClassBody(nClassDecl->classBody);
}

/// ClassBody: '{' { ClassBodyDeclaration } '}'
void Parser::parseClassBody(spClassBody &classBody) {
  // TODO: handle error
  if (lexer->getCurToken() != TOK_LCURLY_BRACKET) {
    return;
  }

  lexer->getNextToken(); // consume '{'

  // ClassBodyDeclaration
  while (isValidInitTokenOfClassBodyDeclaration(lexer->getCurToken())) {
    if (lexer->getCurToken() == TOK_SEMICOLON) {
      lexer->getNextToken(); // consume ';'
      continue;
    }

    spClassBodyDeclaration decl = spClassBodyDeclaration(
      new ClassBodyDeclaration());
    parseClassBodyDeclaration(decl);
    classBody->decls.push_back(decl);
  }

  if (lexer->getCurToken() != TOK_RCURLY_BRACKET) {
    return;
  }
  lexer->getNextToken(); // consume '}'
}

/// ClassBodyDeclaration:
///   ;
///   {Modifier} MemberDecl
///   [static] Block
void Parser::parseClassBodyDeclaration(spClassBodyDeclaration &decl) {
  if (isModifierToken(lexer->getCurToken())
      || lexer->getCurToken() == TOK_IDENTIFIER) {
    decl->opt = ClassBodyDeclaration::OPT_MODIFIER_MEMBER_DECL;
    parseModifier(decl->modifier);
    decl->memberDecl = spMemberDecl(new MemberDecl());
    parseMemberDecl(decl->memberDecl);
    return;
  }

  // TODO: [static] Block

  // TODO:
  lexer->getNextToken();
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
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    if (st.isConstructor(lexer->getCurTokenStr())) {
      memberDecl->opt = MemberDecl::OPT_IDENTIFIER_CONSTRUCTOR_DECLARATOR_REST;

      // Identifier
      int pos = lexer->getCurTokenIni();
      memberDecl->identifier
	= spIdentifier(new Identifier(pos, lexer->getCurTokenStr()));
      st.addSym(ST_CLASS, lexer->getCurToken(), pos, src->getLine(),
        lexer->getCurTokenStr());
      lexer->getNextToken(); // consume Identifier

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
  lexer->getNextToken();
}

/// CompilationUnit: Top level parsing.
///   [PackageDeclaration] [ImportDeclaration] [TypeDeclarations]
void Parser::parseCompilationUnit() {
  std::vector<spAnnotation> annotations;
  if (lexer->getCurToken() == TOK_ANNOTATION) {
    parseAnnotations(annotations);
  }

  if (lexer->getCurToken() == TOK_KEY_PACKAGE) {
    compilationUnit->pkgDecl = parsePackageDeclaration(annotations);
  }

  // Import Declaration
  if (lexer->getCurToken() == TOK_KEY_IMPORT) {
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

  // FormalParameters
  constDeclRest->formParams = spFormalParameters(new FormalParameters());
  parseFormalParameters(constDeclRest->formParams);

  // TODO:
  // [throws QualifiedIdentifierList] Block
  lexer->getNextToken();
}

/// FormalParameters: ( [FormalParameterDecls] )
void Parser::parseFormalParameters(spFormalParameters &formParams) {
  if (lexer->getCurToken() != TOK_LPAREN) {
    addError(ERR_EXP_LPAREN);
    formParams->error = 1;
    return;
  }
  lexer->getNextToken(); // consume '('

  // If our current token is a closing paren we're done and we skip trying
  // to parse FormalParameterDecls.
  if (lexer->getCurToken() == TOK_RPAREN) {
    lexer->getNextToken(); // consume ')'
    return;
  }

  formParams->formParamDecls = spFormalParameterDecls(
    new FormalParameterDecls());
  parseFormalParameterDecls(formParams->formParamDecls);

  if (lexer->getCurToken() != TOK_RPAREN) {
    addError(ERR_EXP_RPAREN);
    formParams->error = 1;
    return;
  }
  lexer->getNextToken(); // consume ')'
}

/// FormalParameterDecls: {VariableModifier} Type FormalParameterDeclsRest
void Parser::parseFormalParameterDecls(spFormalParameterDecls &formParamDecls) {
  // {VariableModifier}
  formParamDecls->varModifier = spVariableModifier(new VariableModifier());
  parseVariableModifier(formParamDecls->varModifier);

  // Type
  formParamDecls->type = spType(new Type());
  parseType(formParamDecls->type);

  // At this point if we have a VariableModifier and no Type we're in an
  // inconsistent state. For example:
  // Constructor(final var) or Constructor(@Annot var).
  if (formParamDecls->varModifier->isEmpty() == false
    && formParamDecls->type->isEmpty()) {
    addError(ERR_EXP_TYPE);
  }

  // If we have a Type we expect a FormalParameterDeclRest
  if (formParamDecls->type->isEmpty() == false) {
    formParamDecls->formParamDeclsRest = spFormalParameterDeclsRest(
      new FormalParameterDeclsRest());
    parseFormalParameterDeclsRest(formParamDecls->formParamDeclsRest);
  }
}

/// VariableModifier:
///   final
///   Annotation
/// One 'final' keyword is allowed, while we can have zero or more annotations.
void Parser::parseVariableModifier(spVariableModifier &varModifier) {

  while (lexer->getCurToken() == TOK_KEY_FINAL
    || lexer->getCurToken() == TOK_ANNOTATION) {

    if (lexer->getCurToken() == TOK_KEY_FINAL) {
      if (varModifier->tokFinal) {
        // TODO: Handle error. We already have a 'final' token.
      } else {
	varModifier->tokFinal = spTokenExp(new TokenExp(
          lexer->getCursor() - tokenUtil.getTokenLength(
            lexer->getCurToken()), lexer->getCurToken()));
      }

      lexer->getNextToken(); // consume 'final'
    }

    // Add annotations to varModifier->annotations
    if (lexer->getCurToken() == TOK_ANNOTATION) {
      parseAnnotations(varModifier->annotations);
    }
  }
}

/// Type:
///   BasicType {[]}
///   ReferenceType {[]}
void Parser::parseType(spType &type) {
  if (isBasicType(lexer->getCurToken())) {
    spTokenExp token = spTokenExp(new TokenExp(lexer->getCursor()
      - tokenUtil.getTokenLength(lexer->getCurToken()), lexer->getCurToken()));
    type->opt = Type::OPT_BASIC_TYPE;
    type->basicType = spBasicType(new BasicType(token));
    lexer->getNextToken(); // consume basic type
    type->arrayDepth = parseArrayDepth();
    return;
  }

  // TODO: ReferenceType
}

/// FormalParameterDeclsRest:
///   VariableDeclaratorId [ , FormalParameterDecls ]
///   ... VariableDeclaratorId
void Parser::parseFormalParameterDeclsRest(
  spFormalParameterDeclsRest &formParamDeclsRest) {

  formParamDeclsRest->varDeclId = spVariableDeclaratorId(
    new VariableDeclaratorId());

  if (lexer->getCurToken() == TOK_ELLIPSIS) {
    formParamDeclsRest->opt = FormalParameterDeclsRest::OPT_VAR_ARITY;
    lexer->getNextToken(); // consume '...'

    // We expect a VariableDeclaratorId
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      addError(ERR_EXP_IDENTIFIER);
      return;
    }

    parseVariableDeclaratorId(formParamDeclsRest->varDeclId);

    // Corner case in the grammar. The array form is invalid in the form:
    // (int ... a[])
    if (formParamDeclsRest->varDeclId->arrayDepth > 0) {
      addError(ERR_NVAL_ARRAY);
    }

    return;
  }

  formParamDeclsRest->opt = FormalParameterDeclsRest::OPT_VAR_DECL_ID;

  // VariableDeclaratorId
  parseVariableDeclaratorId(formParamDeclsRest->varDeclId);

  // Handle error
  if (formParamDeclsRest->varDeclId->identifier->value.length() == 0) {
    addError(ERR_EXP_IDENTIFIER);
  }

  // [ , FormalParameterDecls ]
  if (lexer->getCurToken() == TOK_COMMA) {
    lexer->getNextToken(); // consume ','

    formParamDeclsRest->formParamDecls = spFormalParameterDecls(
      new FormalParameterDecls());
    parseFormalParameterDecls(formParamDeclsRest->formParamDecls);
  }
}

/// VariableDeclaratorId: Identifier {[]}
void Parser::parseVariableDeclaratorId(spVariableDeclaratorId &varDeclId) {
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    varDeclId->identifier = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier
    varDeclId->arrayDepth = parseArrayDepth();
  }
}

void Parser::parse() {
  lexer->getNextToken();
  while (true) {
    switch (lexer->getCurToken()) {
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
