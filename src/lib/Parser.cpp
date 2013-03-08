#include "djp/Parser.h"

namespace djp {

// Helper functions
bool isAssignmentOperator(int token) {
  if (TOK_OP_EQUALS == token
    || TOK_OP_PLUS_EQUALS == token
    || TOK_OP_MINUS_EQUALS == token
    || TOK_OP_MUL_EQUALS == token
    || TOK_OP_DIV_EQUALS == token
    || TOK_OP_AMPERSAND_EQUALS == token
    || TOK_OP_PIPE_EQUALS == token
    || TOK_OP_CARRET_EQUALS == token
    || TOK_OP_REM_EQUALS == token
    || TOK_OP_LSHIFT_EQUALS == token
    || TOK_OP_RSHIFT_EQUALS == token
    || TOK_OP_TRIPLE_RSHIFT_EQUALS == token) {
    return true;
  }

  return false;
}

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

bool isLiteral(int token) {
  return (isIntegerLiteral(token)
    || isFloatingPointLiteral(token)
    || token == TOK_CHARACTER_LITERAL
    || token == TOK_STRING_LITERAL
    || token == TOK_BOOLEAN_LITERAL
    || token == TOK_NULL_LITERAL);
}

bool isPrimary(int token) {
  return (token == TOK_OP_LT
    || token == TOK_LCURLY_BRACKET
    || token == TOK_KEY_THIS
    || token == TOK_KEY_SUPER
    || token == TOK_KEY_NEW
    || token == TOK_IDENTIFIER
    || isBasicType(token)
    || token == TOK_KEY_VOID
    || isLiteral(token)
    || token == TOK_LPAREN);
}

bool isClassOrInterfaceDeclarationCandidate(int token) {
  if (isClassOrInterfaceModifier(token)) {
    return true;
  }

  if (TOK_KEY_CLASS == token
    || TOK_KEY_ENUM == token
    || TOK_KEY_INTERFACE == token
    || TOK_ANNOTATION_TYPE_DECLARATION == token) {
    return true;
  }

  return false;
}

/// ClassModifier: one of
///   Annotation public protected private
///   abstract static final strictfp
bool isClassOrInterfaceModifier(int token) {
  if (TOK_ANNOTATION == token
    || TOK_KEY_PUBLIC == token
    || TOK_KEY_PROTECTED == token
    || TOK_KEY_PRIVATE == token
    || TOK_KEY_ABSTRACT == token
    || TOK_KEY_STATIC == token
    || TOK_KEY_FINAL == token
    || TOK_KEY_STRICTFP == token) {
    return true;
  }

  return false;
}

/// ConstructorModifier: one of
///   Annotation public protected private
///
/// Modifier:
///   Annotation
///   public
///   protected
///   private
///   static
///   abstract
///   final
///   native
///   synchronized
///   transient
///   volatile
///   strictfp
bool isModifierToken(int token) {
  if (TOK_KEY_PUBLIC == token
    || TOK_KEY_PROTECTED == token
    || TOK_KEY_PRIVATE == token
    || TOK_KEY_ABSTRACT == token
    || TOK_KEY_STATIC == token
    || TOK_KEY_FINAL == token
    || TOK_KEY_STRICTFP == token
    || TOK_ANNOTATION == token
    || TOK_KEY_NATIVE == token
    || TOK_KEY_SYNCHRONIZED == token
    || TOK_KEY_TRANSIENT == token
    || TOK_KEY_VOLATILE == token) {
    return true;
  }

  return false;
}

bool isInfixOp(int token) {
  if (TOK_OP_PIPE_PIPE == token
    || TOK_OP_AMPERSAND_AMPERSAND == token
    || TOK_OP_PIPE == token
    || TOK_OP_CARRET == token
    || TOK_OP_AMPERSAND == token
    || TOK_OP_EQUALS_EQUALS == token
    || TOK_OP_EXCLAMATION_EQUALS == token
    || TOK_OP_LT == token
    || TOK_OP_GT == token
    || TOK_OP_LT_EQUALS == token
    || TOK_OP_GT_EQUALS == token
    || TOK_OP_LSHIFT == token
    || TOK_OP_RSHIFT == token
    || TOK_OP_TRIPLE_RSHIFT == token
    || TOK_OP_PLUS == token
    || TOK_OP_MINUS == token
    || TOK_OP_MUL == token
    || TOK_OP_DIV == token
    || TOK_OP_REM == token) {
    return true;
  }

  return false;
}

bool isModifierOrMemberMemberDeclCandidate(int token) {
  // Prior to a MemberDecl
  if (isModifierToken(token)) {
    return true;
  }

  // MemberDecl
  if (TOK_IDENTIFIER == token
    || isBasicType(token)
    || TOK_KEY_VOID == token
    || TOK_KEY_CLASS == token
    || TOK_KEY_ENUM == token
    || TOK_KEY_INTERFACE == token
    || TOK_ANNOTATION_TYPE_DECLARATION == token) {
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
  if (isModifierOrMemberMemberDeclCandidate(token)) {
    return true;
  }

  // Prior to a Block
  if (TOK_KEY_STATIC == token || TOK_LCURLY_BRACKET == token) {
    return true;
  }

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
    || TOK_ANNOTATION_TYPE_DECLARATION == token
    || TOK_KEY_CLASS == token
    || TOK_KEY_INTERFACE == token
    || TOK_KEY_ENUM == token) {

    return true;
  }

  return false;
}

bool isVariableModifier(int token) {
  if (TOK_KEY_FINAL == token || TOK_ANNOTATION == token) {
    return true;
  }

  return false;
}

// Helper methods
void Parser::saveState(State &state) {
  state.diagErrorsSize = diag->errors.size();
  lexer->saveState(state);
}

void Parser::restoreState(State &state) {
  while (diag->errors.size() > state.diagErrorsSize) {
    diag->errors.pop_back();
  }

  lexer->restoreState(state);
}

/// Annotation: @ QualifiedIdentifier [ ( [AnnotationElement] ) ]
spAnnotation Parser::parseAnnotation() {
  spAnnotation annotation = spAnnotation(new Annotation);
  annotation->posTokAt = lexer->getCursor() - 1;
  lexer->getNextToken(); // Consume '@'

  // QualifiedIdentifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    annotation->err = true;
    diag->addErr(ERR_EXP_QID, annotation->posTokAt);
    return annotation;
  }


  annotation->qualifiedId = spQualifiedIdentifier(new QualifiedIdentifier);
  parseQualifiedIdentifier(annotation->qualifiedId);

  // If the current token is '(' we consume the token and expect
  // an optional AnnotaionElement followed by ')'
  if (lexer->getCurToken() == TOK_LPAREN) {
    int openParenPos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '('

    // Empty annotation element
    if (lexer->getCurToken() != TOK_RPAREN) {
      annotation->elem = spAnnotationElement(new AnnotationElement);
      parseAnnotationElement(annotation->elem);
      if (annotation->elem->err) {
        annotation->err = true;
        diag->addErr(ERR_NVAL_ANNOT_ELEM, annotation->posTokAt, openParenPos);
        return annotation;
      }
    }

    if (lexer->getCurToken() != TOK_RPAREN) {
      annotation->err = true;
      diag->addErr(ERR_EXP_LPAREN, annotation->posTokAt, openParenPos);
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
  elem->opt = AnnotationElement::OPT_ELEMENT_VALUE;
  elem->value = spElementValue(new ElementValue);
  parseElementValue(elem->value);
}

/// AnnotationMethodOrConstantRest:
///   AnnotationMethodRest
///   ConstantDeclaratorsRest
void Parser::parseAnnotationMethodOrConstantRest(
  spAnnotationMethodOrConstantRest &methodOrConstRest) {

  // AnnotationMethodRest
  if (lexer->getCurToken() == TOK_LPAREN) {
    methodOrConstRest->opt
      = AnnotationMethodOrConstantRest::OPT_ANNOTATION_METHOD_REST;
    methodOrConstRest->methRest = spAnnotationMethodRest(
      new AnnotationMethodRest);
    parseAnnotationMethodRest(methodOrConstRest->methRest);
    if (methodOrConstRest->methRest->err) {
      methodOrConstRest->addErr(-1);
    }

    return;
  }

  // ConstantDeclaratorsRest
  if (lexer->getCurToken() == TOK_LBRACKET
    || lexer->getCurToken() == TOK_OP_EQUALS) {

    methodOrConstRest->opt
      = AnnotationMethodOrConstantRest::OPT_CONSTANT_DECLARATORS_REST;
    methodOrConstRest->constRest = spConstantDeclaratorsRest(
      new ConstantDeclaratorsRest);
    parseConstantDeclaratorsRest(methodOrConstRest->constRest);
    if (methodOrConstRest->constRest->err) {
      methodOrConstRest->addErr(-1);
    }

    return;
  }

  // Error
  methodOrConstRest->addErr(-1);
}

/// AnnotationMethodRest:
///   '()' ['[]'] [default ElementValue]
void Parser::parseAnnotationMethodRest(spAnnotationMethodRest &methRest) {
  // '('
  if (lexer->getCurToken() != TOK_LPAREN) {
    methRest->addErr(diag->addErr(ERR_EXP_LPAREN, lexer->getCursor() - 1));
    return;
  }

  methRest->posLParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '('

  if (lexer->getCurToken() != TOK_RPAREN) {
    methRest->addErr(diag->addErr(ERR_EXP_RPAREN, lexer->getCursor() - 1));
    return;
  }

  methRest->posRParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ')'

  // ['[]']
  if (lexer->getCurToken() == TOK_LBRACKET) {
    methRest->posLBracket = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '['

    if (lexer->getCurToken() != TOK_RBRACKET) {
      methRest->addErr(diag->addErr(ERR_EXP_RBRACKET, lexer->getCursor() - 1));
      return;
    }

    methRest->posRBracket = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ']'
  }

  // [default ElementValue]
  if (lexer->getCurToken() != TOK_KEY_DEFAULT) {
    return;
  }

  methRest->tokDefault = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
    lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'default'

  methRest->elemVal = spElementValue(new ElementValue);
  parseElementValue(methRest->elemVal);
  if (methRest->elemVal->err) {
    methRest->addErr(-1);
  }
}

/// Annotations.
void Parser::parseAnnotations(std::vector<spAnnotation> &annotations) {
  while (lexer->getCurToken() == TOK_ANNOTATION) {
    spAnnotation annotation = parseAnnotation();
    annotations.push_back(annotation);
  }
}

/// AnnotationTypeBody:
///   [AnnotationTypeElementDeclarations]
void Parser::parseAnnotationTypeBody(spAnnotationTypeBody &annTypeBody) {
  annTypeBody->elemDecls = spAnnotationTypeElementDeclarations(
    new AnnotationTypeElementDeclarations);

  State state;
  unsigned pos = 0;
  while (pos != lexer->getCursor()) {
    pos = lexer->getCursor();
    saveState(state);
    spAnnotationTypeElementDeclaration elemDecl =
      spAnnotationTypeElementDeclaration(new AnnotationTypeElementDeclaration);
    parseAnnotationTypeElementDeclaration(elemDecl);
    if (elemDecl->err) {
      restoreState(state);
      return;
    }

    annTypeBody->elemDecls->elemDecls.push_back(elemDecl);
  }
}

/// AnnotationTypeDeclaration:
///   @ interface Identifier AnnotationTypeBody
void Parser::parseAnnotationTypeDeclaration(
  spAnnotationTypeDeclaration &annotationDecl) {

  // @
  annotationDecl->posAt = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '@'

  // interface
  if (lexer->getCurToken() != TOK_KEY_INTERFACE) {
    annotationDecl->addErr(diag->addErr(
      ERR_EXP_INTERFACE, lexer->getCursor() - 1));
    return;
  }

  annotationDecl->tokInterface = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
    lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'interface'

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    annotationDecl->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  // AnnotationTypeBody
  annotationDecl->annTypeBody = spAnnotationTypeBody(new AnnotationTypeBody);
  parseAnnotationTypeBody(annotationDecl->annTypeBody);
  if (annotationDecl->annTypeBody->err) {
    annotationDecl->addErr(-1);
  }
}

/// AnnotationTypeElementDeclaration:
///   {Modifier} AnnotationTypeElementRest
void Parser::parseAnnotationTypeElementDeclaration(
  spAnnotationTypeElementDeclaration &elemDecl) {

  elemDecl->modifier = spModifier(new Modifier);
  parseModifier(elemDecl->modifier);

  elemDecl->elemRest = spAnnotationTypeElementRest(
    new AnnotationTypeElementRest);
  parseAnnotationTypeElementRest(elemDecl->elemRest);
  if (elemDecl->elemRest->err) {
    elemDecl->addErr(-1);
  }
}

/// AnnotationTypeElementRest:
///   (1) Type Identifier AnnotationMethodOrConstantRest ;
///   (2) ClassDeclaration
///   (3) InterfaceDeclaration
///   (4) EnumDeclaration
///   (5) AnnotationTypeDeclaration
void Parser::parseAnnotationTypeElementRest(
  spAnnotationTypeElementRest &elemRest) {

  // (1) Type Identifier AnnotationMethodOrConstantRest ;
  if (lexer->getCurToken() == TOK_IDENTIFIER
    || isBasicType(lexer->getCurToken())) {

    elemRest->opt = AnnotationTypeElementRest::OPT_METHOD_OR_CONSTANT;

    // Type
    elemRest->type = spType(new Type);
    parseType(elemRest->type);
    if (elemRest->type->err) {
      elemRest->addErr(-1);
      return;
    }

    // Identifier
    elemRest->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume 'Identifier'

    // AnnotationMethodOrConstantRest
    elemRest->methodOrConstRest = spAnnotationMethodOrConstantRest(
      new AnnotationMethodOrConstantRest);
    parseAnnotationMethodOrConstantRest(elemRest->methodOrConstRest);
    if (elemRest->methodOrConstRest->err) {
      elemRest->addErr(-1);
    }

    // ';'
    if (lexer->getCurToken() != TOK_SEMICOLON) {
      elemRest->addErr(diag->addErr(
        ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
      return;
    }

    elemRest->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'

    return;
  }

  // (2) ClassDeclaration
  if (lexer->getCurToken() == TOK_KEY_CLASS) {
    elemRest->opt = AnnotationTypeElementRest::OPT_CLASS_DECLARATION;
    elemRest->classDecl = spClassDeclaration(new ClassDeclaration);
    parseClassDeclaration(elemRest->classDecl);
    if (elemRest->classDecl->err) {
      elemRest->addErr(-1);
    }
    return;
  }

  // (3) InterfaceDeclaration
  if (lexer->getCurToken() == TOK_KEY_INTERFACE) {
    elemRest->opt = AnnotationTypeElementRest::OPT_INTERFACE_DECLARATION;
    elemRest->interfaceDecl = spInterfaceDeclaration(new InterfaceDeclaration);
    parseInterfaceDeclaration(elemRest->interfaceDecl);
    if (elemRest->interfaceDecl->err) {
      elemRest->addErr(-1);
    }
    return;
  }

  // (4) EnumDeclaration
  if (lexer->getCurToken() == TOK_KEY_ENUM) {
    elemRest->opt = AnnotationTypeElementRest::OPT_ENUM_DECLARATION;
    elemRest->enumDecl = spEnumDeclaration(new EnumDeclaration);
    parseEnumDeclaration(elemRest->enumDecl);
    if (elemRest->enumDecl->err) {
      elemRest->addErr(-1);
    }
    return;
  }

  // (5) AnnotationTypeDeclaration
  if (lexer->getCurToken() == TOK_ANNOTATION_TYPE_DECLARATION) {
    elemRest->opt = AnnotationTypeElementRest::OPT_ANNOTATION_DECLARATION;
    elemRest->annotationDecl = spAnnotationTypeDeclaration(
      new AnnotationTypeDeclaration);
    if (elemRest->annotationDecl->err) {
      elemRest->addErr(-1);
    }
    return;
  }

  // Error
  elemRest->addErr(-1);
}

/// Arguments: '(' [ Expression { , Expression }] ')'
void Parser::parseArguments(spArguments &args) {
  if (lexer->getCurToken() != TOK_LPAREN) {
    args->addErr(diag->addErr(ERR_EXP_ARGUMENTS, lexer->getCursor() - 1));
    return;
  }

  args->posLParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '('

  if (lexer->getCurToken() == TOK_RPAREN) {
    args->posRParen = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ')'
    return;
  }

  // Expression
  args->expr = spExpression(new Expression);
  parseExpression(args->expr);
  if (args->expr->isEmpty()) {
    return;
  }

  // { , Expression }
  while (lexer->getCurToken() == TOK_COMMA) {
    unsigned int posComma = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','

    spExpression expr = spExpression(new Expression);
    parseExpression(expr);
    if (expr->isEmpty()) {
      break;
    }

    std::pair<unsigned int, spExpression> pair;
    pair.first = posComma;
    pair.second = expr;
    args->exprs.push_back(pair);
  }

  if (lexer->getCurToken() == TOK_RPAREN) {
    args->posRParen = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ')'
    return;
  }

  // Error
  args->addErr(diag->addErr(ERR_EXP_RCURLY_BRACKET,
    lexer->getCurTokenIni(), lexer->getCursor()));
}

/// ArrayCreatorRest:
///   '['
///     ( ']' { '[]' } ArrayInitializer |
///       Expression ']' { '[' Expression ']' } { '[]' } )
///
/// Non-terminals are enclosed in square brackets.
void Parser::parseArrayCreatorRest(spArrayCreatorRest &arrayCreatorRest) {
  // We look ahead to decide if we have option 1 or option 2
  State openBracketState;
  saveState(openBracketState);
  lexer->getNextToken(); // consume '['
  int lookahead = lexer->getCurToken();
  restoreState(openBracketState);

  // Option 1
  if (lookahead == TOK_RBRACKET) {
    arrayCreatorRest->opt = ArrayCreatorRest::OPT_ARRAY_INITIALIZER;
    arrayCreatorRest->opt1 = spArrayCreatorRestOpt1(new ArrayCreatorRestOpt1());
    parseArrayCreatorRestOpt1(arrayCreatorRest->opt1);
    if (arrayCreatorRest->opt1->err) {
      arrayCreatorRest->addErr(-1);
    }
    return;
  }

  // Option 2
  arrayCreatorRest->opt = ArrayCreatorRest::OPT_EXPRESSION;
  arrayCreatorRest->opt2 = spArrayCreatorRestOpt2(new ArrayCreatorRestOpt2);
  parseArrayCreatorRestOpt2(arrayCreatorRest->opt2);
  if (arrayCreatorRest->opt2->err) {
    arrayCreatorRest->addErr(-1);
  }
}

/// ArrayCreatorRestOpt1:
///   '[' ']' { '[]' } ArrayInitializer
void Parser::parseArrayCreatorRestOpt1(spArrayCreatorRestOpt1 &opt1) {
  // Array Depth
  parseArrayDepth(opt1->arrayDepth);

  if (opt1->arrayDepth.size() < 1) {
    opt1->addErr(diag->addErr(ERR_EXP_ARRAY, lexer->getCursor() - 1));
    return;
  }

  if (lexer->getCurToken() == TOK_RCURLY_BRACKET) {
    opt1->addErr(diag->addErr(ERR_EXP_LCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  // ArrayInitializer
  opt1->arrayInitializer = spArrayInitializer(new ArrayInitializer());
  parseArrayInitializer(opt1->arrayInitializer);

  if (opt1->arrayInitializer->err) { opt1->addErr(-1); }
}

/// ArrayCreatorRestOpt2:
///   '[' Expression ']' { '[' Expression ']' } { '[]' }
void Parser::parseArrayCreatorRestOpt2(spArrayCreatorRestOpt2 &opt2) {
  // '[' Expression ']'
  opt2->exprInBrackets = spExpressionInBrackets(new ExpressionInBrackets);

  if (lexer->getCurToken() != TOK_LBRACKET) {
    opt2->addErr(diag->addErr(ERR_EXP_LBRACKET, lexer->getCursor() - 1));
    return;
  }

  opt2->exprInBrackets->posLBracket = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '['

  opt2->exprInBrackets->expr = spExpression(new Expression);
  parseExpression(opt2->exprInBrackets->expr);
  if (opt2->exprInBrackets->expr->isEmpty()) {
    opt2->addErr(-1);
    return;
  }

  if (lexer->getCurToken() != TOK_RBRACKET) {
    opt2->addErr(diag->addErr(ERR_EXP_RBRACKET, lexer->getCursor() - 1));
    return;
  }

  opt2->exprInBrackets->posRBracket = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ']'

  // Expression in brackets list
  State openBracketState;
  while (lexer->getCurToken() == TOK_LBRACKET) {
    saveState(openBracketState);

    spExpressionInBrackets exprInBr
      = spExpressionInBrackets(new ExpressionInBrackets);
    exprInBr->posLBracket = lexer->getCurTokenIni();
    lexer->getNextToken(); // consume '['

    if (lexer->getCurToken() == TOK_RBRACKET) {
      // We have an empty array so we restore the state and break out.
      restoreState(openBracketState);
      break;
    }

    // Our only option is an expression in brackets.
    exprInBr->expr = spExpression(new Expression);
    parseExpression(exprInBr->expr);
    if (exprInBr->expr->isEmpty()) {
      opt2->addErr(-1);
      return;
    }

    if (lexer->getCurToken() != TOK_RBRACKET) {
      opt2->addErr(diag->addErr(ERR_EXP_RBRACKET, lexer->getCursor() - 1));
      return;
    }

    exprInBr->posRBracket = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ']'

    opt2->exprInBracketsList.push_back(exprInBr);
  }

  // Array Depth
  if (lexer->getCurToken() == TOK_LBRACKET) {
    parseArrayDepth(opt2->arrayDepth);
  }
}

/// {[]}
void Parser::parseArrayDepth(ArrayDepth &arrayDepth) {
  while (lexer->getCurToken() == TOK_LBRACKET) {
    State state;
    saveState(state);

    ArrayPair pair;
    pair.first = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '['

    if (lexer->getCurToken() != TOK_RBRACKET) {
      // We only consume valid pair of brackets
      restoreState(state);
      return;
    }

    pair.second = lexer->getCursor() - 1;
    arrayDepth.push_back(pair);

    lexer->getNextToken(); // consume ']'
  }
}

/// ArrayInitializer:
///   '{' [ VariableInitializer { , VariableInitializer } [,] ] '}'
void Parser::parseArrayInitializer(spArrayInitializer &arrayInit) {
  // '{'
  if (lexer->getCurToken() != TOK_LCURLY_BRACKET) {
    arrayInit->addErr(diag->addErr(
      ERR_EXP_LCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  arrayInit->posLCBrace = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '{'

  // ',}'
  if (lexer->getCurToken() == TOK_COMMA) {
    arrayInit->posComma = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','

    if (lexer->getCurToken() != TOK_RCURLY_BRACKET) {
      arrayInit->addErr(diag->addErr(
        ERR_EXP_RCURLY_BRACKET, lexer->getCursor() - 1));
      return;
    }

    arrayInit->posRCBrace = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '}'
    return;
  }

  // '}'
  if (lexer->getCurToken() == TOK_RCURLY_BRACKET) {
    arrayInit->posRCBrace = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '}'
    return;
  }

  // VariableInitializer { , VariableInitializer } [,] '}'
  arrayInit->varInit = spVariableInitializer(new VariableInitializer);
  parseVariableInitializer(arrayInit->varInit);
  if (arrayInit->varInit->err) { arrayInit->addErr(-1); }

  while (lexer->getCurToken() == TOK_COMMA) {
    unsigned posComma = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','

    // ',}'
    if (lexer->getCurToken() == TOK_RCURLY_BRACKET) {
      arrayInit->posComma = posComma;
      arrayInit->posRCBrace = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume '}'
      return;
    }

    // pair: comma and variable initializer
    spVariableInitializer varInit
      = spVariableInitializer(new VariableInitializer);
    parseVariableInitializer(varInit);
    if (varInit->err) {
      arrayInit->addErr(-1);
      return;
    }

    arrayInit->pairs.push_back(std::make_pair(posComma, varInit));
  }

  // '}'
  if (lexer->getCurToken() != TOK_RCURLY_BRACKET) {
    arrayInit->addErr(diag->addErr(
      ERR_EXP_RCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  arrayInit->posRCBrace = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '}'
}

/// ElementValue: Annotation | Expression1 | ElementValueArrayInitializer
void Parser::parseElementValue(spElementValue &value) {
  if (lexer->getCurToken() == TOK_ANNOTATION) {
    value->opt = ElementValue::OPT_ANNOTATION;
    value->annotation = parseAnnotation();
    return;
  }

  // ElementValueArrayInitializer
  if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
    value->opt = ElementValue::OPT_ELEMENT_VALUE_ARRAY_INITIALIZER;
    value->elemValArrayInit = spElementValueArrayInitializer(
      new ElementValueArrayInitializer);
    parseElementValueArrayInitializer(value->elemValArrayInit);
    if (value->elemValArrayInit->err) {
      value->addErr(-1);
    }
    return;
  }

  // Expression1
  spExpression1 expr1 = spExpression1(new Expression1);
  parseExpression1(expr1);
  if (expr1->isEmpty() == false) {
    value->opt = ElementValue::OPT_EXPRESSION1;
    value->expr1 = expr1;
    return;
  }

  value->addErr(-1);
}

/// ElementValues:
///   ElementValue { , ElementValue }
void Parser::parseElementValues(spElementValues &values) {
  values->elemVal = spElementValue(new ElementValue);
  parseElementValue(values->elemVal);
  if (values->elemVal->err) {
    values->addErr(-1);
    return;
  }

  State state;
  while (lexer->getCurToken() == TOK_COMMA) {
    saveState(state);
    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','
    spElementValue val = spElementValue(new ElementValue);
    parseElementValue(val);
    if (val->err) {
      restoreState(state);
      values->addErr(-1);
      return;
    }

    values->pairs.push_back(std::make_pair(pos, val));
  }
}

/// ElementValueArrayInitializer: '{' [ElementValues] [,] '}'
void Parser::parseElementValueArrayInitializer(
  spElementValueArrayInitializer &elemValArrayInit) {

  // '{'
  if (lexer->getCurToken() != TOK_LCURLY_BRACKET) {
    elemValArrayInit->addErr(diag->addErr(
      ERR_EXP_LCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  elemValArrayInit->posLCBrace = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '{'

  // [ ElementValues ]
  if (lexer->getCurToken() != TOK_COMMA
    && lexer->getCurToken() != TOK_RCURLY_BRACKET) {
    elemValArrayInit->elemVals = spElementValues(new ElementValues);
    parseElementValues(elemValArrayInit->elemVals);
    if (elemValArrayInit->elemVals->err) {
      elemValArrayInit->addErr(-1);
    }
  }

  // [,]
  if (lexer->getCurToken() == TOK_COMMA) {
    elemValArrayInit->posComma = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','
  }

  // '}'
  if (lexer->getCurToken() != TOK_RCURLY_BRACKET) {
    elemValArrayInit->addErr(diag->addErr(
      ERR_EXP_RCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  elemValArrayInit->posRCBrace = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '}'
}

/// EnumBody:
///   '{' [EnumConstants] [,] [EnumBodyDeclarations] '}'
void Parser::parseEnumBody(spEnumBody &enumBody) {
  // '{'
  if (lexer->getCurToken() != TOK_LCURLY_BRACKET) {
    enumBody->addErr(diag->addErr(
      ERR_EXP_LCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  enumBody->posLCBrace = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '{'

  // EnumConstants
  if (lexer->getCurToken() == TOK_ANNOTATION
    || lexer->getCurToken() == TOK_IDENTIFIER) {

    enumBody->enumConsts = spEnumConstants(new EnumConstants);
    parseEnumConstants(enumBody->enumConsts);
    if (enumBody->enumConsts->err) {
      enumBody->addErr(-1);
      return;
    }
  }

  if (lexer->getCurToken() == TOK_COMMA) {
    enumBody->posComma = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','
  }

  // EnumBodyDeclarations
  if (lexer->getCurToken() == TOK_SEMICOLON) {
    enumBody->bodyDecls = spEnumBodyDeclarations(new EnumBodyDeclarations);
    parseEnumBodyDeclarations(enumBody->bodyDecls);
    if (enumBody->bodyDecls->err) {
      enumBody->addErr(-1);
    }
  }

  // '}'
  if (lexer->getCurToken() != TOK_RCURLY_BRACKET) {
    enumBody->addErr(diag->addErr(
      ERR_EXP_RCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  enumBody->posRCBrace = lexer->getCursor() - 1;
  st.updateScopeEnd(lexer->getCursor());
  lexer->getNextToken(); // consume '}'
}

/// EnumBodyDeclarations:
///   ; {ClassBodyDeclaration}
void Parser::parseEnumBodyDeclarations(spEnumBodyDeclarations &bodyDecls) {
  // ';'
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    bodyDecls->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
    return;
  }

  bodyDecls->posSemiColon = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ';'

  // {ClassBodyDeclaration}
  parseClassBodyDeclarationsHelper(bodyDecls->classBodyDecls);
}

/// EnumConstant:
///   [Annotations] Identifier [Arguments] [ClassBody]
void Parser::parseEnumConstant(spEnumConstant &enumConst) {
  // [Annotations]
  parseAnnotations(enumConst->annotations);

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    enumConst->addErr(diag->addErr(ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  enumConst->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume 'Identifier'

  // [Arguments]
  if (lexer->getCurToken() == TOK_LPAREN) {
    enumConst->args = spArguments(new Arguments);
    parseArguments(enumConst->args);
    if (enumConst->args->err) {
      enumConst->addErr(-1);
      return;
    }
  }

  // [ClassBody]
  if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
    enumConst->classBody = spClassBody(new ClassBody);
    parseClassBody(enumConst->classBody);
    if (enumConst->classBody->err) {
      enumConst->addErr(-1);
      return;
    }
  }
}

/// EnumConstants:
///   EnumConstant
///   EnumConstants , EnumConstant
void Parser::parseEnumConstants(spEnumConstants &enumConsts) {
  enumConsts->enumConst = spEnumConstant(new EnumConstant);
  parseEnumConstant(enumConsts->enumConst);
  if (enumConsts->enumConst->err) {
    enumConsts->addErr(-1);
    return;
  }

  State state;
  while (lexer->getCurToken() == TOK_COMMA) {
    saveState(state);
    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','

    spEnumConstant enumConst = spEnumConstant(new EnumConstant);
    parseEnumConstant(enumConst);
    if (enumConst->err) {
      restoreState(state);
      return;
    }

    enumConsts->pairs.push_back(std::make_pair(pos, enumConst));
  }
}

/// EnumDeclaration:
///   enum Identifier [implements TypeList] EnumBody
void Parser::parseEnumDeclaration(spEnumDeclaration &enumDecl) {
  // enum
  if (lexer->getCurToken() != TOK_KEY_ENUM) {
    enumDecl->addErr(-1);
    return;
  }

  enumDecl->tokEnum = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
    lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'enum'

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    enumDecl->addErr(diag->addErr(ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  enumDecl->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));

  st.addSym(ST_IDENTIFIER, lexer->getCurTokenIni(), lexer->getCursor(),
    src->getLine(), lexer->getCurTokenStr());

  lexer->getNextToken(); // consume Identifier

  // [implements TypeList]
  if (lexer->getCurToken() == TOK_KEY_IMPLEMENTS) {
    enumDecl->tokImpl = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'implements'

    enumDecl->typeList = spTypeList(new TypeList);
    parseTypeList(enumDecl->typeList);
    if (enumDecl->typeList->err) {
      enumDecl->addErr(-1);
      return;
    }
  }

  enumDecl->enumBody = spEnumBody(new EnumBody);
  parseEnumBody(enumDecl->enumBody);
  if (enumDecl->enumBody->err) {
    enumDecl->addErr(-1);
  }
}

/// Block: '{' BlockStatements '}'
void Parser::parseBlock(spBlock &block) {
  // '{'
  if (lexer->getCurToken() != TOK_LCURLY_BRACKET) {
    block->addErr(diag->addErr(ERR_EXP_LCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  block->posLCBrace = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '{'

  // BlockStatements
  // Check for a closing curly bracket. This means we don't have a
  // BlockStatement.
  if (lexer->getCurToken() == TOK_RCURLY_BRACKET) {
    block->posRCBrace = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '}'
    return;
  }

  parseBlockStatements(block->blockStmts);

  // '}'
  if (lexer->getCurToken() != TOK_RCURLY_BRACKET) {
    block->addErr(diag->addErr(ERR_EXP_RCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  block->posRCBrace = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '}'
}

/// BlockStatement:
///   LocalVariableDeclarationStatement
///   ClassOrInterfaceDeclaration
///   [Identifier :] Statement
void Parser::parseBlockStatement(spBlockStatement &blockStmt) {
  // LocalVariableDeclarationStatement
  State state;
  saveState(state);
  spLocalVariableDeclarationStatement localVar =
    spLocalVariableDeclarationStatement(new LocalVariableDeclarationStatement);
  parseLocalVariableDeclarationStatement(localVar);
  if (localVar->err) {
    restoreState(state);
  } else {
    blockStmt->opt = BlockStatement::OPT_LOCAL_VAR;
    blockStmt->localVar = localVar;
    return;
  }

  // ClassOrInterfaceDeclaration
  saveState(state);
  if (isClassOrInterfaceDeclarationCandidate(lexer->getCurToken())) {
    spClassOrInterfaceDeclaration decl = spClassOrInterfaceDeclaration(
      new ClassOrInterfaceDeclaration);
    parseClassOrInterfaceDeclaration(decl);
    if (decl->err == false) {
      blockStmt->opt = BlockStatement::OPT_CLASS_OR_INTERFACE_DECL;
      blockStmt->classOrIntDecl = decl;
      return;
    }

    restoreState(state);
  }

  // [Identifier :] Statement
  blockStmt->opt = BlockStatement::OPT_ID_STMT;
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    saveState(state);
    spIdentifier id = spIdentifier(new Identifier(
      state.cursor - state.tokenStr.length(), state.tokenStr));
    lexer->getNextToken(); // consume Identifier

    if (lexer->getCurToken() == TOK_OP_COLON) {
      blockStmt->id = id;
      blockStmt->posColon = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume ':'
    } else {
      restoreState(state);
    }
  }

  spStatement stmt = spStatement(new Statement);
  parseStatement(stmt);
  if (stmt->err) {
    blockStmt->addErr(-1);
    return;
  }

  // Accept stmt
  blockStmt->stmt = stmt;
}

/// BlockStatements: { BlockStatement }
void Parser::parseBlockStatements(std::vector<spBlockStatement> &blockStmts) {
  unsigned pos = 0;
  while (lexer->getCurToken() != TOK_RCURLY_BRACKET
    && lexer->getCurToken() != TOK_KEY_CASE     // in case we are inside a
    && lexer->getCurToken() != TOK_KEY_DEFAULT  // switch statement.
    && pos != lexer->getCursor()) {

    pos = lexer->getCursor();
    spBlockStatement blockStmt = spBlockStatement(new BlockStatement);
    parseBlockStatement(blockStmt);
    if (blockStmt->err) {
      return;
    }
    blockStmts.push_back(blockStmt);
  }
}

void Parser::parseBooleanLiteral(spBooleanLiteral &boolLit) {
  boolLit->pos = lexer->getCurTokenIni();
  if (lexer->getCurTokenStr().compare("true") == 0) {
    boolLit->val = true;
  } else {
    boolLit->val = false;
  }
  lexer->getNextToken(); // consume 'true' or 'false'
}

/// Bound:
///   ReferenceType { & ReferenceType }
void Parser::parseBound(spBound &bound) {
  bound->refType = spReferenceType(new ReferenceType);
  parseReferenceType(bound->refType);
  if (bound->refType->err) {
    bound->addErr(-1);
    return;
  }

  while (lexer->getCurToken() == TOK_OP_AMPERSAND) {
    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '&'

    spReferenceType refType = spReferenceType(new ReferenceType);
    parseReferenceType(refType);
    if (refType->err) {
      bound->addErr(-1);
      return;
    }

    bound->pairs.push_back(std::make_pair(pos, refType));
  }
}

/// CatchClause:
///   catch '(' {VariableModifier} CatchType Identifier ')' Block
void Parser::parseCatchClause(spCatchClause &catchClause) {
  // 'catch'
  if (lexer->getCurToken() != TOK_KEY_CATCH) {
    catchClause->addErr(diag->addErr(ERR_EXP_CATCH,
      lexer->getCurTokenIni(), lexer->getCursor()));
    return;
  }

  catchClause->tokCatch = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'catch'

  // '('
  if (lexer->getCurToken() != TOK_LPAREN) {
    catchClause->addErr(diag->addErr(ERR_EXP_LPAREN, lexer->getCursor() - 1));
    return;
  }

  catchClause->posLParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '('

  // {VariableModifier}
  if (isVariableModifier(lexer->getCurToken())) {
    catchClause->varMod = spVariableModifier(new VariableModifier);
    parseVariableModifier(catchClause->varMod);
    if (catchClause->varMod->err) {
      catchClause->addErr(-1);
    }
  }

  // CatchType
  catchClause->catchType = spCatchType(new CatchType);
  parseCatchType(catchClause->catchType);
  if (catchClause->catchType->err) {
    catchClause->addErr(-1);
    return;
  }

  // Identifier
  catchClause->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier

  // ')'
  if (lexer->getCurToken() != TOK_RPAREN) {
    catchClause->addErr(diag->addErr(ERR_EXP_RPAREN, lexer->getCursor() - 1));
    return;
  }

  catchClause->posRParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ')'

  // Block
  catchClause->block = spBlock(new Block);
  parseBlock(catchClause->block);
  if (catchClause->block->err) {
    catchClause->addErr(-1);
    return;
  }
}

/// Catches: CatchClause { CatchClause }
void Parser::parseCatches(spCatches &catches) {
  catches->catchClause = spCatchClause(new CatchClause);
  parseCatchClause(catches->catchClause);
  if (catches->catchClause->err) {
    catches->addErr(-1);
    return;
  }

  while (lexer->getCurToken() == TOK_KEY_CATCH) {
    spCatchClause catchClause = spCatchClause(new CatchClause);
    parseCatchClause(catchClause);
    if (catchClause->err) {
      return;
    }

    catches->catchClauses.push_back(catchClause);
  }
}

/// CatchType: QualifiedIdentifier { '|' QualifiedIdentifier }
void Parser::parseCatchType(spCatchType &catchType) {
  // QualifiedIdentifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    catchType->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  catchType->qualifiedId = spQualifiedIdentifier(new QualifiedIdentifier);
  parseQualifiedIdentifier(catchType->qualifiedId);

  // { '|' QualifiedIdentifier }
  State state;
  while (lexer->getCurToken() == TOK_OP_PIPE) {
    saveState(state);
    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '|'
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      restoreState(state);
      return; // let upper levels deal with the error
    }

    spQualifiedIdentifier qualifiedId
      = spQualifiedIdentifier(new QualifiedIdentifier);
    parseQualifiedIdentifier(qualifiedId);

    catchType->pairs.push_back(std::make_pair(pos, qualifiedId));
  }
}

void Parser::parseCharacterLiteral(spCharacterLiteral &charLit) {
  charLit->pos = lexer->getCurTokenIni();
  charLit->val = lexer->getCurTokenStr();
  lexer->getNextToken(); // consume character literal
}

/// Creator:
///   NonWildcardTypeArguments CreatedName ClassCreatorRest
///   CreatedName ( ClassCreatorRest | ArrayCreatorRest )
///   BasicType ArrayCreatorRest
/// See notes in AST.h
void Parser::parseCreator(spCreator &creator) {
  // Option 1
  if (lexer->getCurToken() == TOK_OP_LT) {
    creator->opt = Creator::OPT_NON_WILDCARD_TYPE_ARGUMENTS;
    creator->opt1 = spCreatorOpt1(new CreatorOpt1);
    parseCreatorOpt1(creator->opt1);
    return;
  }

  // Option 3
  if (isBasicType(lexer->getCurToken())) {
    creator->opt = Creator::OPT_BASIC_TYPE;
    creator->opt3 = spCreatorOpt3(new CreatorOpt3);
    parseCreatorOpt3(creator->opt3);
    if (creator->opt3->err) {
      creator->addErr(-1);
    }
    return;
  }

  // Option 2
  creator->opt = Creator::OPT_CREATED_NAME;
  creator->opt2 = spCreatorOpt2(new CreatorOpt2);
  parseCreatorOpt2(creator->opt2);
}

/// CreatorOpt1: NonWildcardTypeArguments CreatedName ClassCreatorRest
void Parser::parseCreatorOpt1(spCreatorOpt1 &opt1) {
  // NonWildcardTypeArguments
  opt1->nonWildcardTypeArguments = spNonWildcardTypeArguments(
    new NonWildcardTypeArguments());
  parseNonWildcardTypeArguments(opt1->nonWildcardTypeArguments);

  if (opt1->nonWildcardTypeArguments->err) {
    opt1->addErr(-1);
    return;
  }

  // CreatedName
  opt1->createdName = spCreatedName(new CreatedName());
  parseCreatedName(opt1->createdName);

  if (opt1->createdName->err) {
    opt1->addErr(-1);
    return;
  }

  // ClassCreatorRest
  // GCC(4.6.3). Assign it in two steps.
  spClassCreatorRest classCreatorRest = spClassCreatorRest(
    new ClassCreatorRest());
  opt1->classCreatorRest = classCreatorRest;
  parseClassCreatorRest(opt1->classCreatorRest);
  if (opt1->classCreatorRest->err) {
    opt1->addErr(-1);
  }
}

/// CreatorOpt2: CreatedName ( ClassCreatorRest | ArrayCreatorRest )
void Parser::parseCreatorOpt2(spCreatorOpt2 &opt2) {
  // CreatedName
  opt2->createdName = spCreatedName(new CreatedName);
  parseCreatedName(opt2->createdName);

  if (opt2->createdName->err) {
    opt2->addErr(-1);
    return;
  }

  // ( ClassCreatorRest | ArrayCreatorRest )
  // ClassCreatorRest
  if (lexer->getCurToken() == TOK_LPAREN) {
    opt2->classCreatorRest = spClassCreatorRest(new ClassCreatorRest);
    parseClassCreatorRest(opt2->classCreatorRest);
    return;
  }

  // ArrayCreatorRest
  if (lexer->getCurToken() == TOK_LBRACKET) {
    opt2->arrayCreatorRest = spArrayCreatorRest(new ArrayCreatorRest);
    parseArrayCreatorRest(opt2->arrayCreatorRest);
    return;
  }

  // Error
  opt2->addErr(diag->addErr(
    ERR_EXP_CLASS_OR_ARRAY_CREATOR_REST, lexer->getCursor() - 1));
}

/// CreatorOpt3: BasicType ArrayCreatorRest
void Parser::parseCreatorOpt3(spCreatorOpt3 &opt3) {
  spTokenExp token = spTokenExp(new TokenExp(lexer->getCursor()
    - tokenUtil.getTokenLength(lexer->getCurToken()), lexer->getCurToken()));
  opt3->basicType = spBasicType(new BasicType(token));
  lexer->getNextToken(); // consume BasicType

  if (lexer->getCurToken() != TOK_LBRACKET) {
    opt3->addErr(diag->addErr(ERR_EXP_LBRACKET, lexer->getCursor() - 1));
    return;
  }

  opt3->arrayCreatorRest = spArrayCreatorRest(new ArrayCreatorRest);
  parseArrayCreatorRest(opt3->arrayCreatorRest);
  if (opt3->arrayCreatorRest->err) {
    opt3->addErr(-1);
  }
}

/// Expression: Expression1 [ AssignmentOperator Expression ]
void Parser::parseExpression(spExpression &expr) {
  spExpression1 expr1 = spExpression1(new Expression1);
  parseExpression1(expr1);
  if (expr1->isEmpty()) {
    return;
  }

  expr->expr1 = expr1;

  // [ AssignmentOperator Expression1 ]
  if (isAssignmentOperator(lexer->getCurToken())) {
    State state;
    saveState(state);

    spAssignmentOperator assignOp
      = spAssignmentOperator(new AssignmentOperator);
    assignOp->tok = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume assignment token

    spExpression assignExpr = spExpression(new Expression);
    parseExpression(assignExpr);
    if (assignExpr->isEmpty()) {
      restoreState(state);
      return;
    }

    expr->assignOp = assignOp;
    expr->assignExpr = assignExpr;
  }
}

/// Expression1: Expression2 [Expression1Rest]
void Parser::parseExpression1(spExpression1 &expr1) {
  spExpression2 expr2 = spExpression2(new Expression2);
  parseExpression2(expr2);
  if (expr2->isEmpty()) {
    return;
  }

  expr1->expr2 = expr2;

  // Expression1Rest
  if (lexer->getCurToken() == TOK_OP_QUESTION_MARK) {
    State state;
    saveState(state);
    spExpression1Rest expr1Rest = spExpression1Rest(new Expression1Rest);
    parseExpression1Rest(expr1Rest);
    if (expr1Rest->err) {
      restoreState(state);
      return;
    }

    expr1->expr1Rest = expr1Rest;
  }
}

/// Expression1Rest:
///   ? Expression : Expression1
void Parser::parseExpression1Rest(spExpression1Rest &expr1Rest) {
  // '?'
  expr1Rest->posQuestionMark = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '?'

  // Expression
  expr1Rest->expr = spExpression(new Expression);
  parseExpression(expr1Rest->expr);
  if (expr1Rest->expr->isEmpty()) {
    expr1Rest->addErr(-1);
    return;
  }

  // ':'
  if (lexer->getCurToken() != TOK_OP_COLON) {
    expr1Rest->addErr(diag->addErr(ERR_EXP_OP_COLON, lexer->getCursor() - 1));
    return;
  }

  expr1Rest->posColon = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ':'

  expr1Rest->expr1 = spExpression1(new Expression1);
  parseExpression1(expr1Rest->expr1);
  if (expr1Rest->expr1->isEmpty()) {
    expr1Rest->addErr(-1);
  }
}

/// Expression2: Expression3 [ Expression2Rest ]
void Parser::parseExpression2(spExpression2 &expr2) {
  spExpression3 expr3 = spExpression3(new Expression3);
  parseExpression3(expr3);
  if (expr3->isEmpty()) {
    return;
  }

  expr2->expr3 = expr3;

  // [ Expression2Rest ]
  spExpression2Rest expr2Rest = spExpression2Rest(new Expression2Rest);
  parseExpression2Rest(expr2Rest);
  if (expr2Rest->pairs.size()) {
    expr2->expr2Rest = expr2Rest;
  }
}

/// Expression2Rest:
///   { InfixOp Expression3 | instanceof Type }
/// See notes in AST.h
void Parser::parseExpression2Rest(spExpression2Rest &expr2Rest) {
  State state;
  while (lexer->getCurToken() == TOK_KEY_INSTANCEOF
    || isInfixOp(lexer->getCurToken())) {

    saveState(state);
    spExpression2RestHelper helper
      = spExpression2RestHelper(new Expression2RestHelper);

    // (2) instanceof Type
    if (lexer->getCurToken() == TOK_KEY_INSTANCEOF) {
      helper->opt = Expression2RestHelper::OPT_INSTANCEOF_TYPE;
      helper->tokInstanceOf = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
          lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'instanceof'

      helper->type = spType(new Type);
      parseType(helper->type);

      if (helper->type->err) {
        restoreState(state);
        return;
      }

      expr2Rest->pairs.push_back(helper);
      continue;
    }

    // (1) { InfixOp Expression3 }
    helper->opt = Expression2RestHelper::OPT_INFIXOP_EXPR3;
    helper->tokInfixOp = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume InfixOp

    helper->expr3 = spExpression3(new Expression3);
    parseExpression3(helper->expr3);

    if (helper->expr3->isEmpty()) {
      restoreState(state);
      return;
    }

    expr2Rest->pairs.push_back(helper);
  }
}

/// Expression3:
///   (1) PrefixOp Expression3
///   (2) '(' Type ')' Expression3
///   (3) '(' Expression ')' Expression3
///   (4) Primary { Selector } { PostfixOp }
///
/// The first option is recursive only if the prefixes are '!' and '~'.
/// For example: !!a; ~~b; are legal expressions but ++++a; is invalid
/// while ~++c; is valid;
void Parser::parseExpression3(spExpression3 &expr3) {
  // (1) PrefixOp Expression3
  if (isPrefixOp(lexer->getCurToken())) {
    expr3->opt = Expression3::OPT_PREFIXOP_EXPRESSION3;
    expr3->prefixOp = spPrefixOp(new PrefixOp(
      lexer->getCurTokenIni(), lexer->getCurToken()));
    lexer->getNextToken(); // consume PrefixOp

    expr3->expr3 = spExpression3(new Expression3);
    parseExpression3(expr3->expr3);
    return;
  }

  // (2,3) '(' Expression | Type ')' Expression3
  // We use a lock in case Expression lead us back to Expression3.
  // This won't be pretty.
  if (lexer->getCurToken() == TOK_LPAREN) {
    State state;
    saveState(state);

    // Let's try Type first
    // Option 2
    spExpression3Opt2 opt2 = spExpression3Opt2(new Expression3Opt2);
    parseExpression3Opt2(opt2);
    if (opt2->err == false) {
      expr3->opt = Expression3::OPT_TYPE_EXPRESSION3;
      expr3->opt2 = opt2;
      return;
    }

    // Recover and try option 3
    restoreState(state);

    // Option 3
    expr3->opt = Expression3::OPT_EXPRESSION_EXPRESSION3;
    expr3->opt3 = spExpression3Opt3(new Expression3Opt3);
    parseExpression3Opt3(expr3->opt3);
    if (expr3->opt3->err == false) {
      return;
    }

    // We restore the state and try option 4. It might be a primary expression.
    restoreState(state);
  }

  // (4) Primary { Selector } { PostfixOp }
  if (isPrimary(lexer->getCurToken())) {
    spPrimary primary = spPrimary(new Primary);
    parsePrimary(primary);
    if (primary->isEmpty() == false && primary->err == false) {
      expr3->opt = Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP;
      expr3->primary = primary;

      // { Selector }
      while (lexer->getCurToken() == TOK_PERIOD
        || lexer->getCurToken() == TOK_LBRACKET) {
        spSelector selector = spSelector(new Selector);
        parseSelector(selector);
        if (selector->err) {
          expr3->addErr(-1);
          return;
        }

        expr3->selectors.push_back(selector);
      }

      // { PostfixOp }
      while (lexer->getCurToken() == TOK_OP_PLUS_PLUS
        || lexer->getCurToken() == TOK_OP_MINUS_MINUS) {
        spPostfixOp postfixOp = spPostfixOp(new PostfixOp);
        parsePostfixOp(postfixOp);
        expr3->postfixOps.push_back(postfixOp);
      }

      return;
    }
  }

  expr3->addErr(-1);
}

void Parser::parseExpression3Opt2(spExpression3Opt2 &opt2) {
  // '('
  opt2->posLParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consue '('

  // Type
  opt2->type = spType(new Type);
  parseType(opt2->type);
  if (opt2->type->err) {
    opt2->addErr(-1);
    return;
  }

  // ')'
  if (lexer->getCurToken() != TOK_RPAREN) {
    opt2->addErr(-1);
    return;
  }

  opt2->posRParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ')'

  // Expression3
  opt2->expr3 = spExpression3(new Expression3);
  parseExpression3(opt2->expr3);
  if (opt2->expr3->err) {
    opt2->addErr(-1);
    return;
  }
}

void Parser::parseExpression3Opt3(spExpression3Opt3 &opt3) {
  // '('
  opt3->posLParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '('

  // Expression
  opt3->expr = spExpression(new Expression);
  parseExpression(opt3->expr);
  if (opt3->expr->isEmpty()) {
    opt3->addErr(-1);
  }

  // ')'
  if (lexer->getCurToken() != TOK_RPAREN) {
    opt3->addErr(-1);
    return;
  }

  opt3->posRParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ')'

  // Expression3
  opt3->expr3 = spExpression3(new Expression3);
  parseExpression3(opt3->expr3);
  if (opt3->expr3->err) {
    opt3->addErr(-1);
    return;
  }
}

/// Finally: finally Block
void Parser::parseFinally(spFinally &finally) {
  // 'finally'
  if (lexer->getCurToken() != TOK_KEY_FINALLY) {
    finally->addErr(-1);
    return;
  }

  finally->tokFinally = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'finally'

  // Block
  finally->block = spBlock(new Block);
  parseBlock(finally->block);
  if (finally->block->err) {
    finally->addErr(-1);
  }
}

/// FieldDeclaratorsRest: VariableDeclaratorRest { , VariableDeclarator }
void Parser::parseFieldDeclaratorsRest(spFieldDeclaratorsRest &fieldDeclsRest) {
  // VariableDeclaratorRest
  fieldDeclsRest->varDeclRest
    = spVariableDeclaratorRest(new VariableDeclaratorRest());
  parseVariableDeclaratorRest(fieldDeclsRest->varDeclRest);
  if (fieldDeclsRest->varDeclRest->err) {
    fieldDeclsRest->addErr(-1);
    return;
  }

  // { , VariableDeclarator }
  State state;
  while (lexer->getCurToken() == TOK_COMMA) {
    saveState(state);
    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','
    spVariableDeclarator varDecl = spVariableDeclarator(new VariableDeclarator);
    parseVariableDeclarator(varDecl);
    if (varDecl->err) {
      restoreState(state);
      return; // let upper levels deal with error
    }

    fieldDeclsRest->pairs.push_back(std::make_pair(pos, varDecl));
  }
}

/// ForControl
///   (1) ForVarControl
///   (2) [ForInit] ; [Expression] ; [ForUpdate]
void Parser::parseForControl(spForControl &forCtrl) {
  State state;
  saveState(state);

  // (1) ForVarControl
  spForVarControl varCtrl = spForVarControl(new ForVarControl);
  parseForVarControl(varCtrl);
  if (varCtrl->err == false) {
    forCtrl->opt = ForControl::OPT_FOR_VAR_CTRL;
    forCtrl->varCtrl = varCtrl;
    return;
  }

  restoreState(state);
  forCtrl->opt = ForControl::OPT_FOR_INIT;

  // (2) [ForInit] ; [Expression] ; [ForUpdate]
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    forCtrl->forInit = spForInit(new ForInit);
    parseForInit(forCtrl->forInit);
  }

  // ';'
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    forCtrl->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
    return;
  }

  forCtrl->posSemiColon1 = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ';'

  // [Expression]
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    forCtrl->expr = spExpression(new Expression);
    parseExpression(forCtrl->expr);
    if (forCtrl->expr->isEmpty()) {
      forCtrl->addErr(-1);
    }
  }

  // ';'
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    forCtrl->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
    return;
  }

  forCtrl->posSemiColon2 = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ';'

  // [ForUpdate]
  if (lexer->getCurToken() != TOK_RPAREN) {
    forCtrl->forUpdate = spForUpdate(new ForUpdate);
    parseForUpdate(forCtrl->forUpdate);
    if (forCtrl->forUpdate->err) {
      forCtrl->addErr(-1);
    }
  }
}

/// ForInit:
/// ForUpdate:
///   StatementExpression { , StatementExpression }
void Parser::parseForInit(spForInit &forInit) {
  forInit->stmtExpr = spStatementExpression(new StatementExpression);
  parseStatementExpression(forInit->stmtExpr);
  if (forInit->stmtExpr->err) {
    forInit->addErr(-1);
    return;
  }

  // { , StatementExpression }
  while (lexer->getCurToken() == TOK_COMMA) {
    unsigned pos = lexer->getCursor();
    lexer->getNextToken(); // consume ';'

    spStatementExpression stmtExpr
      = spStatementExpression(new StatementExpression);
    parseStatementExpression(stmtExpr);
    if (stmtExpr->err) { return; }

    std::pair<unsigned, spStatementExpression> pair;
    pair.first = pos;
    pair.second = stmtExpr;

    forInit->pairs.push_back(pair);
  }
}

/// ForInit:
/// ForUpdate:
///   StatementExpression { , StatementExpression }
void Parser::parseForUpdate(spForUpdate &forUpdate) {
  parseForInit(forUpdate);
}

/// ForVarControl
///   {VariableModifier} Type VariableDeclaratorId ForVarControlRest
void Parser::parseForVarControl(spForVarControl &forVarCtrl) {
  // {VariableModifier}
  forVarCtrl->varMod = spVariableModifier(new VariableModifier);
  parseVariableModifier(forVarCtrl->varMod);

  // Type
  forVarCtrl->type = spType(new Type);
  parseType(forVarCtrl->type);
  if (forVarCtrl->type->err) {
    forVarCtrl->addErr(-1);
    return;
  }

  // VariableDeclaratorId
  forVarCtrl->varDeclId = spVariableDeclaratorId(new VariableDeclaratorId);
  parseVariableDeclaratorId(forVarCtrl->varDeclId);
  if (forVarCtrl->varDeclId->err) {
    forVarCtrl->addErr(-1);
  }

  // ForVarControlRest
  forVarCtrl->forVarCtrlRest = spForVarControlRest(new ForVarControlRest);
  parseForVarControlRest(forVarCtrl->forVarCtrlRest);
  if (forVarCtrl->forVarCtrlRest->err) {
    forVarCtrl->addErr(-1);
  }
}

/// ForVarControlRest
///   (1) ForVariableDeclaratorsRest ; [Expression] ; [ForUpdate]
///   (2) : Expression
void Parser::parseForVarControlRest(spForVarControlRest &forVarCtrlRest) {
  // (2) : Expression
  if (lexer->getCurToken() == TOK_OP_COLON) {
    forVarCtrlRest->opt = ForVarControlRest::OPT_COLON_EXPR;
    forVarCtrlRest->posColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ':'
    forVarCtrlRest->expr = spExpression(new Expression);
    parseExpression(forVarCtrlRest->expr);
    if (forVarCtrlRest->expr->isEmpty()) {
      forVarCtrlRest->addErr(-1);
    }
    return;
  }

  // (1) ForVariableDeclaratorsRest ; [Expression] ; [ForUpdate]
  forVarCtrlRest->opt = ForVarControlRest::OPT_FOR_VAR_DECLS_REST;

  // ForVariableDeclaratorsRest
  forVarCtrlRest->forVarDeclsRest
    = spForVariableDeclaratorsRest(new ForVariableDeclaratorsRest);
  parseForVariableDeclaratorsRest(forVarCtrlRest->forVarDeclsRest);
  if (forVarCtrlRest->forVarDeclsRest->err) {
    forVarCtrlRest->addErr(-1);
  }

  // ;
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    forVarCtrlRest->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
    return;
  }

  forVarCtrlRest->posSemiColon1 = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ;

  // [Expression]
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    forVarCtrlRest->expr = spExpression(new Expression);
    parseExpression(forVarCtrlRest->expr);
    if (forVarCtrlRest->expr->isEmpty()) {
      forVarCtrlRest->addErr(-1);
    }
  }

  // ;
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    forVarCtrlRest->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
    return;
  }

  forVarCtrlRest->posSemiColon2 = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ;

  // [ForUpdate]
  if (lexer->getCurToken() != TOK_RPAREN) {
    forVarCtrlRest->forUpdate = spForUpdate(new ForUpdate);
    parseForUpdate(forVarCtrlRest->forUpdate);
    if (forVarCtrlRest->forUpdate->err) {
      forVarCtrlRest->addErr(-1);
    }
  }
}

/// ForVariableDeclaratorsRest
///   [ = VariableInitializer ] { , VariableDeclarator }
void Parser::parseForVariableDeclaratorsRest(
  spForVariableDeclaratorsRest &forVarDeclsRest) {

  // [ = VariableInitializer ]
  if (lexer->getCurToken() == TOK_OP_EQUALS) {
    forVarDeclsRest->posEquals = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '='

    forVarDeclsRest->varInit = spVariableInitializer(new VariableInitializer);
    parseVariableInitializer(forVarDeclsRest->varInit);
    if (forVarDeclsRest->varInit->err) {
      forVarDeclsRest->addErr(-1);
    }
  }

  while (lexer->getCurToken() == TOK_COMMA) {
    State state;
    saveState(state);

    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','
    spVariableDeclarator varDecl = spVariableDeclarator(new VariableDeclarator);
    parseVariableDeclarator(varDecl);
    if (varDecl->err) {
      restoreState(state);
      return;
    }

    std::pair<unsigned, spVariableDeclarator> pair;
    pair.first = pos;
    pair.second = varDecl;
    forVarDeclsRest->pairs.push_back(pair);
  }
}

/// GenericMethodOrConstructorDecl:
/// TypeParameters GenericMethodOrConstructorRest
void Parser::parseGenericMethodOrConstructorDecl(
  spGenericMethodOrConstructorDecl &genMethodOrConstDecl) {

  genMethodOrConstDecl->typeParams = spTypeParameters(new TypeParameters);
  parseTypeParameters(genMethodOrConstDecl->typeParams);
  if (genMethodOrConstDecl->typeParams->err) {
    genMethodOrConstDecl->addErr(-1);
    return;
  }

  genMethodOrConstDecl->rest
    = spGenericMethodOrConstructorRest(new GenericMethodOrConstructorRest);
  parseGenericMethodOrConstructorRest(genMethodOrConstDecl->rest);
  if (genMethodOrConstDecl->rest->err) {
    genMethodOrConstDecl->addErr(-1);
  }
}

/// GenericMethodOrConstructorRest:
/// (1) Type Identifier MethodDeclaratorRest
/// (2) void Identifier MethodDeclaratorRest
/// (3) Identifier ConstructorDeclaratorRest
void Parser::parseGenericMethodOrConstructorRest(
  spGenericMethodOrConstructorRest &rest) {


  // (1) Type Identifier MethodDeclaratorRest
  if (lexer->getCurToken() == TOK_KEY_VOID) {
    // 'void'
    rest->opt = GenericMethodOrConstructorRest::OPT_VOID_IDENTIFIER;
    rest->tokVoid = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'void'

    // Identifier
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      rest->addErr(diag->addErr(ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
      return;
    }

    rest->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier

    // MethodDeclaratorRest
    rest->methodDeclRest = spMethodDeclaratorRest(new MethodDeclaratorRest);
    parseMethodDeclaratorRest(rest->methodDeclRest);
    if (rest->methodDeclRest->err) {
      rest->addErr(-1);
    }

    return;
  }

  // We have to decide between (1) and (2). If we have an identifier we consume
  // it and if the next token is an opening parenthesis we know we have
  // (3). Otherwise we assume we have (1).
  State state;
  saveState(state);
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    spIdentifier id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier
    if (lexer->getCurToken() == TOK_LPAREN) {
      // (3) Identifier ConstructorDeclaratorRest
      rest->opt = GenericMethodOrConstructorRest::OPT_IDENTIFIER_CONSTRUCTOR;
      rest->id = id;
      rest->constDeclRest
        = spConstructorDeclaratorRest(new ConstructorDeclaratorRest);
      parseConstructorDeclaratorRest(rest->constDeclRest);
      if (rest->constDeclRest->err) {
        rest->addErr(-1);
      }
      return;
    }

    restoreState(state);
  }

  // (1) Type Identifier MethodDeclaratorRest
  rest->opt = GenericMethodOrConstructorRest::OPT_TYPE_IDENTIFIER;
  // Type
  rest->type = spType(new Type);
  parseType(rest->type);
  if (rest->type->err) {
    rest->addErr(-1);
    return;
  }

  // Identifier
  rest->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier

  // MethodDeclaratorRest
  rest->methodDeclRest = spMethodDeclaratorRest(new MethodDeclaratorRest);
  parseMethodDeclaratorRest(rest->methodDeclRest);
  if (rest->methodDeclRest->err) {
    rest->addErr(-1);
  }
}

/// InterfaceBody:
///   '{' { InterfaceBodyDeclaration } '}'
void Parser::parseInterfaceBody(spInterfaceBody &body) {
  // '{'
  if (lexer->getCurToken() != TOK_LCURLY_BRACKET) {
    body->addErr(diag->addErr(
      ERR_EXP_LCURLY_BRACKET, lexer->getCurToken() - 1));
    return;
  }

  body->posLCBrace = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '{'

  // { InterfaceBodyDeclaration }
  unsigned pos = 0;
  while (lexer->getCurToken() != TOK_RCURLY_BRACKET
    && pos != lexer->getCursor()) {
    pos = lexer->getCursor();

    spInterfaceBodyDeclaration bodyDecl = spInterfaceBodyDeclaration(
      new InterfaceBodyDeclaration);
    parseInterfaceBodyDeclaration(bodyDecl);
    body->bodyDecls.push_back(bodyDecl);

    // Exit if we find an error
    if (bodyDecl->err) {
      return;
    }
  }

  // '}'
  if (lexer->getCurToken() != TOK_RCURLY_BRACKET) {
    body->addErr(diag->addErr(
      ERR_EXP_RCURLY_BRACKET, lexer->getCurToken() - 1));
    return;
  }

  body->posRCBrace = lexer->getCursor() - 1;
  st.updateScopeEnd(lexer->getCursor());
  lexer->getNextToken(); // consume '}'
}

/// InterfaceBodyDeclaration:
///   (1) ;
///   (2) {Modifier} InterfaceMemberDecl
void Parser::parseInterfaceBodyDeclaration(
  spInterfaceBodyDeclaration &bodyDecl) {

  // (1) ';'
  if (lexer->getCurToken() == TOK_SEMICOLON) {
    bodyDecl->opt = InterfaceBodyDeclaration::OPT_SEMICOLON;
    bodyDecl->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'
    return;
  }

  // (2) {Modifier} InterfaceMemberDecl
  st.addSym(ST_MEMBER_DECL, lexer->getCurTokenIni(), 0, src->getLine(), "");
  bodyDecl->opt = InterfaceBodyDeclaration::OPT_MEMBER_DECL;
  bodyDecl->modifier = spModifier(new Modifier);
  parseModifier(bodyDecl->modifier);
  bodyDecl->memberDecl = spInterfaceMemberDecl(new InterfaceMemberDecl);
  parseInterfaceMemberDecl(bodyDecl->memberDecl);
  if (bodyDecl->memberDecl->err) {
    bodyDecl->addErr(-1);
  }
  st.scopePop();
}

/// InterfaceDeclaration:
///   NormalInterfaceDeclaration
///   AnnotationTypeDeclaration
void Parser::parseInterfaceDeclaration(spInterfaceDeclaration &interfaceDecl) {
  // AnnotationTypeDeclaration
  if (lexer->getCurToken() == TOK_ANNOTATION_TYPE_DECLARATION) {
    interfaceDecl->opt = InterfaceDeclaration::OPT_ANNOTATION;
    interfaceDecl->annotationDecl = spAnnotationTypeDeclaration(
      new AnnotationTypeDeclaration);
    parseAnnotationTypeDeclaration(interfaceDecl->annotationDecl);
    if (interfaceDecl->annotationDecl->err) {
      interfaceDecl->addErr(-1);
    }
    return;
  }

  // NormalInterfaceDeclaration
  if (lexer->getCurToken() == TOK_KEY_INTERFACE) {
    interfaceDecl->opt = InterfaceDeclaration::OPT_NORMAL;
    interfaceDecl->normalDecl = spNormalInterfaceDeclaration(
      new NormalInterfaceDeclaration);
    parseNormalInterfaceDeclaration(interfaceDecl->normalDecl);
    if (interfaceDecl->normalDecl->err) {
      interfaceDecl->addErr(-1);
    }

    return;
  }

  // Error
  interfaceDecl->addErr(-1);
}

/// InterfaceGenericMethodDecl:
///   TypeParameters (Type | void) Identifier InterfaceMethodDeclaratorRest
void Parser::parseInterfaceGenericMethodDecl(
  spInterfaceGenericMethodDecl &genMethDecl) {

  // TypeParameters
  genMethDecl->typeParams = spTypeParameters(new TypeParameters);
  parseTypeParameters(genMethDecl->typeParams);
  if (genMethDecl->typeParams->err) {
    genMethDecl->addErr(-1);
    return;
  }

  // Type | void
  if (lexer->getCurToken() == TOK_KEY_VOID) {
    genMethDecl->tokVoid = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'void'
  } else {
    genMethDecl->type = spType(new Type);
    parseType(genMethDecl->type);
    if (genMethDecl->type->err) {
      genMethDecl->addErr(-1);
      return;
    }
  }

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    genMethDecl->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  genMethDecl->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier

  // InterfaceMethodDeclaratorRest
  genMethDecl->methDeclRest = spInterfaceMethodDeclaratorRest(
    new InterfaceMethodDeclaratorRest);
  parseInterfaceMethodDeclaratorRest(genMethDecl->methDeclRest);
  if (genMethDecl->methDeclRest->err) {
    genMethDecl->addErr(-1);
  }
}

/// InterfaceMemberDecl:
///   (1) InterfaceMethodOrFieldDecl
///   (2) void Identifier VoidInterfaceMethodDeclaratorRest
///   (3) InterfaceGenericMethodDecl
///   (4) ClassDeclaration
///   (5) InterfaceDeclaration
void Parser::parseInterfaceMemberDecl(spInterfaceMemberDecl &memberDecl) {
  // (1) InterfaceMethodOrFieldDecl
  if (isBasicType(lexer->getCurToken())
    || lexer->getCurToken() == TOK_IDENTIFIER) {

    memberDecl->opt = InterfaceMemberDecl::OPT_INTERFACE_METHOD_OR_FIELD_DECL;
    memberDecl->methodOrFieldDecl = spInterfaceMethodOrFieldDecl(
      new InterfaceMethodOrFieldDecl);
    parseInterfaceMethodOrFieldDecl(memberDecl->methodOrFieldDecl);
    if (memberDecl->methodOrFieldDecl->err) {
      memberDecl->addErr(-1);
    }

    return;
  }

  // (2) void Identifier VoidInterfaceMethodDeclaratorRest
  if (lexer->getCurToken() == TOK_KEY_VOID) {
    memberDecl->opt = InterfaceMemberDecl::OPT_VOID_IDENTIFIER;

    // void
    memberDecl->tokVoid = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'void'

    // Identifier
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      memberDecl->addErr(diag->addErr(
        ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
      return;
    }

    memberDecl->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    st.addSym(ST_IDENTIFIER, lexer->getCurTokenIni(), lexer->getCursor(),
      src->getLine(), lexer->getCurTokenStr());
    lexer->getNextToken(); // consume Identifier

    // VoidInterfaceMethodDeclaratorRest
    memberDecl->voidMethDeclRest = spVoidInterfaceMethodDeclaratorRest(
      new VoidInterfaceMethodDeclaratorRest);
    parseVoidInterfaceMethodDeclaratorRest(memberDecl->voidMethDeclRest);
    if (memberDecl->voidMethDeclRest->err) {
      memberDecl->addErr(-1);
    }

    return;
  }

  // (3) InterfaceGenericMethodDecl
  if (lexer->getCurToken() == TOK_OP_LT) {
    memberDecl->opt = InterfaceMemberDecl::OPT_INTERFACE_GENERIC;
    memberDecl->genMethodDecl = spInterfaceGenericMethodDecl(
      new InterfaceGenericMethodDecl);
    parseInterfaceGenericMethodDecl(memberDecl->genMethodDecl);
    if (memberDecl->genMethodDecl->err) {
      memberDecl->addErr(-1);
    }
    return;
  }

  // (4) ClassDeclaration
  if (lexer->getCurToken() == TOK_KEY_CLASS
    || lexer->getCurToken() == TOK_KEY_ENUM) {

    memberDecl->opt = InterfaceMemberDecl::OPT_CLASS_DECLARATION;
    if (lexer->getCurToken() == TOK_KEY_CLASS) {
      st.updateScopeType(ST_CLASS);
    } else {
      st.updateScopeType(ST_ENUM);
    }

    memberDecl->classDecl = spClassDeclaration(new ClassDeclaration);
    parseClassDeclaration(memberDecl->classDecl);
    if (memberDecl->classDecl->err) {
      memberDecl->addErr(-1);
    }
    return;
  }

  // (5) InterfaceDeclaration
  if (lexer->getCurToken() == TOK_KEY_INTERFACE) {
    memberDecl->opt = InterfaceMemberDecl::OPT_INTERFACE_DECLARATION;
    memberDecl->interfaceDecl = spInterfaceDeclaration(
      new InterfaceDeclaration);
    parseInterfaceDeclaration(memberDecl->interfaceDecl);
    if (memberDecl->interfaceDecl->err) {
      memberDecl->addErr(-1);
    }
    return;
  }

  memberDecl->addErr(-1);
}

/// InterfaceMethodDeclaratorRest:
///   FormalParameters {'[]'} [throws QualifiedIdentifierList] ;
void Parser::parseInterfaceMethodDeclaratorRest(
  spInterfaceMethodDeclaratorRest &methDeclRest) {
  // FormalParameters
  methDeclRest->formParams = spFormalParameters(new FormalParameters);
  parseFormalParameters(methDeclRest->formParams);
  if (methDeclRest->formParams->err) {
    methDeclRest->addErr(-1);
    return;
  }

  // {'[]'}
  parseArrayDepth(methDeclRest->arrayDepth);

  // [throws QualifiedIdentifierList]
  if (lexer->getCurToken() == TOK_KEY_THROWS) {
    methDeclRest->tokThrows = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'throws'

    methDeclRest->qualifiedIdList = spQualifiedIdentifierList(
      new QualifiedIdentifierList);
    parseQualifiedIdentifierList(methDeclRest->qualifiedIdList);
    if (methDeclRest->qualifiedIdList->err) {
      methDeclRest->addErr(-1);
      return;
    }
  }

  // ;
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    methDeclRest->addErr(diag->addErr(
      ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
    return;
  }

  methDeclRest->posSemiColon = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ';'
}

/// InterfaceMethodOrFieldDecl:
///   Type Identifier InterfaceMethodOrFieldRest
void Parser::parseInterfaceMethodOrFieldDecl(
  spInterfaceMethodOrFieldDecl &methodOrFieldDecl) {

  // Type
  methodOrFieldDecl->type = spType(new Type);
  parseType(methodOrFieldDecl->type);
  if (methodOrFieldDecl->type->err) {
    methodOrFieldDecl->addErr(-1);
    return;
  }

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    methodOrFieldDecl->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() -1));
    return;
  }

  methodOrFieldDecl->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier

  // InterfaceMethodOrFieldRest
  methodOrFieldDecl->rest = spInterfaceMethodOrFieldRest(
    new InterfaceMethodOrFieldRest);
  parseInterfaceMethodOrFieldRest(methodOrFieldDecl->rest);
  if (methodOrFieldDecl->rest->err) {
    methodOrFieldDecl->addErr(-1);
  }
}

/// InterfaceMethodOrFieldRest
///   ConstantDeclaratorsRest ;
///   InterfaceMethodDeclaratorRest
void Parser::parseInterfaceMethodOrFieldRest(
  spInterfaceMethodOrFieldRest &rest) {
  // ConstantDeclaratorsRest ;
  if (lexer->getCurToken() == TOK_LBRACKET
    || lexer->getCurToken() == TOK_OP_EQUALS) {

    rest->opt = InterfaceMethodOrFieldRest::OPT_CONSTANT_REST;

    // ConstantDeclaratorsRest
    rest->constDeclsRest = spConstantDeclaratorsRest(
      new ConstantDeclaratorsRest);
    parseConstantDeclaratorsRest(rest->constDeclsRest);
    if (rest->constDeclsRest->err) {
      rest->addErr(-1);
      return;
    }

    // ';'
    if (lexer->getCurToken() != TOK_SEMICOLON) {
      rest->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
      return;
    }

    rest->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'
    return;
  }

  // InterfaceMethodDeclaratorRest
  if (lexer->getCurToken() == TOK_LPAREN) {
    rest->opt = InterfaceMethodOrFieldRest::OPT_METHOD_REST;
    rest->methDeclRest = spInterfaceMethodDeclaratorRest(
      new InterfaceMethodDeclaratorRest);
    parseInterfaceMethodDeclaratorRest(rest->methDeclRest);
    if (rest->methDeclRest->err) {
      rest->addErr(-1);
    }

    return;
  }

  // Error
  rest->addErr(-1);
}

/// IdentifierSuffix:
///   '[' ( ']' {'[]'} . class | Expression ']' )
///   Arguments
///   . ( class | ExplicitGenericInvocation | this | super Arguments |
///       new [NonWildcardTypeArguments] InnerCreator )
void Parser::parseIdentifierSuffix(spIdentifierSuffix &idSuffix) {
  // opt1-2
  if (lexer->getCurToken() == TOK_LBRACKET) {
    idSuffix->arrayPair.first = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '['

    // opt1
    if (lexer->getCurToken() == TOK_RBRACKET) {
      // '[]' {'[]'} . class
      idSuffix->opt = IdentifierSuffix::OPT_ARRAY_ARRAY_DEPTH_CLASS;
      idSuffix->arrayPair.second = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume ']'

      // {'[]'}
      parseArrayDepth(idSuffix->arrayDepth);

      // . class
      if (lexer->getCurToken() != TOK_PERIOD) {
        idSuffix->addErr(diag->addErr(ERR_EXP_PERIOD, lexer->getCursor() - 1));
        return;
      }

      idSuffix->posPeriod = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume '.'

      // class
      if (lexer->getCurToken() != TOK_KEY_CLASS) {
        idSuffix->addErr(diag->addErr(ERR_EXP_CLASS, lexer->getCursor() - 1));
        return;
      }

      // class
      idSuffix->tokSuper = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
          lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'class'
      return;
    }

    // opt2
    // '[' Expression ']'
    idSuffix->opt = IdentifierSuffix::OPT_ARRAY_EXPRESSION;
    idSuffix->expr = spExpression(new Expression);
    parseExpression(idSuffix->expr);
    if (idSuffix->expr->isEmpty()) {
      idSuffix->addErr(-1);
      return;
    }

    // ']'
    if (lexer->getCurToken() != TOK_RBRACKET) {
      idSuffix->addErr(diag->addErr(ERR_EXP_RBRACKET, lexer->getCursor() - 1));
      return;
    }

    idSuffix->arrayPair.second = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ']'
    return;
  }

  // opt3: Arguments
  if (lexer->getCurToken() == TOK_LPAREN) {
    idSuffix->opt = IdentifierSuffix::OPT_ARGUMENTS;
    idSuffix->args = spArguments(new Arguments);
    parseArguments(idSuffix->args);
    if (idSuffix->args->err) { idSuffix->addErr(-1); }
    return;
  }

  // opt4-8
  if (lexer->getCurToken() == TOK_PERIOD) {
    idSuffix->posPeriod = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '.'

    // opt4: . class
    if (lexer->getCurToken() == TOK_KEY_CLASS) {
      idSuffix->opt = IdentifierSuffix::OPT_PERIOD_CLASS;
      idSuffix->tokClass = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
          lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'class'
      return;
    }

    // opt5: . ExplicitGenericInvocation
    if (lexer->getCurToken() == TOK_OP_LT) {
      idSuffix->opt = IdentifierSuffix::OPT_PERIOD_EXPLICIT_GENERIC_INVOCATION;
      idSuffix->explGenInvocation = spExplicitGenericInvocation(
        new ExplicitGenericInvocation());
      parseExplicitGenericInvocation(idSuffix->explGenInvocation);
      if (idSuffix->explGenInvocation->err) { idSuffix->addErr(-1); }
      return;
    }

    // opt6: . this
    if (lexer->getCurToken() == TOK_KEY_THIS) {
      idSuffix->opt = IdentifierSuffix::OPT_PERIOD_THIS;
      idSuffix->tokThis = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
          lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'this'
      return;
    }

    // opt7: . super Arguments
    if (lexer->getCurToken() == TOK_KEY_SUPER) {
      idSuffix->opt = IdentifierSuffix::OPT_PERIOD_SUPER_ARGUMENTS;
      idSuffix->tokSuper = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
          lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'super'

      // Error: expected '('
      if (lexer->getCurToken() != TOK_LPAREN) {
        idSuffix->addErr(diag->addErr(ERR_EXP_LPAREN, lexer->getCursor() - 1));
        return;
      }

      idSuffix->args = spArguments(new Arguments);
      parseArguments(idSuffix->args);
      if (idSuffix->args->err) { idSuffix->addErr(-1); }
      return;
    }

    // opt8: . new [NonWildcardTypeArguments] InnerCreator
    if (lexer->getCurToken() == TOK_KEY_NEW) {
      idSuffix->opt = IdentifierSuffix::OPT_NEW;
      idSuffix->tokNew = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
          lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'new'

      // NonWildcardTypeArguments
      if (lexer->getCurToken() == TOK_OP_LT) {
        idSuffix->nonWildcardTypeArguments = spNonWildcardTypeArguments(
          new NonWildcardTypeArguments());
        parseNonWildcardTypeArguments(idSuffix->nonWildcardTypeArguments);

        // Error: invalid NonWildcardTypeArguments
        if (idSuffix->nonWildcardTypeArguments->err) {
          idSuffix->addErr(-1);
          return;
        }
      }

      // InnerCreator
      idSuffix->innerCreator = spInnerCreator(new InnerCreator);
      parseInnerCreator(idSuffix->innerCreator);

      // Error: invalid InnerCreator
      if (idSuffix->innerCreator->err) {
        idSuffix->addErr(-1);
      }

      return;
    }
  }

  // error
  idSuffix->addErr(diag->addErr(
    ERR_NVAL_IDENTIFIER_SUFFIX, lexer->getCursor() - 1));
}

/// ElementValuePairs: ElementValuePair {, ElementValuePair }
/// ElementValuePair: Identifier = ElementValue
void Parser::parseElementValuePairs(std::vector<spElementValuePair> &pairs) {
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    return;
  }

  // Lookahed for a '='.
  State identifierState;
  saveState(identifierState);
  lexer->getNextToken(); // consume Identifier
  if (lexer->getCurToken() != TOK_OP_EQUALS) {
    restoreState(identifierState);
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
    diag->addErr(ERR_EXP_ELEMENT_VALUE,
      lexer->getCurTokenIni(), lexer->getCursor());
  }

  // Even if we have an error while parsing the element value we add the pair
  // indicating this is an ElementValuePair node.
  pairs.push_back(pair);

  if (lexer->getCurToken() == TOK_COMMA) {
    lexer->getNextToken(); // consume ','
    parseElementValuePairs(pairs);
  }
}

/// ExplicitGenericInvocation:
///   NonWildcardTypeArguments ExplicitGenericInvocationSuffix
void Parser::parseExplicitGenericInvocation(
  spExplicitGenericInvocation &explGenInvocation) {

  // NonWildcardTypeArguments
  explGenInvocation->nonWildcardTypeArguments = spNonWildcardTypeArguments(
    new NonWildcardTypeArguments());
  parseNonWildcardTypeArguments(explGenInvocation->nonWildcardTypeArguments);
  if (explGenInvocation->nonWildcardTypeArguments->err) {
    explGenInvocation->addErr(-1);
    return;
  }

  // ExplicitGenericInvocationSuffix
  explGenInvocation->explGen = spExplicitGenericInvocationSuffix(
    new ExplicitGenericInvocationSuffix());
  parseExplicitGenericInvocationSuffix(explGenInvocation->explGen);
  if (explGenInvocation->explGen->err) {
    explGenInvocation->addErr(-1);
  }
}

/// ExplicitGenericInvocationSuffix:
///   super SuperSuffix
///   Identifier Arguments
void Parser::parseExplicitGenericInvocationSuffix(
  spExplicitGenericInvocationSuffix &explGen) {

  // opt1
  if (lexer->getCurToken() == TOK_KEY_SUPER) {
    explGen->opt = ExplicitGenericInvocationSuffix::OPT_SUPER;

    // Token 'super'
    explGen->tokSuper = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));

    lexer->getNextToken(); // consume 'super';

    explGen->superSuffix = spSuperSuffix(new SuperSuffix());
    parseSuperSuffix(explGen->superSuffix);

    if (explGen->superSuffix->err) {
      explGen->addErr(-1);
    }

    return;
  }

  // opt2
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    explGen->opt = ExplicitGenericInvocationSuffix::OPT_IDENTIFIER;

    // Identifier
    explGen->id = spIdentifier(
      new Identifier(lexer->getCurTokenIni(), lexer->getCurTokenStr()));

    lexer->getNextToken(); // consume Identifier

    // Arguments
    explGen->args = spArguments(new Arguments);
    parseArguments(explGen->args);

    if (explGen->args->err) {
      explGen->addErr(-1);
    }

    return;
  }

  // Error
  explGen->addErr(diag->addErr(
    ERR_NVAL_EXPLICIT_GENERIC_INVOCATION_SUFFIX, lexer->getCursor() - 1));
}


/// PackageDeclaration: [ [Annotations]  package QualifiedIdentifier ; ]
spPackageDeclaration Parser::parsePackageDeclaration(
  std::vector<spAnnotation> &annotations, spPackageDeclaration &pkgDecl) {

  // If we have annotations they belong to the package declaration
  if (annotations.size()) {
    pkgDecl->annotations = annotations;
    annotations.clear();
  }

  // package
  pkgDecl->tokPackage = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // Consume 'package'

  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    pkgDecl->addErr(diag->addErr(ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return pkgDecl;
  }

  pkgDecl->qualifiedId = spQualifiedIdentifier(new QualifiedIdentifier);
  parseQualifiedIdentifier(pkgDecl->qualifiedId);
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    pkgDecl->err = true;
    return pkgDecl;
  }

  lexer->getNextToken(); // Consume ';'
  return pkgDecl;
}

/// PostfixOp: ++ | --
void Parser::parsePostfixOp(spPostfixOp &postfixOp) {
  if (lexer->getCurToken() == TOK_OP_MINUS_MINUS) {
    postfixOp->opt = PostfixOp::OPT_MINUS_MINUS;
    postfixOp->pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '--'
    return;
  }

  if (lexer->getCurToken() == TOK_OP_PLUS_PLUS) {
    postfixOp->opt = PostfixOp::OPT_PLUS_PLUS;
    postfixOp->pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '++'
    return;
  }
}

/// ImportDeclarations:
///   ImportDeclaration
///   ImportDeclarations ImportDeclaration
/// ImportDeclaration: import [static] QualifiedId [.*] ';'
spImportDeclarations Parser::parseImportDeclarations() {
  std::vector<spImportDeclaration> imports;
  while (lexer->getCurToken() == TOK_KEY_IMPORT) {
    spImportDeclaration import = parseImportDeclaration();
    imports.push_back(import);
  }

  spImportDeclarations impDecls = spImportDeclarations(
    new ImportDeclarations(imports));
  return impDecls;
}

spImportDeclaration Parser::parseImportDeclaration() {
  spImportDeclaration import = spImportDeclaration(new ImportDeclaration);
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

  import->qualifiedId = spQualifiedIdentifier(new QualifiedIdentifier);
  parseQualifiedIdentifier(import->qualifiedId);

  // Check [.*]
  if (lexer->getCurToken() == TOK_PERIOD) {
    import->iniOnDemand = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '.'
    if (lexer->getCurToken() != TOK_OP_MUL) {
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

  lexer->getNextToken(); // consume ';'
  return import;
}

/// InnerCreator:
///   Identifier [NonWildcardTypeArgumentsOrDiamond] ClassCreatorRest
void Parser::parseInnerCreator(spInnerCreator &innerCreator) {
  // Error: expected Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    innerCreator->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  // Identifier
  innerCreator->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier

  // NonWildcardTypeArgumentsOrDiamond
  if (lexer->getCurToken() == TOK_OP_LT) {
    innerCreator->nonWildcardOrDiam = spNonWildcardTypeArgumentsOrDiamond(
      new NonWildcardTypeArgumentsOrDiamond());
    parseNonWildcardTypeArgumentsOrDiamond(innerCreator->nonWildcardOrDiam);

    // Error: invalid NonWildcardTypeArgumentsOrDiamond
    if (innerCreator->nonWildcardOrDiam->err) {
      innerCreator->addErr(-1);
      return;
    }
  }

  // ClassCreatorRest
  innerCreator->classCreatorRest = spClassCreatorRest(new ClassCreatorRest);
  parseClassCreatorRest(innerCreator->classCreatorRest);
  if (innerCreator->classCreatorRest->err) {
    innerCreator->addErr(-1);
  }
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
    literal->intLiteral = spIntegerLiteral(new IntegerLiteral);
    parseIntegerLiteral(literal->intLiteral);
    return;
  }

  if (isFloatingPointLiteral(lexer->getCurToken())) {
    literal->opt = Literal::OPT_FLOATING_POINT;
    literal->fpLiteral = spFloatingPointLiteral(new FloatingPointLiteral);
    parseFloatingPointLiteral(literal->fpLiteral);
    return;
  }

  // CharacterLiteral
  if (lexer->getCurToken() == TOK_CHARACTER_LITERAL) {
    literal->opt = Literal::OPT_CHAR;
    literal->charLiteral = spCharacterLiteral(new CharacterLiteral);
    parseCharacterLiteral(literal->charLiteral);
    return;
  }

  // StringLiteral
  if (lexer->getCurToken() == TOK_STRING_LITERAL) {
    literal->opt = Literal::OPT_STRING;
    literal->strLiteral = spStringLiteral(new StringLiteral);
    parseStringLiteral(literal->strLiteral);
    return;
  }

  if (lexer->getCurToken() == TOK_BOOLEAN_LITERAL) {
    literal->opt = Literal::OPT_BOOLEAN;
    literal->boolLiteral = spBooleanLiteral(new BooleanLiteral);
    parseBooleanLiteral(literal->boolLiteral);
    return;
  }

  if (lexer->getCurToken() == TOK_NULL_LITERAL) {
    literal->opt = Literal::OPT_NULL;
    literal->nullLiteral = spNullLiteral(new NullLiteral);
    parseNullLiteral(literal->nullLiteral);
  }
}

/// LocalVariableDeclarationStatement:
///   { VariableModifier } Type VariableDeclarators ;
void Parser::parseLocalVariableDeclarationStatement(
  spLocalVariableDeclarationStatement &localVar) {

  // { VariableModifier }
  localVar->varModifier = spVariableModifier(new VariableModifier);
  parseVariableModifier(localVar->varModifier);
  if (localVar->varModifier->err) {
    localVar->addErr(-1);
    return;
  }

  // Type
  localVar->type = spType(new Type);
  parseType(localVar->type);
  if (localVar->type->err) {
    localVar->addErr(-1);
    return;
  }

  // VariableDeclarators
  localVar->varDecls = spVariableDeclarators(new VariableDeclarators);
  parseVariableDeclarators(localVar->varDecls);
  if (localVar->varDecls->err) {
    localVar->addErr(-1);
    return;
  }

  // ';'
  if (lexer->getCurToken() == TOK_SEMICOLON) {
    localVar->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'
    return;
  }

  // Error
  localVar->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
}

/// Primary:
///   (1) Literal
///   (2) ParExpression
///   (3) this [Arguments]
///   (4) super SuperSuffix
///   (5) new Creator
///   (6) NonWildcardTypeArguments
///     ( ExplicitGenericInvocationSuffix | this Arguments )
///   (7) Identifier { . Identifier } [IdentifierSuffix]
///   (8) BasicType {[]} . class
///   (9) void . class
void Parser::parsePrimary(spPrimary &primary) {
  // (6) NonWildcardTypeArguments
  if (lexer->getCurToken() == TOK_OP_LT) {
    primary->opt = Primary::OPT_NON_WILDCARD_TYPE_ARGUMENTS;
    primary->nonWildcardTypeArguments = spPrimaryNonWildcardTypeArguments(
      new PrimaryNonWildcardTypeArguments);
    parsePrimaryNonWildcardTypeArguments(primary->nonWildcardTypeArguments);
    if (primary->nonWildcardTypeArguments->err) {
      primary->addErr(-1);
    }
    return;
  }

  // (2) ParExpression
  if (lexer->getCurToken() == TOK_LPAREN) {
    spParExpression parExpr = spParExpression(new ParExpression);
    parseParExpression(parExpr);
    if (parExpr->err) { return; }

    primary->opt = Primary::OPT_PAR_EXPRESSION;
    primary->parExpr = parExpr;
    return;
  }

  // (3) this [Arguments]
  if (lexer->getCurToken() == TOK_KEY_THIS) {
    primary->opt = Primary::OPT_THIS_ARGUMENTS;
    primary->thisArgs = spPrimaryThisArguments(new PrimaryThisArguments);
    parsePrimaryThisArguments(primary->thisArgs);
    return;
  }

  // (4) super SuperSuffix
  if (lexer->getCurToken() == TOK_KEY_SUPER) {
    primary->opt = Primary::OPT_SUPER_SUPER_SUFFIX;
    primary->superSuperSuffix = spPrimarySuperSuperSuffix(
      new PrimarySuperSuperSuffix);
    parsePrimarySuperSuperSuffix(primary->superSuperSuffix);
    return;
  }

  // (5) new Creator
  if (lexer->getCurToken() == TOK_KEY_NEW) {
    primary->opt = Primary::OPT_NEW_CREATOR;
    primary->newCreator = spPrimaryNewCreator(new PrimaryNewCreator);
    parsePrimaryNewCreator(primary->newCreator);
    return;
  }

  // (7) Identifier { . Identifier } [IdentifierSuffix]
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    primary->opt = Primary::OPT_IDENTIFIER;
    primary->primaryId = spPrimaryIdentifier(new PrimaryIdentifier);
    parsePrimaryIdentifier(primary->primaryId);
    if (primary->primaryId->err) {
      primary->addErr(-1);
    }
    return;
  }

  // (8) BasicType {[]} . class
  if (isBasicType(lexer->getCurToken())) {
    primary->opt = Primary::OPT_BASIC_TYPE;
    primary->primaryBasicType = spPrimaryBasicType(new PrimaryBasicType);
    parsePrimaryBasicType(primary->primaryBasicType);
    return;
  }

  // (9) void . class
  if (lexer->getCurToken() == TOK_KEY_VOID) {
    primary->opt = Primary::OPT_VOID_CLASS;
    primary->primaryVoidClass = spPrimaryVoidClass(new PrimaryVoidClass);
    parsePrimaryVoidClass(primary->primaryVoidClass);
    return;
  }

  // (1) Literal
  spLiteral literal = spLiteral(new Literal);
  parseLiteral(literal);
  if (literal->isEmpty() == false) {
    primary->opt = Primary::OPT_LITERAL;
    primary->literal = literal;
    return;
  }
}

/// Primary: BasicType {[]} . class
void Parser::parsePrimaryBasicType(spPrimaryBasicType &primaryBasicType) {
  // Basic Type
  spTokenExp token = spTokenExp(new TokenExp(lexer->getCursor()
    - tokenUtil.getTokenLength(lexer->getCurToken()), lexer->getCurToken()));
  primaryBasicType->basicType = spBasicType(new BasicType(token));
  lexer->getNextToken(); // consume 'BasicType'

  // {[]}
  parseArrayDepth(primaryBasicType->arrayDepth);

  // '.'
  if (lexer->getCurToken() != TOK_PERIOD) {
    primaryBasicType->addErr(diag->addErr(
      ERR_EXP_PERIOD, lexer->getCursor() - 1));
    return;
  }

  primaryBasicType->posPeriod = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '.'

  // 'class'
  if (lexer->getCurToken() != TOK_KEY_CLASS) {
    primaryBasicType->addErr(diag->addErr(
      ERR_EXP_CLASS, lexer->getCursor() - 1));
    return;
  }

  primaryBasicType->tokClass = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'class'
}

/// Primary: Identifier { . Identifier } [IdentifierSuffix]
void Parser::parsePrimaryIdentifier(spPrimaryIdentifier &primaryId) {

  // Identifier
  primaryId->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier

  // { . Identifier }
  State state;
  while (lexer->getCurToken() == TOK_PERIOD) {
    saveState(state);

    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '.'

    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      restoreState(state);
      break;
    }

    spIdentifier id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier

    primaryId->pairs.push_back(std::make_pair(pos, id));
  }

  // [IdentifierSuffix]
  if (lexer->getCurToken() == TOK_LBRACKET
    || lexer->getCurToken() == TOK_LPAREN
    || lexer->getCurToken() == TOK_PERIOD) {

    primaryId->idSuffix = spIdentifierSuffix(new IdentifierSuffix);
    parseIdentifierSuffix(primaryId->idSuffix);
    if (primaryId->idSuffix->err) {
      primaryId->addErr(-1);
    }
  }
}

/// Primary: this [Arguments]
void Parser::parsePrimaryThisArguments(
  spPrimaryThisArguments &primaryThisArgs) {

  // Token 'this'
  primaryThisArgs->tokThis = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));

  lexer->getNextToken(); // consume 'this'

  // [Arguments]
  if (lexer->getCurToken() == TOK_LPAREN) {
    primaryThisArgs->args = spArguments(new Arguments);
    parseArguments(primaryThisArgs->args);
  }
}

/// Primary: super SuperSuffix
void Parser::parsePrimarySuperSuperSuffix(
  spPrimarySuperSuperSuffix &primarySuperSuperSuffix) {

  // Token 'super'
  primarySuperSuperSuffix->tokSuper = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));

  lexer->getNextToken(); // consume 'super'

  // SuperSuffix
  if (lexer->getCurToken() == TOK_LPAREN
    || lexer->getCurToken() == TOK_PERIOD) {
    primarySuperSuperSuffix->superSuffix = spSuperSuffix(new SuperSuffix);
    parseSuperSuffix(primarySuperSuperSuffix->superSuffix);
  }
}

/// Primary: new Creator
void Parser::parsePrimaryNewCreator(spPrimaryNewCreator &primaryNewCreator) {
  // Token 'new'
  primaryNewCreator->tokNew = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));

  lexer->getNextToken(); // consume 'new'

  // Creator
  primaryNewCreator->creator = spCreator(new Creator);
  parseCreator(primaryNewCreator->creator);
}

/// Primary:
///   NonWildcardTypeArguments
///     ( ExplicitGenericInvocationSuffix | this Arguments )
void Parser::parsePrimaryNonWildcardTypeArguments(
  spPrimaryNonWildcardTypeArguments &primaryNonWildcard) {

  // NonWildcardTypeArguments
  primaryNonWildcard->nonWildcardTypeArguments = spNonWildcardTypeArguments(
    new NonWildcardTypeArguments);
  parseNonWildcardTypeArguments(primaryNonWildcard->nonWildcardTypeArguments);

  if (primaryNonWildcard->nonWildcardTypeArguments->err) {
    primaryNonWildcard->addErr(-1);
    return;
  }

  // opt1: ExplicitGenericInvocationSuffix
  if (lexer->getCurToken() == TOK_KEY_SUPER
    || lexer->getCurToken() == TOK_IDENTIFIER) {

    primaryNonWildcard->opt =
      PrimaryNonWildcardTypeArguments::OPT_EXPLICIT_GENERIC_INVOCATION_SUFFIX;
    primaryNonWildcard->explGen = spExplicitGenericInvocationSuffix(
      new ExplicitGenericInvocationSuffix());
    parseExplicitGenericInvocationSuffix(primaryNonWildcard->explGen);
    return;
  }

  // opt2: this Arguments
  if (lexer->getCurToken() == TOK_KEY_THIS) {
    primaryNonWildcard->opt =
      PrimaryNonWildcardTypeArguments::OPT_THIS_ARGUMENTS;

    // Token 'this'
    primaryNonWildcard->tokThis = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));

    lexer->getNextToken(); // consume 'this'

    // Arguments
    if (lexer->getCurToken() == TOK_LPAREN) {
      primaryNonWildcard->addErr(diag->addErr(
        ERR_EXP_ARGUMENTS, lexer->getCursor() - 1));
      return;
    }

    primaryNonWildcard->args = spArguments(new Arguments());
    parseArguments(primaryNonWildcard->args);

    if (primaryNonWildcard->err) {
      primaryNonWildcard->addErr(-1);
    }

    return;
  }

  primaryNonWildcard->addErr(-1);
}

/// Primary: void . class
void Parser::parsePrimaryVoidClass(spPrimaryVoidClass &primaryVoidClass) {
  if (lexer->getCurToken() != TOK_KEY_VOID) {
    primaryVoidClass->addErr(diag->addErr(
      ERR_EXP_VOID, lexer->getCursor() - 1));
    return;
  }

  // 'void'
  primaryVoidClass->tokVoid = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'void'

  // '.'
  if (lexer->getCurToken() != TOK_PERIOD) {
    primaryVoidClass->addErr(diag->addErr(
      ERR_EXP_PERIOD, lexer->getCursor() - 1));
    return;
  }

  primaryVoidClass->posPeriod = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '.'

  // 'class'
  if (lexer->getCurToken() != TOK_KEY_CLASS) {
    primaryVoidClass->addErr(diag->addErr(
      ERR_EXP_CLASS, lexer->getCursor() - 1));
    return;
  }

  primaryVoidClass->tokClass = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'class'
}

/// QualifiedIdentifier: Identifier { . Identifier }
void Parser::parseQualifiedIdentifier(
  spQualifiedIdentifier &qualifiedId) {

  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    qualifiedId->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  // QualifiedIdentifier
  qualifiedId->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume identifier

  // { . Identifier }
  while (lexer->getCurToken() == TOK_PERIOD) {
    State state;
    saveState(state);

    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '.'

    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      restoreState(state);
      return;
    }

    spIdentifier id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume identifier

    qualifiedId->pairs.push_back(std::make_pair(pos, id));
  }
}

/// QualifiedIdentifierList:
///   QualifiedIdentifier { , QualifiedIdentifier }
void Parser::parseQualifiedIdentifierList(
  spQualifiedIdentifierList &qualifiedIdList) {

  // QualifiedIdentifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    qualifiedIdList->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  qualifiedIdList->qualifiedId = spQualifiedIdentifier(new QualifiedIdentifier);
  parseQualifiedIdentifier(qualifiedIdList->qualifiedId);

  // { , QualifiedIdentifier }
  while (lexer->getCurToken() == TOK_COMMA) {
    State state;
    saveState(state);

    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','

    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      restoreState(state);
      return;
    }

    spQualifiedIdentifier qualifiedId
      = spQualifiedIdentifier(new QualifiedIdentifier);
    parseQualifiedIdentifier(qualifiedId);

    qualifiedIdList->pairs.push_back(std::make_pair(pos, qualifiedId));
  }
}

/// ReferenceType:
///    Identifier [TypeArguments] { . Identifier [TypeArguments] }
void Parser::parseReferenceType(spReferenceType &refType) {
  // indentifier
  refType->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume identifier

  // [TypeArguments]
  if (lexer->getCurToken() == TOK_OP_LT) {
    refType->typeArgs = spTypeArguments(new TypeArguments);
    parseTypeArguments(refType->typeArgs);
    if (refType->typeArgs->err) { refType->addErr(-1); }
  }

  // { . Identifier [TypeArguments] }
  State state;
  while (lexer->getCurToken() == TOK_PERIOD) {
    saveState(state);
    spReferenceTypeTriplet tri
      = spReferenceTypeTriplet(new ReferenceTypeTriplet);

    // '.'
    tri->posPeriod = lexer->getCursor() - 1;
    lexer->getNextToken();

    // Identifier
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      restoreState(state);
      return;
    }

    tri->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier

    // [TypeArguments]
    if (lexer->getCurToken() == TOK_OP_LT) {
      tri->typeArgs = spTypeArguments(new TypeArguments);
      parseTypeArguments(tri->typeArgs);
      if (tri->typeArgs->err) {
        restoreState(state);
        return;
      }
    }

    refType->triplets.push_back(tri);
  }
}

/// Resource:
///   {VariableModifier} ReferenceType VariableDeclaratorId = Expression
void Parser::parseResource(spResource &res) {
  // {VariableModifier}
  res->varModifier = spVariableModifier(new VariableModifier);
  parseVariableModifier(res->varModifier);

  // ReferenceType
  res->refType = spReferenceType(new ReferenceType);
  parseReferenceType(res->refType);
  if (res->refType->err) {
    res->addErr(-1);
    return;
  }

  // VariableDeclaratorId
  res->varDeclId = spVariableDeclaratorId(new VariableDeclaratorId);
  parseVariableDeclaratorId(res->varDeclId);
  if (res->varDeclId->err) {
    res->addErr(-1);
    return;
  }

  // =
  res->posEquals = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '='

  // Expression
  res->expr = spExpression(new Expression);
  parseExpression(res->expr);
  if (res->expr->isEmpty()) {
    res->addErr(-1);
  }
}

/// Resources:
///   Resource { ; Resource }
void Parser::parseResources(spResources &resources) {
  // Resource
  resources->res = spResource(new Resource);
  parseResource(resources->res);
  if (resources->res->err) {
    resources->addErr(-1);
    return;
  }

  // { ; Resource }
  State state;
  while (lexer->getCurToken() == TOK_SEMICOLON) {
    saveState(state);
    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'

    spResource res = spResource(new Resource);
    parseResource(res);
    if (res->err) {
      // We let upper layers handle the error
      restoreState(state);
      return;
    }

    resources->pairs.push_back(std::make_pair(pos, res));
  }
}

/// ResourceSpecification:
///   '(' Resources [;] ')'
void Parser::parseResourceSpecification(spResourceSpecification &resSpec) {
  // '('
  if (lexer->getCurToken() != TOK_LPAREN) {
    resSpec->addErr(diag->addErr(ERR_EXP_LPAREN, lexer->getCursor() - 1));
    return;
  }

  resSpec->posLParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '('

  // Resources
  resSpec->resources = spResources(new Resources);
  parseResources(resSpec->resources);
  if (resSpec->resources->err) {
    resSpec->addErr(-1);
    return;
  }

  // [;]
  if (lexer->getCurToken() == TOK_SEMICOLON) {
    resSpec->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'
  }

  // ')'
  if (lexer->getCurToken() != TOK_RPAREN) {
    resSpec->addErr(diag->addErr(ERR_EXP_RPAREN, lexer->getCursor() - 1));
    return;
  }

  resSpec->posRParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ')'
}

/// Selector:
///   . Identifier [Arguments]
///   . ExplicitGenericInvocation
///   . this
///   . super SuperSuffix
///   . new [NonWildcardTypeArguments] InnerCreator
///   '[' Expression ']'
void Parser::parseSelector(spSelector &selector) {
  if (lexer->getCurToken() == TOK_PERIOD) {
    selector->posPeriod = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '.'

    // . Identifier [Arguments]
    if (lexer->getCurToken() == TOK_IDENTIFIER) {
      selector->opt = Selector::OPT_IDENTIFIER_ARGUMENTS;
      selector->id = spIdentifier(new Identifier(
        lexer->getCurTokenIni(), lexer->getCurTokenStr()));
      lexer->getNextToken(); // consume Identifier

      // [Arguments]
      if (lexer->getCurToken() == TOK_LPAREN) {
        selector->args = spArguments(new Arguments);
        parseArguments(selector->args);
        if (selector->args->err) { selector->addErr(-1); }
      }

      return;
    }

    // . ExplicitGenericInvocation
    if (lexer->getCurToken() == TOK_OP_LT) {
      selector->opt = Selector::OPT_IDENTIFIER_ARGUMENTS;
      selector->explGenInvocation = spExplicitGenericInvocation(
        new ExplicitGenericInvocation());
      parseExplicitGenericInvocation(selector->explGenInvocation);
      if (selector->explGenInvocation->err) { selector->addErr(-1); }
      return;
    }

    // . this
    if (lexer->getCurToken() == TOK_KEY_THIS) {
      selector->opt = Selector::OPT_THIS;
      selector->tokThis = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'this'
      return;
    }

    /// . super SuperSuffix
    if (lexer->getCurToken() == TOK_KEY_SUPER) {
      selector->opt = Selector::OPT_SUPER_SUPER_SUFFIX;
      selector->tokSuper = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'super'

      selector->superSuffix = spSuperSuffix(new SuperSuffix());
      parseSuperSuffix(selector->superSuffix);
      if (selector->superSuffix->err) { selector->addErr(-1); }

      return;
    }

    /// . new [NonWildcardTypeArguments] InnerCreator
    if (lexer->getCurToken() == TOK_KEY_NEW) {
      selector->opt = Selector::OPT_NEW;
      selector->tokNew = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'new'

      // [NonWildcardTypeArguments]
      if (lexer->getCurToken() == TOK_OP_LT) {
        selector->nonWildcardTypeArguments = spNonWildcardTypeArguments(
          new NonWildcardTypeArguments());
        parseNonWildcardTypeArguments(selector->nonWildcardTypeArguments);
        if (selector->nonWildcardTypeArguments->err) {
          selector->addErr(-1);
          return;
        }
      }

      // InnerCreator
      selector->innerCreator = spInnerCreator(new InnerCreator());
      parseInnerCreator(selector->innerCreator);
      if (selector->innerCreator->err) { selector->addErr(-1); }
      return;
    }
  }

  /// '[' Expression ']'
  if (lexer->getCurToken() == TOK_LBRACKET) {
    selector->opt = Selector::OPT_EXPRESSION;

    // '['
    selector->arrayPair.first = lexer->getCursor() - 1;
    selector->arrayPair.second = 0;
    lexer->getNextToken(); // consume '['

    // Expression
    selector->expr = spExpression(new Expression);
    parseExpression(selector->expr);
    if (selector->expr->isEmpty()) {
      selector->addErr(-1);
    }

    // ']'
    if (lexer->getCurToken() == TOK_RBRACKET) {
      selector->arrayPair.second = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume ']'
      return;
    }

    selector->addErr(diag->addErr(ERR_EXP_RBRACKET, lexer->getCursor() - 1));
    return;
  }

  selector->addErr(diag->addErr(ERR_NVAL_SELECTOR, lexer->getCursor() - 1));
}

/// Statement:
///   (1) Block
///   (2) ;
///   (3) Identifier : Statement
///   (4) StatementExpression ;
///   (5) if ParExpression Statement [else Statement]
///   (6) assert Expression [: Expression] ;
///   (7) switch ParExpression '{' SwitchBlockStatementGroups '}'
///   (8) while ParExpression Statement
///   (9) do Statement while ParExpression ;
///   (10) for '(' ForControl ')' Statement
///   (11) break [Identifier] ;
///   (12) continue [Identifier] ;
///   (13) return [Expression] ;
///   (14) throw Expression ;
///   (15) synchronized ParExpression Block
///   (16) try Block ( Catches | [Catches] Finally )
///   (17) try ResourceSpecification Block [Catches] [Finally]
void Parser::parseStatement(spStatement &stmt) {
  // (1) Block
  if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
    stmt->opt = Statement::OPT_BLOCK;
    stmt->block = spBlock(new Block);
    parseBlock(stmt->block);
    return;
  }

  // (2) ;
  if (lexer->getCurToken() == TOK_SEMICOLON) {
    stmt->opt = Statement::OPT_SEMI_COLON;
    stmt->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'
    return;
  }

  // We skip (3) and (4)

  // (5) if ParExpression Statement [else Statement]
  if (lexer->getCurToken() == TOK_KEY_IF) {
    // 'if'
    stmt->opt = Statement::OPT_IF;
    stmt->tokIf = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'if'

    // ParExpression
    stmt->parExpr = spParExpression(new ParExpression);
    parseParExpression(stmt->parExpr);
    if (stmt->parExpr->err) {
      stmt->addErr(-1);
      return;
    }

    // Statement
    stmt->stmtIf = spStatement(new Statement);
    parseStatement(stmt->stmtIf);
    if (stmt->stmtIf->err) {
      stmt->addErr(-1);
      return;
    }

    // [else Statement]
    if (lexer->getCurToken() == TOK_KEY_ELSE) {
      stmt->tokElse = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'else'

      stmt->stmtElse = spStatement(new Statement);
      parseStatement(stmt->stmtElse);
      if (stmt->stmtElse->err) { stmt->addErr(-1); }
    }

    return;
  }

  // (6) assert Expression [: Expression] ;
  if (lexer->getCurToken() == TOK_KEY_ASSERT) {
    stmt->opt = Statement::OPT_ASSERT;
    // assert
    stmt->tokAssert = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'assert'

    // Expression
    spExpression expr = spExpression(new Expression);
    parseExpression(expr);
    if (expr->isEmpty()) {
      stmt->addErr(-1);
      return;
    }

    stmt->exprAssert1 = expr;

    // [: Expression]
    if (lexer->getCurToken() == TOK_OP_COLON) {
      stmt->posColon = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume ':'

      spExpression expr = spExpression(new Expression);
      parseExpression(expr);
      if (expr->isEmpty()) {
        stmt->addErr(-1);
        return;
      }

      stmt->exprAssert2 = expr;
    }

    // ';'
    if (lexer->getCurToken() != TOK_SEMICOLON) {
      stmt->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
      return;
    }

    stmt->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'
    return;
  }

  // (7) switch ParExpression '{' SwitchBlockStatementGroups '}'
  if (lexer->getCurToken() == TOK_KEY_SWITCH) {
    // switch
    stmt->opt = Statement::OPT_SWITCH;
    stmt->tokSwitch = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'switch'

    // ParExpression
    stmt->parExpr = spParExpression(new ParExpression);
    parseParExpression(stmt->parExpr);
    if (stmt->parExpr->err) {
      stmt->addErr(-1);
      return;
    }

    // '{'
    if (lexer->getCurToken() != TOK_LCURLY_BRACKET) {
      stmt->addErr(diag->addErr(
        ERR_EXP_LCURLY_BRACKET, lexer->getCursor() - 1));
      return;
    }

    stmt->posLCBrace = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '{'

    // SwitchBlockStatementGroups
    stmt->switchStmtGroups = spSwitchBlockStatementGroups(
      new SwitchBlockStatementGroups);
    parseSwitchBlockStatementGroups(stmt->switchStmtGroups);
    if (stmt->switchStmtGroups->err) {
      stmt->addErr(-1);
      return;
    }

    // '}'
    if (lexer->getCurToken() != TOK_RCURLY_BRACKET) {
      stmt->addErr(diag->addErr(
        ERR_EXP_RCURLY_BRACKET, lexer->getCursor() - 1));
      return;
    }

    stmt->posRCBrace = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '}'

    return;
  }

  // (8) while ParExpression Statement
  if (lexer->getCurToken() == TOK_KEY_WHILE) {
    // while
    stmt->opt = Statement::OPT_WHILE;
    stmt->tokWhile = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'while'

    // ParExpression
    stmt->parExpr = spParExpression(new ParExpression);
    parseParExpression(stmt->parExpr);
    if (stmt->parExpr->err) {
      stmt->addErr(-1);
      return;
    }

    // Statement
    stmt->stmtWhile = spStatement(new Statement);
    parseStatement(stmt->stmtWhile);
    if (stmt->stmtWhile->err) {
      stmt->addErr(-1);
    }

    return;
  }

  // (9) do Statement while ParExpression ;
  if (lexer->getCurToken() == TOK_KEY_DO) {
    stmt->opt = Statement::OPT_DO;

    // do
    stmt->tokDo = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'do'

    // Statement
    stmt->stmtDo = spStatement(new Statement);
    parseStatement(stmt->stmtDo);
    if (stmt->stmtDo->err) {
      stmt->addErr(-1);
      return;
    }

    // while
    if (lexer->getCurToken() != TOK_KEY_WHILE) {
      stmt->addErr(diag->addErr(ERR_EXP_WHILE, lexer->getCursor() - 1));
      return;
    }

    stmt->tokWhile = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'while'

    // ParExpression
    stmt->parExpr = spParExpression(new ParExpression);
    parseParExpression(stmt->parExpr);
    if (stmt->parExpr->err) {
      stmt->addErr(-1);
      return;
    }

    // ';'
    if (lexer->getCurToken() != TOK_SEMICOLON) {
      stmt->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
      return;
    }

    stmt->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'
    return;
  }

  // (10) for '(' ForControl ')' Statement
  if (lexer->getCurToken() == TOK_KEY_FOR) {
    stmt->opt = Statement::OPT_FOR;

    // for
    stmt->tokFor = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'for'

    // '('
    if (lexer->getCurToken() != TOK_LPAREN) {
      stmt->addErr(diag->addErr(ERR_EXP_LPAREN, lexer->getCurTokenIni() - 1));
      return;
    }

    stmt->posLParen = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '('

    // ForControl
    stmt->forCtrl = spForControl(new ForControl);
    parseForControl(stmt->forCtrl);
    if (stmt->forCtrl->err) {
      // We don't exit and try to parse the next token.
      stmt->addErr(-1);
    }

    // ')'
    if (lexer->getCurToken() != TOK_RPAREN) {
      stmt->addErr(diag->addErr(ERR_EXP_RPAREN, lexer->getCurTokenIni() - 1));
      return;
    }

    stmt->posRParen = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ')'

    // Statement
    stmt->stmtFor = spStatement(new Statement);
    parseStatement(stmt->stmtFor);
    if (stmt->stmtFor->err) {
      stmt->addErr(-1);
    }

    return;
  }

  // (11) break [Identifier] ;
  if (lexer->getCurToken() == TOK_KEY_BREAK) {
    stmt->opt = Statement::OPT_BREAK;
    stmt->tokBreak = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'break'

    if (lexer->getCurToken() == TOK_IDENTIFIER) {
      stmt->id = spIdentifier(new Identifier(
        lexer->getCurTokenIni(), lexer->getCurTokenStr()));
      lexer->getNextToken(); // consume Identifier
    }

    if (lexer->getCurToken() != TOK_SEMICOLON) {
      stmt->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
      return;
    }

    stmt->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'

    return;
  }

  // (12) continue [Identifier] ;
  if (lexer->getCurToken() == TOK_KEY_CONTINUE) {
    // continue
    stmt->opt = Statement::OPT_CONTINUE;
    stmt->tokContinue = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'continue'

    // [Identifier]
    if (lexer->getCurToken() == TOK_IDENTIFIER) {
      stmt->id = spIdentifier(new Identifier(
        lexer->getCurTokenIni(), lexer->getCurTokenStr()));
      lexer->getNextToken(); // consume Identifier
    }

    if (lexer->getCurToken() != TOK_SEMICOLON) {
      stmt->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
      return;
    }

    // ';'
    stmt->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'

    return;
  }

  // (13) return [Expression] ;
  if (lexer->getCurToken() == TOK_KEY_RETURN) {
    stmt->opt = Statement::OPT_RETURN;
    // 'return'
    stmt->tokReturn = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'return'

    // [Expression]
    if (lexer->getCurToken() != TOK_SEMICOLON) {
      stmt->exprReturn = spExpression(new Expression);
      parseExpression(stmt->exprReturn);
      if (stmt->exprReturn->isEmpty()) {
        stmt->addErr(-1);
        return;
      }
    }

    // ';'
    if (lexer->getCurToken() == TOK_SEMICOLON) {
      stmt->posSemiColon = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume ';'
      return;
    }

    stmt->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
    return;
  }

  // (14) throw Expression ;
  if (lexer->getCurToken() == TOK_KEY_THROW) {
    stmt->opt = Statement::OPT_THROW;
    stmt->tokThrow = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'throw'

    stmt->throwExpr = spExpression(new Expression);
    parseExpression(stmt->throwExpr);
    if (stmt->throwExpr->isEmpty()) {
      stmt->addErr(-1);
      return;
    }

    if (lexer->getCurToken() == TOK_SEMICOLON) {
      stmt->posSemiColon = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume ';'
      return;
    }

    stmt->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
    return;
  }

  // (15) synchronized ParExpression Block
  if (lexer->getCurToken() == TOK_KEY_SYNCHRONIZED) {
    // synchronized
    stmt->opt = Statement::OPT_SYNC;
    stmt->tokSync = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'synchronized'

    // ParExpression
    stmt->parExpr = spParExpression(new ParExpression);
    parseParExpression(stmt->parExpr);
    if (stmt->parExpr->err) {
      stmt->addErr(-1);
      return;
    }

    // Block
    stmt->block = spBlock(new Block);
    parseBlock(stmt->block);
    if (stmt->block->err) {
      stmt->addErr(-1);
    }

    return;
  }

  // (16) try Block ( Catches | [Catches] Finally )
  // (17) try ResourceSpecification Block [Catches] [Finally]
  if (lexer->getCurToken() == TOK_KEY_TRY) {
    stmt->tokTry = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'try'

    // (16) try Block ( Catches | [Catches] Finally )
    if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
      stmt->opt = Statement::OPT_TRY_BLOCK;

      // Block
      stmt->block = spBlock(new Block);
      parseBlock(stmt->block);
      if (stmt->block->err) {
        stmt->addErr(-1);
        return;
      }

      // Catches
      if (lexer->getCurToken() == TOK_KEY_CATCH) {
        stmt->catches = spCatches(new Catches);
        parseCatches(stmt->catches);
        if (stmt->catches->err) {
          stmt->addErr(-1);
          return;
        }
      }

      // Finally
      if (lexer->getCurToken() == TOK_KEY_FINALLY) {
        stmt->finally = spFinally(new Finally);
        parseFinally(stmt->finally);
        if (stmt->finally->err) {
          stmt->addErr(-1);
          return;
        }
      }
      return;
    }

    // (17) try ResourceSpecification Block [Catches] [Finally]
    // ResourceSpecification
    stmt->resSpec = spResourceSpecification(new ResourceSpecification);
    parseResourceSpecification(stmt->resSpec);
    if (stmt->resSpec->err) {
      stmt->addErr(-1);
      return;
    }

    // Block
    stmt->block = spBlock(new Block);
    parseBlock(stmt->block);
    if (stmt->block->err) {
      stmt->addErr(-1);
      return;
    }

    // [Catches]
    if (lexer->getCurToken() == TOK_KEY_CATCH) {
      stmt->catches = spCatches(new Catches);
      parseCatches(stmt->catches);
      if (stmt->catches->err) {
	stmt->addErr(-1);
	return;
      }
    }

    // [Finally]
    if (lexer->getCurToken() == TOK_KEY_FINALLY) {
      stmt->finally = spFinally(new Finally);
      parseFinally(stmt->finally);
      if (stmt->finally->err) {
	stmt->addErr(-1);
      }
    }

    return;
  }

  // (3) Identifier : Statement
  // (4) StatementExpression ;
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    State state;
    saveState(state);
    spIdentifier id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier

    // (3)
    if (lexer->getCurToken() == TOK_OP_COLON) {
      stmt->opt = Statement::OPT_ID_STMT;
      stmt->id = id;
      stmt->posColon = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume ':'
      stmt->stmt = spStatement(new Statement);
      parseStatement(stmt->stmt);
      if (stmt->stmt->err) { stmt->addErr(-1); }
      return;
    }

    restoreState(state);
  }

  // (4)
  stmt->opt = Statement::OPT_STMT_EXPR;
  stmt->stmtExpr = spStatementExpression(new StatementExpression);
  parseStatementExpression(stmt->stmtExpr);
  if (stmt->stmtExpr->err) { stmt->addErr(-1); }

  // ';'
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    stmt->addErr(diag->addErr(ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
  }

  stmt->posSemiColon = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ';'
}

/// StatementExpression: Expression
void Parser::parseStatementExpression(spStatementExpression &stmtExpr) {
  stmtExpr->expr = spExpression(new Expression);
  parseExpression(stmtExpr->expr);
  if (stmtExpr->expr->isEmpty()) { stmtExpr->addErr(-1); }
}

/// TypeArgument:
///   Type
///   ? [(extends|super) Type]
void Parser::parseTypeArgument(spTypeArgument &typeArg) {
  // option 1
  if (lexer->getCurToken() == TOK_IDENTIFIER
    || isBasicType(lexer->getCurToken())) {
    typeArg->opt = TypeArgument::OPT_TYPE;
    typeArg->type = spType(new Type);
    parseType(typeArg->type);
    // TODO: it's an error if we have a non-array basic type
    return;
  }

  // option 2
  if (lexer->getCurToken() == TOK_OP_QUESTION_MARK) {
    typeArg->opt = TypeArgument::OPT_QUESTION_MARK;
    typeArg->opt2 = spTypeArgumentOpt2(new TypeArgumentOpt2);
    parseTypeArgumentOpt2(typeArg->opt2);
    return;
  }

  // error
  typeArg->addErr(diag->addErr(ERR_NVAL_TYPE_ARGUMENT, lexer->getCursor() - 1));
}

/// TypeArgument: ? [(extends|super) Type ]
void Parser::parseTypeArgumentOpt2(spTypeArgumentOpt2 &opt2) {
  opt2->posQuestionMark = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '?'

  // [(extends|super) Type]
  if (lexer->getCurToken() != TOK_KEY_EXTENDS &&
    lexer->getCurToken() != TOK_KEY_SUPER) {
    return;
  }

  // Token 'extends' or 'super'
  opt2->tokExtendsOrSuper = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume token

  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    opt2->addErr(diag->addErr(ERR_EXP_REFTYPE, lexer->getCursor() - 1));
    return;
  }

  // Type
  opt2->type = spType(new Type);
  parseType(opt2->type);
  // TODO: it's an error if we have a non-array basic type
}

/// TypeArguments: < TypeArgument { , TypeArgument } >
void Parser::parseTypeArguments(spTypeArguments &typeArgs) {
  typeArgs->posLt = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '<'

  spTypeArgument typeArg = spTypeArgument(new TypeArgument);
  typeArgs->typeArg = typeArg;
  parseTypeArgument(typeArgs->typeArg);
  if (typeArg->err) {
    typeArgs->addErr(-1);
    return;
  }

  // Additional TypeArgument list
  State state;
  while (lexer->getCurToken() == TOK_COMMA) {
    saveState(state);

    // ,
    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','

    // TypeArgument
    spTypeArgument typeArgTmp = spTypeArgument(new TypeArgument);
    parseTypeArgument(typeArgTmp);
    if (typeArgTmp->err) {
      typeArgs->addErr(-1);
      return;
    }

    typeArgs->pairs.push_back(std::make_pair(pos, typeArgTmp));
  }

  if (lexer->getCurToken() == TOK_OP_GT) {
    typeArgs->posGt = lexer->getCursor() - 1;
    lexer->getNextToken();
    return;
  }

  // We also check for '>>' and '>>>' before flagging an error.
  // Ex.: A<B<T>>
  if (lexer->getCurToken() == TOK_OP_RSHIFT) {
    typeArgs->posGt = lexer->getCursor() - 2;
    src->ungetChar(1);
    lexer->getNextToken();
    return;
  }

  if (lexer->getCurToken() == TOK_OP_TRIPLE_RSHIFT) {
    typeArgs->posGt = lexer->getCursor() - 2;
    src->ungetChar(2);
    lexer->getNextToken();
    return;
  }

  // error
  typeArgs->addErr(diag->addErr(ERR_EXP_OP_GT, lexer->getCursor() - 1));
}

/// TypeArgumentsOrDiamond:
///   < >
///   TypeArguments
void Parser::parseTypeArgumentsOrDiamond(
  spTypeArgumentsOrDiamond &typeArgsOrDiam) {

  int posLt = lexer->getCursor() - 1;
  if (lexer->getCurToken() != TOK_OP_LT) {
    typeArgsOrDiam->addErr(diag->addErr(
      ERR_EXP_OP_LT, posLt, lexer->getCursor()));
    return;
  }

  // We need to look ahead for the next token.
  State ltState;
  saveState(ltState);
  lexer->getNextToken(); // consume '<'

  // Diamond
  if (lexer->getCurToken() == TOK_OP_GT) {
    typeArgsOrDiam->opt = TypeArgumentsOrDiamond::OPT_DIAMOND;
    typeArgsOrDiam->posLt = posLt;
    typeArgsOrDiam->posGt = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '>'
    return;
  }

  // At this point we know that we don't have a diamond, and we parse
  // TypeArguments.
  restoreState(ltState);
  typeArgsOrDiam->opt = TypeArgumentsOrDiamond::OPT_TYPE_ARGUMENTS;
  // GCC(4.6.3). If we don't create typeArgs separately and then assign
  // it to typeArgsOrDiam things get weird with the last 'if' condition.
  // Clang is fine either way.
  spTypeArguments typeArgs = spTypeArguments(new TypeArguments);
  typeArgsOrDiam->typeArgs = typeArgs;
  parseTypeArguments(typeArgsOrDiam->typeArgs);

  if (typeArgsOrDiam->typeArgs->err) {
    typeArgsOrDiam->addErr(-1);
  }
}

/// TypeDeclarations: { TypeDeclaration }
/// TypeDeclaration: ClassOrInterfaceDeclaration ;
std::vector<spTypeDeclaration> Parser::parseTypeDeclarations(
  std::vector<spAnnotation> &annotations) {

  std::vector<spTypeDeclaration> typeDecls = std::vector<spTypeDeclaration>();

  if (annotations.size() > 0) {
    spModifier modifier = spModifier(new Modifier);
    modifier->annotations = annotations;

    spTypeDeclaration typeDecl = spTypeDeclaration(new TypeDeclaration);
    typeDecl->classOrIntDecl = spClassOrInterfaceDeclaration(
      new ClassOrInterfaceDeclaration);
    typeDecl->classOrIntDecl->modifier = modifier;

    parseClassOrInterfaceDeclaration(typeDecl->classOrIntDecl);
    typeDecls.push_back(typeDecl);
  }

  while (isValidInitTokenOfTypeDeclaration(lexer->getCurToken())) {
    spTypeDeclaration typeDecl = spTypeDeclaration(new TypeDeclaration);
    typeDecl->classOrIntDecl = spClassOrInterfaceDeclaration(
      new ClassOrInterfaceDeclaration);
    parseClassOrInterfaceDeclaration(typeDecl->classOrIntDecl);
    typeDecls.push_back(typeDecl);
  }

  return typeDecls;
}

/// ClassOrInterfaceDeclaration:
///   {Modifier} (ClassDeclaration | InterfaceDeclaration)
void Parser::parseClassOrInterfaceDeclaration(
  spClassOrInterfaceDeclaration& decl) {

  st.addSym(ST_CLASS_OR_INTERFACE,
    lexer->getCurTokenIni(), 0, src->getLine(), "");

  // Modifier
  if (!decl->modifier) {
    decl->modifier = spModifier(new Modifier);
  }

  parseModifier(decl->modifier);

  // Class
  if (lexer->getCurToken() == TOK_KEY_CLASS) {
    decl->opt = ClassOrInterfaceDeclaration::OPT_CLASS;
    st.updateScopeType(ST_CLASS);
    decl->classDecl = spClassDeclaration(new ClassDeclaration);
    parseClassDeclaration(decl->classDecl);
    st.scopePop();
    return;
  }

  // Enum
  if (lexer->getCurToken() == TOK_KEY_ENUM) {
    decl->opt = ClassOrInterfaceDeclaration::OPT_CLASS;
    st.updateScopeType(ST_ENUM);
    decl->classDecl = spClassDeclaration(new ClassDeclaration);
    parseClassDeclaration(decl->classDecl);
    st.scopePop();
    return;
  }

  // Interface
  if (lexer->getCurToken() == TOK_KEY_INTERFACE
      || lexer->getCurToken() == TOK_ANNOTATION_TYPE_DECLARATION) {
    decl->opt = ClassOrInterfaceDeclaration::OPT_INTERFACE;
    st.updateScopeType(ST_INTERFACE);
    decl->interfaceDecl = spInterfaceDeclaration(new InterfaceDeclaration);
    parseInterfaceDeclaration(decl->interfaceDecl);
    st.scopePop();
    return;
  }

  st.scopePop();
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
  // NormalClassDeclaration
  if (lexer->getCurToken() == TOK_KEY_CLASS) {
    classDecl->nClassDecl = spNormalClassDeclaration(
      new NormalClassDeclaration);
    parseNormalClassDeclaration(classDecl->nClassDecl);
    return;
  }

  // EnumDeclaration
  if (lexer->getCurToken() == TOK_KEY_ENUM) {
    classDecl->enumDecl = spEnumDeclaration(new EnumDeclaration);
    parseEnumDeclaration(classDecl->enumDecl);
    return;
  }

  // Error
  classDecl->addErr(-1);
}

/// NormalClassDeclaration:
///   class Identifier [TypeParameters] [extends Type] [implements TypeList]
///     ClassBody
void Parser::parseNormalClassDeclaration(spNormalClassDeclaration &nClassDecl) {
  if (lexer->getCurToken() != TOK_KEY_CLASS) {
    nClassDecl->addErr(diag->addErr(ERR_EXP_CLASS, lexer->getCursor() - 1));
    return;
  }

  // 'class'
  nClassDecl->tokClass = spTokenExp(new TokenExp(lexer->getCursor()
    - tokenUtil.getTokenLength(TOK_KEY_CLASS), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'class'

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    nClassDecl->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  nClassDecl->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));

  st.addSym(ST_IDENTIFIER, lexer->getCurTokenIni(), lexer->getCursor(),
    src->getLine(), lexer->getCurTokenStr());

  lexer->getNextToken(); // consume Identifier

  // [TypeParameters]
  if (lexer->getCurToken() == TOK_OP_LT) {
    nClassDecl->typeParams = spTypeParameters(new TypeParameters);
    parseTypeParameters(nClassDecl->typeParams);
    if (nClassDecl->typeParams->err) {
      nClassDecl->addErr(-1);
      return;
    }
  }

  // [extends Type]
  if (lexer->getCurToken() == TOK_KEY_EXTENDS) {
    nClassDecl->tokExtends = spTokenExp(new TokenExp(lexer->getCursor()
      - tokenUtil.getTokenLength(TOK_KEY_EXTENDS), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'extends'

    // Type. We can only inherit from a ReferenceType with no array depth.
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      nClassDecl->addErr(diag->addErr(
        ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
      return;
    }

    nClassDecl->type = spType(new Type);
    nClassDecl->type->opt = Type::OPT_REFERENCE_TYPE;
    nClassDecl->type->refType = spReferenceType(new ReferenceType);
    parseReferenceType(nClassDecl->type->refType);
  }

  // [implements TypeList]
  if (lexer->getCurToken() == TOK_KEY_IMPLEMENTS) {
    nClassDecl->implementsTok = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(TOK_KEY_IMPLEMENTS),
      lexer->getCurToken()));
    lexer->getNextToken(); // consume 'implements'

    nClassDecl->typeList = spTypeList(new TypeList);
    parseTypeList(nClassDecl->typeList);
    if (nClassDecl->typeList->err) {
      nClassDecl->addErr(-1);
      return;
    }
  }

  nClassDecl->classBody = spClassBody(new ClassBody);
  parseClassBody(nClassDecl->classBody);
}

/// NormalInterfaceDeclaration:
///   interface Identifier [TypeParameters] [extends TypeList] InterfaceBody
void Parser::parseNormalInterfaceDeclaration(
  spNormalInterfaceDeclaration &normalDecl) {

  // interface
  normalDecl->tokInterface = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'interface'

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    normalDecl->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  normalDecl->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));

  st.addSym(ST_IDENTIFIER, lexer->getCurTokenIni(), lexer->getCursor(),
    src->getLine(), lexer->getCurTokenStr());

  lexer->getNextToken(); // consume Identifier

  // [TypeParameters]
  if (lexer->getCurToken() == TOK_OP_LT) {
    normalDecl->typeParams = spTypeParameters(new TypeParameters);
    parseTypeParameters(normalDecl->typeParams);
    if (normalDecl->typeParams->err) {
      normalDecl->addErr(-1);
      return;
    }
  }

  // [extends TypeList]
  if (lexer->getCurToken() == TOK_KEY_EXTENDS) {
    // extends
    normalDecl->tokExtends = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(TOK_KEY_EXTENDS),
      lexer->getCurToken()));
    lexer->getNextToken(); // consume 'extends'

    // TypeList
    normalDecl->typeList = spTypeList(new TypeList);
    parseTypeList(normalDecl->typeList);
    if (normalDecl->typeList->err) {
      normalDecl->addErr(-1);
      return;
    }
  }

  normalDecl->body = spInterfaceBody(new InterfaceBody);
  parseInterfaceBody(normalDecl->body);
  if (normalDecl->body->err) {
    normalDecl->addErr(-1);
  }
}

void Parser::parseNullLiteral(spTokenExp &nullLiteral) {
  nullLiteral->pos = lexer->getCurTokenIni();
  nullLiteral->type = TOK_NULL_LITERAL;
  lexer->getNextToken(); // consume 'null'
}

/// ParExpression: '(' Expression ')'
void Parser::parseParExpression(spParExpression &parExpr) {
  // '('
  if (lexer->getCurToken() != TOK_LPAREN) {
    parExpr->addErr(diag->addErr(ERR_EXP_LPAREN, lexer->getCursor() - 1));
    return;
  }

  parExpr->posLParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '('

  // Expression
  parExpr->expr = spExpression(new Expression);
  parseExpression(parExpr->expr);
  if (parExpr->expr->isEmpty()) {
    parExpr->addErr(-1);
    return;
  }

  // ')'
  if (lexer->getCurToken() != TOK_RPAREN) {
    parExpr->addErr(diag->addErr(ERR_EXP_RPAREN, lexer->getCursor() - 1));
    return;
  }

  parExpr->posRParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ')'
}

/// ClassBody:
///   '{' { ClassBodyDeclaration } '}'
void Parser::parseClassBody(spClassBody &classBody) {
  // '{'
  if (lexer->getCurToken() != TOK_LCURLY_BRACKET) {
    classBody->addErr(diag->addErr(
      ERR_EXP_LCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  classBody->posLCBrace = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '{'

  // { ClassBodyDeclaration }
  parseClassBodyDeclarationsHelper(classBody->classBodyDecls);

  // '}'
  if (lexer->getCurToken() != TOK_RCURLY_BRACKET) {
    classBody->addErr(diag->addErr(
      ERR_EXP_RCURLY_BRACKET, lexer->getCursor() - 1));
    return;
  }

  classBody->posRCBrace = lexer->getCursor() - 1;
  st.updateScopeEnd(lexer->getCursor());
  lexer->getNextToken(); // consume '}'
}

/// { ClassBodyDeclaration }
void Parser::parseClassBodyDeclarationsHelper(
  std::vector<spClassBodyDeclaration> &classBodyDecls) {

  unsigned pos = 0;
  while (isValidInitTokenOfClassBodyDeclaration(lexer->getCurToken())
    && pos != lexer->getCursor()) {

    pos = lexer->getCursor();
    if (lexer->getCurToken() == TOK_SEMICOLON) {
      lexer->getNextToken(); // consume ';'
      continue;
    }

    spClassBodyDeclaration decl = spClassBodyDeclaration(
      new ClassBodyDeclaration);
    parseClassBodyDeclaration(decl);
    classBodyDecls.push_back(decl);
  }
}

/// ClassBodyDeclaration:
///   (1) ;
///   (2) {Modifier} MemberDecl
///   (3) [static] Block
void Parser::parseClassBodyDeclaration(spClassBodyDeclaration &decl) {

  // The keyword 'static' is ambiguous. It can lead to option (2) or (3).
  bool isOption3 = false;
  if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
    isOption3 = true;
  } else if (lexer->getCurToken() == TOK_KEY_STATIC) {
    // We look ahead for a '{'
    State state;
    saveState(state);
    lexer->getNextToken(); // consume 'static'
    if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
      isOption3 = true;
    }
    restoreState(state);
  }

  // (3) [static] Block
  if (isOption3) {
    decl->opt = ClassBodyDeclaration::OPT_STATIC_BLOCK;
    // [static]
    if (lexer->getCurToken() == TOK_KEY_STATIC) {
      decl->tokStatic = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
          lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'static'
    }

    // Block
    if (lexer->getCurToken() != TOK_LCURLY_BRACKET) {
      decl->addErr(diag->addErr(
        ERR_EXP_LCURLY_BRACKET, lexer->getCursor() - 1));
      return;
    }

    decl->block = spBlock(new Block);
    parseBlock(decl->block);
    if (decl->block->err) {
      decl->addErr(-1);
    }

    return;
  }

  // (2) {Modifier} MemberDecl
  if (isModifierOrMemberMemberDeclCandidate(lexer->getCurToken())) {
    st.addSym(ST_MEMBER_DECL, lexer->getCurTokenIni(), 0, src->getLine(), "");
    decl->opt = ClassBodyDeclaration::OPT_MODIFIER_MEMBER_DECL;
    decl->modifier = spModifier(new Modifier);
    parseModifier(decl->modifier);
    decl->memberDecl = spMemberDecl(new MemberDecl);
    parseMemberDecl(decl->memberDecl);
    st.scopePop();
    return;
  }

  // (1) ;
  if (lexer->getCurToken() == TOK_SEMICOLON) {
    decl->opt = ClassBodyDeclaration::OPT_SEMICOLON;
    decl->posSemiColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ';'
    return;
  }

  // Error
  decl->addErr(-1);
}

/// ClassCreatorRest: Arguments [ClassBody]
void Parser::parseClassCreatorRest(spClassCreatorRest &classCreatorRest) {
  // Arguments
  spArguments args = spArguments(new Arguments);
  classCreatorRest->args = args;
  parseArguments(classCreatorRest->args);
  if (classCreatorRest->args->err) {
    classCreatorRest->addErr(-1);
  }

  // ClassBody
  if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
    spClassBody classBody = spClassBody(new ClassBody);
    classCreatorRest->classBody = classBody;
    parseClassBody(classCreatorRest->classBody);
  }
}

/// MemberDecl:
///   (1) MethodOrFieldDecl
///   (2) void Identifier VoidMethodDeclaratorRest
///   (3) Identifier ConstructorDeclaratorRest
///   (4) GenericMethodOrConstructorDecl
///   (5) ClassDeclaration
///   (6) InterfaceDeclaration
void Parser::parseMemberDecl(spMemberDecl &memberDecl) {
  // We have an Identifier but we have to discern between a TypeParameter and a
  // Constructor Identifier. For example, the identifier we found can represent
  // the return type of the following method:
  //     ReturnType method(...) {}
  //     ----------
  // Or, a member declaration like:
  //     class MyClass { MyClass m; }
  // Finally, our identifier can represent a constructor:
  //     class MyClass { MyClass() {...} }
  //                     ---------
  // We consult the symbol to check if the Identifier name is the same as the
  // class name in our current scope.
  bool isConstructor = false;
  if (lexer->getCurToken() == TOK_IDENTIFIER
    && st.isConstructor(lexer->getCurTokenStr())) {
    // We look ahead for a left parenthesis
    State state;
    saveState(state);
    lexer->getNextToken(); // consume identifier
    if (lexer->getCurToken() == TOK_LPAREN) {
      isConstructor = true;
    }
    restoreState(state);
  }

  if (isConstructor) {
    // (3) Identifier ConstructorDeclaratorRest
    memberDecl->opt = MemberDecl::OPT_IDENTIFIER_CONSTRUCTOR_DECLARATOR_REST;
    st.updateScopeType(ST_METHOD);

    // Identifier
    memberDecl->id = spIdentifier(new Identifier(lexer->getCurTokenIni(),
      lexer->getCurTokenStr()));
    st.addSym(ST_IDENTIFIER, lexer->getCurTokenIni(), lexer->getCursor(),
      src->getLine(), lexer->getCurTokenStr());
    lexer->getNextToken(); // consume Identifier

    // ConstructorDeclaratorRest
    memberDecl->constDeclRest = spConstructorDeclaratorRest(
      new ConstructorDeclaratorRest);
    parseConstructorDeclaratorRest(memberDecl->constDeclRest);
    return;
  }

  // (1) MethodOrFieldDecl
  if (lexer->getCurToken() == TOK_IDENTIFIER
    || isBasicType(lexer->getCurToken())) {
    memberDecl->opt = MemberDecl::OPT_METHOD_OR_FIELD_DECL;
    memberDecl->methodOrFieldDecl
      = spMethodOrFieldDecl(new MethodOrFieldDecl);
    parseMethodOrFieldDecl(memberDecl->methodOrFieldDecl);
    return;
  }

  // (2) void Identifier VoidMethodDeclaratorRest
  if (lexer->getCurToken() == TOK_KEY_VOID) {
    memberDecl->opt
      = MemberDecl::OPT_VOID_IDENTIFIER_VOID_METHOD_DECLARATOR_REST;
    st.updateScopeType(ST_METHOD);

    // void
    memberDecl->tokVoid = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'void'

    // Identifier
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      memberDecl->addErr(diag->addErr(ERR_EXP_IDENTIFIER,
        lexer->getCurTokenIni(), lexer->getCursor()));
      return;
    }

    memberDecl->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    st.addSym(ST_IDENTIFIER, lexer->getCurTokenIni(), lexer->getCursor(),
      src->getLine(), lexer->getCurTokenStr());
    lexer->getNextToken(); // consume Identifier

    // VoidMethodDeclaratorRest
    memberDecl->voidMethDeclRest = spVoidMethodDeclaratorRest(
      new VoidMethodDeclaratorRest);
    parseVoidMethodDeclaratorRest(memberDecl->voidMethDeclRest);
    if (memberDecl->voidMethDeclRest->err) {
      memberDecl->addErr(-1);
    }

    return;
  }

  // (4) GenericMethodOrConstructorDecl
  if (lexer->getCurToken() == TOK_OP_LT) {
    memberDecl->opt = MemberDecl::OPT_GENERIC_METHOD_OR_CONSTRUCTOR_DECL;
    memberDecl->genMethodOrConstDecl
      = spGenericMethodOrConstructorDecl(new GenericMethodOrConstructorDecl);
    parseGenericMethodOrConstructorDecl(memberDecl->genMethodOrConstDecl);
    if (memberDecl->genMethodOrConstDecl->err) {
      memberDecl->addErr(-1);
    }
    return;
  }

  // (5) ClassDeclaration
  if (lexer->getCurToken() == TOK_KEY_CLASS
    || lexer->getCurToken() == TOK_KEY_ENUM) {

    memberDecl->opt = MemberDecl::OPT_CLASS_DECLARATION;
    if (lexer->getCurToken() == TOK_KEY_CLASS) {
      st.updateScopeType(ST_CLASS);
    } else {
      st.updateScopeType(ST_ENUM);
    }
    memberDecl->classDecl = spClassDeclaration(new ClassDeclaration);
    parseClassDeclaration(memberDecl->classDecl);
    if (memberDecl->classDecl->err) {
      memberDecl->addErr(-1);
    }
    return;
  }

  // InterfaceDeclaration
  if (lexer->getCurToken() == TOK_KEY_INTERFACE
    || lexer->getCurToken() == TOK_ANNOTATION_TYPE_DECLARATION) {

    memberDecl->opt = MemberDecl::OPT_INTERFACE_DECLARATION;
    memberDecl->interfaceDecl = spInterfaceDeclaration(
      new InterfaceDeclaration);
    parseInterfaceDeclaration(memberDecl->interfaceDecl);
    if (memberDecl->interfaceDecl->err) {
      memberDecl->addErr(-1);
    }

    return;
  }

  memberDecl->addErr(-1);
}

/// MethodDeclaratorRest:
///  FormalParameters {'[' ']'} [throws QualifiedIdentifierList] (Block | ;)
void Parser::parseMethodDeclaratorRest(spMethodDeclaratorRest &methodDeclRest) {
  // FormalParameters
  methodDeclRest->formParams = spFormalParameters(new FormalParameters);
  parseFormalParameters(methodDeclRest->formParams);
  if (methodDeclRest->formParams->err) {
    methodDeclRest->addErr(-1);
    return;
  }

  // {'[' ']'}
  if (lexer->getCurToken() == TOK_LBRACKET) {
    parseArrayDepth(methodDeclRest->arrayDepth);
  }

  // [throws QualifiedIdentifierList]
  if (lexer->getCurToken() == TOK_KEY_THROWS) {
    // 'throws'
    methodDeclRest->tokThrows = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'throws'

    // QualifiedIdentifierList
    methodDeclRest->qualifiedIdList = spQualifiedIdentifierList(
      new QualifiedIdentifierList);
    parseQualifiedIdentifierList(methodDeclRest->qualifiedIdList);
    if (methodDeclRest->qualifiedIdList->err) {
      methodDeclRest->addErr(-1);
    }
  }

  // (Block | ;)
  // (1) ';'
  // This should be an abstract method.
  if (lexer->getCurToken() == TOK_SEMICOLON) {
    st.updateScopeEnd(lexer->getCursor());
    methodDeclRest->posSemiColon = lexer->getCursor() - 1;
    return;
  }

  // (2) Block
  methodDeclRest->block = spBlock(new Block);
  parseBlock(methodDeclRest->block);
  if (methodDeclRest->block->err) {
    methodDeclRest->addErr(-1);
  }

  st.updateScopeEnd(methodDeclRest->block->posRCBrace + 1);
}

/// MethodOrFieldDecl: Type Identifier MethodOrFieldRest
void Parser::parseMethodOrFieldDecl(spMethodOrFieldDecl &methodOrFieldDecl) {
  // Type
  methodOrFieldDecl->type = spType(new Type);
  parseType(methodOrFieldDecl->type);
  if (methodOrFieldDecl->type->err) {
    methodOrFieldDecl->addErr(-1);
    return;
  }

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    methodOrFieldDecl->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  methodOrFieldDecl->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  st.addSym(ST_IDENTIFIER, lexer->getCurTokenIni(), lexer->getCursor(),
    src->getLine(), lexer->getCurTokenStr());
  lexer->getNextToken(); // consume identifier

  // MethodOrFieldRest
  methodOrFieldDecl->methodOrFieldRest
    = spMethodOrFieldRest(new MethodOrFieldRest);
  parseMethodOrFieldRest(methodOrFieldDecl->methodOrFieldRest);
  if (methodOrFieldDecl->methodOrFieldRest->err) {
    methodOrFieldDecl->addErr(-1);
  }
}

/// MethodOrFieldRest:
///   (1) FieldDeclaratorsRest ;
///   (2) MethodDeclaratorRest
void Parser::parseMethodOrFieldRest(spMethodOrFieldRest &methodOrFieldRest) {
  // (2) MethodDeclaratorRest
  if (lexer->getCurToken() == TOK_LPAREN) {
    methodOrFieldRest->opt = MethodOrFieldRest::OPT_METHOD;
    st.updateScopeType(ST_METHOD);

    methodOrFieldRest->methodDeclRest = spMethodDeclaratorRest(
      new MethodDeclaratorRest);
    parseMethodDeclaratorRest(methodOrFieldRest->methodDeclRest);
    if (methodOrFieldRest->methodDeclRest->err) {
      methodOrFieldRest->addErr(-1);
    }
    return;
  }

  // (1) FieldDeclaratorsRest ;
  methodOrFieldRest->opt = MethodOrFieldRest::OPT_FIELD;
  st.updateScopeType(ST_FIELD);

  methodOrFieldRest->fieldDeclsRest
    = spFieldDeclaratorsRest(new FieldDeclaratorsRest());
  parseFieldDeclaratorsRest(methodOrFieldRest->fieldDeclsRest);
  if (methodOrFieldRest->fieldDeclsRest->err) {
    methodOrFieldRest->addErr(-1);
    return;
  }

  // ';'
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    methodOrFieldRest->addErr(diag->addErr(
      ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
  }

  methodOrFieldRest->posSemiColon = lexer->getCursor() - 1;
  st.updateScopeEnd(lexer->getCursor());
  lexer->getNextToken(); // consume ';'
}

/// NonWildcardTypeArguments: < TypeList2 >
void Parser::parseNonWildcardTypeArguments(
  spNonWildcardTypeArguments &nonWildcardTypeArguments) {

  // TOK_OP_LT
  if (lexer->getCurToken() != TOK_OP_LT) {
    nonWildcardTypeArguments->addErr(diag->addErr(
      ERR_EXP_OP_LT, lexer->getCursor() - 1));
    return;
  }

  nonWildcardTypeArguments->posLt = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '<'

  nonWildcardTypeArguments->typeList2 = spTypeList2(new TypeList2);
  parseTypeList2(nonWildcardTypeArguments->typeList2);
  if (nonWildcardTypeArguments->typeList2->err) {
    nonWildcardTypeArguments->addErr(-1);
    return;
  }

  // TOK_OP_GT
  if (lexer->getCurToken() != TOK_OP_GT) {
    nonWildcardTypeArguments->addErr(diag->addErr(
      ERR_EXP_OP_GT, lexer->getCursor() - 1));
    return;
  }

  nonWildcardTypeArguments->posGt = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '>'
}

/// NonWildcardTypeArgumentsOrDiamond:
///   < >
///   NonWildcardTypeArguments
void Parser::parseNonWildcardTypeArgumentsOrDiamond(
  spNonWildcardTypeArgumentsOrDiamond &nonWildcardOrDiam) {
  if (lexer->getCurToken() != TOK_OP_LT) {
    nonWildcardOrDiam->addErr(diag->addErr(
      ERR_EXP_OP_LT, lexer->getCursor() - 1));
    return;
  }

  int posLt = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '<'

  // opt1: < >
  if (lexer->getCurToken() == TOK_OP_GT) {
    nonWildcardOrDiam->opt = NonWildcardTypeArgumentsOrDiamond::OPT_DIAMOND;
    nonWildcardOrDiam->diamond.first = posLt;
    nonWildcardOrDiam->diamond.second = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '>'
    return;
  }

  // opt2: NonWildcardTypeArguments
  nonWildcardOrDiam->opt
    = NonWildcardTypeArgumentsOrDiamond::OPT_NON_WILDCARD_TYPE_ARGUMENTS;
  nonWildcardOrDiam->nonWildcardTypeArguments = spNonWildcardTypeArguments(
    new NonWildcardTypeArguments);
  parseNonWildcardTypeArguments(nonWildcardOrDiam->nonWildcardTypeArguments);
  if (nonWildcardOrDiam->nonWildcardTypeArguments->err) {
    nonWildcardOrDiam->addErr(-1);
  }
}

/// CompilationUnit: Top level parsing.
///   [PackageDeclaration] [ImportDeclaration] [TypeDeclarations]
void Parser::parseCompilationUnit() {
  std::vector<spAnnotation> annotations;
  if (lexer->getCurToken() == TOK_ANNOTATION) {
    parseAnnotations(annotations);
  }

  if (lexer->getCurToken() == TOK_KEY_PACKAGE) {
    compilationUnit->pkgDecl = spPackageDeclaration(new PackageDeclaration);
    parsePackageDeclaration(annotations, compilationUnit->pkgDecl);
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

/// ConstantDeclaratorRest:
///   {'[]'} = VariableInitializer
void Parser::parseConstantDeclaratorRest(
  spConstantDeclaratorRest &constDeclRest) {

  // {'[]'}
  parseArrayDepth(constDeclRest->arrayDepth);

  // '='
  if (lexer->getCurToken() != TOK_OP_EQUALS) {
    constDeclRest->addErr(diag->addErr(
      ERR_EXP_OP_EQUALS, lexer->getCursor() - 1));
    return;
  }

  constDeclRest->posEquals = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '='

  constDeclRest->varInit = spVariableInitializer(new VariableInitializer);
  parseVariableInitializer(constDeclRest->varInit);
  if (constDeclRest->varInit->err) {
    constDeclRest->addErr(-1);
  }
}

/// ConstantDeclaratorsRest:
///   ConstantDeclaratorRest { , ConstantDeclarator }
void Parser::parseConstantDeclaratorsRest(
  spConstantDeclaratorsRest &constDeclsRest) {

  constDeclsRest->constDeclRest = spConstantDeclaratorRest(
    new ConstantDeclaratorRest);
  parseConstantDeclaratorRest(constDeclsRest->constDeclRest);
  if (constDeclsRest->constDeclRest->err) {
    constDeclsRest->addErr(-1);
    return;
  }

  State state;
  while (lexer->getCurToken() == TOK_COMMA) {
    saveState(state);
    unsigned pos = lexer->getCursor() - 1;
    spConstantDeclaratorRest constDeclRest = spConstantDeclaratorRest(
      new ConstantDeclaratorRest);
    parseConstantDeclaratorRest(constDeclRest);
    // If we find an error we restore the state and exit leaving upper levels to
    // handle the error.
    if (constDeclRest->err) {
      restoreState(state);
      return;
    }

    constDeclsRest->pairs.push_back(std::make_pair(pos, constDeclRest));
  }
}

/// ConstructorDeclaratorRest:
///   FormalParameters [throws QualifiedIdentifierList] Block
void Parser::parseConstructorDeclaratorRest(
  spConstructorDeclaratorRest &constDeclRest) {

  // FormalParameters
  constDeclRest->formParams = spFormalParameters(new FormalParameters());
  parseFormalParameters(constDeclRest->formParams);

  // [throws QualifiedIdentifierList] Block
  if (lexer->getCurToken() == TOK_KEY_THROWS) {
    // 'throws'
    constDeclRest->tokThrows = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'throws'

    // QualifiedIdentifierList
    constDeclRest->qualifiedIdList = spQualifiedIdentifierList(
      new QualifiedIdentifierList);
    parseQualifiedIdentifierList(constDeclRest->qualifiedIdList);
    if (constDeclRest->qualifiedIdList->err) {
      constDeclRest->addErr(-1);
    }
  }

  // Block
  constDeclRest->block = spBlock(new Block);
  parseBlock(constDeclRest->block);
  if (constDeclRest->block->err) {
    constDeclRest->addErr(-1);
  }

  st.updateScopeEnd(constDeclRest->block->posRCBrace + 1);
}

/// CreatedName:
///   Identifier [TypeArgumentsOrDiamond]
///     { . Identifier [TypeArgumentsOrDiamond] }
void Parser::parseCreatedName(spCreatedName &createdName) {
  // Identifier [TypeArgumentsOrDiamond]
  parseCreatedNameHelper(createdName);
  if (createdName->err) {
    return;
  }

  // { . Identifier [TypeArgumentsOrDiamond] }
  State state;
  while (lexer->getCurToken() == TOK_PERIOD) {
    saveState(state);

    spCreatedNameTriplet triplet = spCreatedNameTriplet(new CreatedNameTriplet);

    // .
    triplet->posPeriod = lexer->getCursor();
    lexer->getNextToken(); // consume '.'

    // Identifier
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      restoreState(state);
      return;
    }

    triplet->id = spIdentifier(new Identifier(
        lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume identifier

    // TypeArgumentsOrDiamond
    if (lexer->getCurToken() != TOK_OP_LT) {
      continue;
    }

    triplet->typeArgsOrDiam = spTypeArgumentsOrDiamond(
      new TypeArgumentsOrDiamond);
    parseTypeArgumentsOrDiamond(createdName->typeArgsOrDiam);

    if (triplet->typeArgsOrDiam->err) {
      restoreState(state);
      return;
    }

    createdName->triplets.push_back(triplet);
  }
}

void Parser::parseCreatedNameHelper(spCreatedName &createdName) {
  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    createdName->addErr(diag->addErr(
      ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  createdName->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume identifier

  // TypeArgumentsOrDiamond
  if (lexer->getCurToken() != TOK_OP_LT) {
    return;
  }

  // GCC(4.6.3) has an odd behavior here. If we assign
  // createdName->typeArgsOrDiam =
  //   spTypeArgumentsOrDiamond(new TypeArgumentsOrDiamond());
  // The conditional 'if (createdName->typeArgsOrDiam->err)'
  // is always true, even when the value of err is false
  // (as seen in the debugger). The same problem does not occur in clang.
  spTypeArgumentsOrDiamond typeArgsOrDiam = spTypeArgumentsOrDiamond(
    new TypeArgumentsOrDiamond());
  createdName->typeArgsOrDiam = typeArgsOrDiam;
  parseTypeArgumentsOrDiamond(createdName->typeArgsOrDiam);

  if (createdName->typeArgsOrDiam->err) {
    createdName->addErr(-1);
  }
}

/// FormalParameters: '(' [FormalParameterDecls] ')'
void Parser::parseFormalParameters(spFormalParameters &formParams) {
  if (lexer->getCurToken() != TOK_LPAREN) {
    formParams->addErr(diag->addErr(
      ERR_EXP_LPAREN, lexer->getCurTokenIni(), lexer->getCursor()));
    return;
  }
  formParams->posLParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '('

  // If our current token is a closing paren we're done and we skip trying
  // to parse FormalParameterDecls.
  if (lexer->getCurToken() == TOK_RPAREN) {
    formParams->posRParen = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ')'
    return;
  }

  formParams->formParamDecls = spFormalParameterDecls(
    new FormalParameterDecls());
  parseFormalParameterDecls(formParams->formParamDecls);

  if (lexer->getCurToken() != TOK_RPAREN) {
    formParams->addErr(diag->addErr(
      ERR_EXP_RPAREN, lexer->getCurTokenIni(), lexer->getCursor()));
    return;
  }

  formParams->posRParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ')'
}

/// FormalParameterDecls: {VariableModifier} Type FormalParameterDeclsRest
void Parser::parseFormalParameterDecls(spFormalParameterDecls &formParamDecls) {
  // {VariableModifier}
  formParamDecls->varModifier = spVariableModifier(new VariableModifier);
  parseVariableModifier(formParamDecls->varModifier);

  // Type
  formParamDecls->type = spType(new Type());
  parseType(formParamDecls->type);

  // At this point if we have a VariableModifier and no Type we're in an
  // inconsistent state. For example:
  // Constructor(final var) or Constructor(@Annot var).
  if (formParamDecls->varModifier->isEmpty() == false
    && formParamDecls->type->err) {
    diag->addErr(ERR_EXP_TYPE, lexer->getCurTokenIni(), lexer->getCursor());
  }

  // If we have a Type we expect a FormalParameterDeclRest
  if (formParamDecls->type->err == false) {
    formParamDecls->formParamDeclsRest = spFormalParameterDeclsRest(
      new FormalParameterDeclsRest);
    parseFormalParameterDeclsRest(formParamDecls->formParamDeclsRest);
  }
}

/// VariableModifier:
///   final
///   Annotation
/// One 'final' keyword is allowed, while we can have zero or more annotations.
void Parser::parseVariableModifier(spVariableModifier &varModifier) {
  while (isVariableModifier(lexer->getCurToken())) {
    if (lexer->getCurToken() == TOK_KEY_FINAL) {
      if (varModifier->tokFinal) {
        // Error. We already have a 'final' token.
        varModifier->addErr(diag->addErr(ERR_VAR_MODIFIER_FINAL,
          lexer->getCurTokenIni(), lexer->getCursor()));
        return;
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

/// VoidInterfaceMethodDeclaratorRest:
///   FormalParameters [throws QualifiedIdentifierList] ;
void Parser::parseVoidInterfaceMethodDeclaratorRest(
  spVoidInterfaceMethodDeclaratorRest &voidMethDeclRest) {

  // FormalParameters
  voidMethDeclRest->formParams = spFormalParameters(new FormalParameters);
  parseFormalParameters(voidMethDeclRest->formParams);
  if (voidMethDeclRest->formParams->err) {
    voidMethDeclRest->addErr(-1);
    return;
  }

  // [throws QualifiedIdentifierList]
  if (lexer->getCurToken() == TOK_KEY_THROWS) {
    voidMethDeclRest->tokThrows = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'throws'

    voidMethDeclRest->qualifiedIdList = spQualifiedIdentifierList(
      new QualifiedIdentifierList);
    parseQualifiedIdentifierList(voidMethDeclRest->qualifiedIdList);
    if (voidMethDeclRest->qualifiedIdList->err) {
      voidMethDeclRest->addErr(-1);
      return;
    }
  }

  // ;
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    voidMethDeclRest->addErr(diag->addErr(
      ERR_EXP_SEMICOLON, lexer->getCursor() - 1));
    return;
  }

  voidMethDeclRest->posSemiColon = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ';'
}

/// VoidMethodDeclaratorRest:
///   FormalParameters [throws QualifiedIdentifierList] (Block | ;)
void Parser::parseVoidMethodDeclaratorRest(
  spVoidMethodDeclaratorRest &voidMethDeclRest) {

  // FormalParameters
  voidMethDeclRest->formParams = spFormalParameters(new FormalParameters);
  parseFormalParameters(voidMethDeclRest->formParams);
  if (voidMethDeclRest->formParams->err) {
    voidMethDeclRest->addErr(-1);
    return;
  }

  // [throws QualifiedIdentifierList]
  if (lexer->getCurToken() == TOK_KEY_THROWS) {
    // 'throws'
    voidMethDeclRest->tokThrows = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
        lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'throws'

    // QualifiedIdentifierList
    voidMethDeclRest->qualifiedIdList = spQualifiedIdentifierList(
      new QualifiedIdentifierList);
    parseQualifiedIdentifierList(voidMethDeclRest->qualifiedIdList);
    if (voidMethDeclRest->qualifiedIdList->err) {
      voidMethDeclRest->addErr(-1);
    }
  }

  // (Block | ;)
  // (1) ';'
  if (lexer->getCurToken() == TOK_SEMICOLON) {
    st.updateScopeEnd(lexer->getCursor());
    voidMethDeclRest->posSemiColon = lexer->getCursor() - 1;
    return;
  }

  // (2) Block
  voidMethDeclRest->block = spBlock(new Block);
  parseBlock(voidMethDeclRest->block);
  if (voidMethDeclRest->block->err) {
    voidMethDeclRest->addErr(-1);
  }

  st.updateScopeEnd(voidMethDeclRest->block->posRCBrace + 1);
}

/// SuperSuffix:
///   Arguments
///   . Identifier [Arguments]
void Parser::parseSuperSuffix(spSuperSuffix &superSuffix) {
  // 1st option - Arguments
  if (lexer->getCurToken() == TOK_LPAREN) {
    superSuffix->opt = SuperSuffix::OPT_ARGUMENTS;
    superSuffix->args = spArguments(new Arguments());
    parseArguments(superSuffix->args);
    return;
  }

  // 2nd - . Identifier [Arguments]
  if (lexer->getCurToken() == TOK_PERIOD) {
    superSuffix->opt = SuperSuffix::OPT_IDENTIFIER_ARGUMENTS;
    superSuffix->posPeriod = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '.'

    // Error
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      superSuffix->err = diag->addErr(
        ERR_EXP_IDENTIFIER, lexer->getCurTokenIni(), lexer->getCursor());
      return;
    }

    // Identifier
    superSuffix->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier

    if (lexer->getCurToken() == TOK_LPAREN) {
      superSuffix->args = spArguments(new Arguments);
      parseArguments(superSuffix->args);
    }
  }
}

/// SwitchBlockStatementGroups:
///   { SwitchBlockStatementGroup }
void Parser::parseSwitchBlockStatementGroups(
  spSwitchBlockStatementGroups &switchStmtGroups) {

  while (lexer->getCurToken() == TOK_KEY_CASE
    || lexer->getCurToken() == TOK_KEY_DEFAULT) {

    // We only consume valid nodes and we leave to upper levels dealing with
    // errors found here.
    State state;
    saveState(state);
    spSwitchBlockStatementGroup group = spSwitchBlockStatementGroup(
      new SwitchBlockStatementGroup);
    parseSwitchBlockStatementGroup(group);
    if (group->err) {
      restoreState(state);
      return;
    }

    switchStmtGroups->groups.push_back(group);
  }

}

/// SwitchBlockStatementGroup:
///   SwitchLabels BlockStatements
void Parser::parseSwitchBlockStatementGroup(
  spSwitchBlockStatementGroup &group) {

  // SwitchLabels
  group->labels = spSwitchLabels(new SwitchLabels);
  parseSwitchLabels(group->labels);
  if (group->labels->err) {
    group->addErr(-1);
  }

  // BlockStatements
  parseBlockStatements(group->blockStmts);
}

/// SwitchLabel:
///   (1) case Expression :
///   (2) case EnumConstantName :
///   (3) default :
void Parser::parseSwitchLabel(spSwitchLabel &label) {
  // (3) default :
  if (lexer->getCurToken() == TOK_KEY_DEFAULT) {
    label->opt = SwitchLabel::OPT_DEFAULT;

    // default
    label->tokDefault = spTokenExp(new TokenExp(
      lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'default'

    if (lexer->getCurToken() != TOK_OP_COLON) {
      label->addErr(diag->addErr(ERR_EXP_OP_COLON, lexer->getCursor() - 1));
      return;
    }

    label->posColon = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ':'
    return;
  }

  if (lexer->getCurToken() != TOK_KEY_CASE) {
    label->addErr(-1);
    return;
  }

  // case
  label->tokCase = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'case'

  // Try the production rule
  // (2) case EnumConstantName :
  State state;
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    saveState(state);
    spIdentifier id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier
    if (lexer->getCurToken() == TOK_OP_COLON) {
      label->opt = SwitchLabel::OPT_ENUM;
      label->enumConstName = spEnumConstantName(new EnumConstantName);
      label->enumConstName->id = id;
      label->posColon = lexer->getCursor() - 1;
      lexer->getNextToken(); // consume ':'
      return;
    }

    // We restore the state and try the first production rule
    restoreState(state);
  }

  // (1) case Expression :
  label->opt = SwitchLabel::OPT_EXPRESSION;
  label->expr = spExpression(new Expression);
  parseExpression(label->expr);
  if (label->expr->isEmpty()) {
    label->addErr(-1);
    return;
  }

  // ':'
  if (lexer->getCurToken() != TOK_OP_COLON) {
    label->addErr(diag->addErr(ERR_EXP_OP_COLON, lexer->getCursor() - 1));
    return;
  }

  label->posColon = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ':'
}

/// SwitchLabels:
///   SwitchLabel { SwitchLabel }
void Parser::parseSwitchLabels(spSwitchLabels &labels) {
  // SwtichLabel
  labels->label = spSwitchLabel(new SwitchLabel);
  parseSwitchLabel(labels->label);
  if (labels->label->err) {
    labels->addErr(-1);
    return;
  }

  // { SwitchLabel }
  while (lexer->getCurToken() == TOK_KEY_CASE
    || lexer->getCurToken() == TOK_KEY_DEFAULT) {
    State state;
    saveState(state);
    spSwitchLabel label = spSwitchLabel(new SwitchLabel);
    parseSwitchLabel(label);
    if (label->err) {
      restoreState(state);
      return;
    }
    labels->labels.push_back(label);
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
    parseArrayDepth(type->arrayDepth);
    return;
  }

  // ReferenceType
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    type->opt = Type::OPT_REFERENCE_TYPE;
    type->refType = spReferenceType(new ReferenceType);
    parseReferenceType(type->refType);
    if (type->refType->err) { type->addErr(-1); }
    parseArrayDepth(type->arrayDepth);
    return;
  }

  type->addErr(-1);
}

/// TypeList: ReferenceType { , ReferenceType }
void Parser::parseTypeList(spTypeList &typeList) {
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    typeList->addErr(diag->addErr(ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  typeList->refType = spReferenceType(new ReferenceType);
  parseReferenceType(typeList->refType);

  // { , ReferenceType }
  State state;
  while (lexer->getCurToken() == TOK_COMMA) {
    saveState(state);

    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','

    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      restoreState(state);
      return;
    }

    spReferenceType refType = spReferenceType(new ReferenceType);
    parseReferenceType(refType);
    if (refType->err) {
      restoreState(state);
      return;
    }

    typeList->pairs.push_back(std::make_pair(pos, refType));
  }
}

/// TypeList2: Type { , Type }
void Parser::parseTypeList2(spTypeList2 &typeList2) {

  typeList2->type = spType(new Type);
  parseType(typeList2->type);

  if (typeList2->type->err) {
    typeList2->addErr(diag->addErr(ERR_EXP_TYPE, lexer->getCursor() - 1));
    return;
  }

  State state;
  while (lexer->getCurToken() == TOK_COMMA) {
    saveState(state);
    unsigned int pos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','

    spType type = spType(new Type);
    parseType(type);
    if (type->err) {
      // we let the above layer to handle the error
      restoreState(state);
      return;
    }

    typeList2->pairs.push_back(std::make_pair(pos, type));
  }
}

/// TypeParameter:
///   Identifier [extends Bound]
void Parser::parseTypeParameter(spTypeParameter &typeParam) {
  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    typeParam->addErr(diag->addErr(ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  typeParam->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier

  if (lexer->getCurToken() != TOK_KEY_EXTENDS) {
    return;
  }

  // 'extends'
  typeParam->tokExtends = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'extends'

  // Bound
  typeParam->bound = spBound(new Bound);
  parseBound(typeParam->bound);
  if (typeParam->bound->err) {
    typeParam->addErr(-1);
  }
}

/// TypeParameters:
///   < TypeParameter { , TypeParameter } >
void Parser::parseTypeParameters(spTypeParameters &typeParams) {
  if (lexer->getCurToken() != TOK_OP_LT) {
    typeParams->addErr(diag->addErr(ERR_EXP_OP_LT, lexer->getCursor() - 1));
    return;
  }

  typeParams->posLt = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '<'

  typeParams->typeParam = spTypeParameter(new TypeParameter);
  parseTypeParameter(typeParams->typeParam);
  if (typeParams->typeParam->err) {
    typeParams->addErr(-1);
  }

  // { , TypeParameter }
  State state;
  while (lexer->getCurToken() == TOK_COMMA) {
    saveState(state);
    unsigned pos = lexer->getCursor() - 1;
    lexer->getNextToken();

    spTypeParameter typeParam = spTypeParameter(new TypeParameter);
    parseTypeParameter(typeParam);
    if (typeParam->err) {
      restoreState(state);
      typeParams->addErr(-1);
      return;
    }

    typeParams->pairs.push_back(std::make_pair(pos, typeParam));
  }

  if (lexer->getCurToken() != TOK_OP_GT) {
    typeParams->addErr(diag->addErr(ERR_EXP_OP_GT, lexer->getCursor() - 1));
    return;
  }

  typeParams->posGt = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '>'
}

void Parser::parseStringLiteral(spStringLiteral &strLit) {
  strLit->pos = lexer->getCurTokenIni();
  strLit->val = lexer->getCurTokenStr();
  lexer->getNextToken(); // consume string literal
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
      diag->addErr(
        ERR_EXP_IDENTIFIER, lexer->getCurTokenIni(), lexer->getCursor());
      return;
    }

    parseVariableDeclaratorId(formParamDeclsRest->varDeclId);

    // Corner case in the grammar. The array form is invalid in the form:
    // (int ... a[])
    if (formParamDeclsRest->varDeclId->arrayDepth.size() > 0) {
      diag->addErr(ERR_NVAL_ARRAY, lexer->getCurTokenIni(), lexer->getCursor());
    }

    return;
  }

  formParamDeclsRest->opt = FormalParameterDeclsRest::OPT_VAR_DECL_ID;

  // VariableDeclaratorId
  parseVariableDeclaratorId(formParamDeclsRest->varDeclId);

  // Handle error
  if (formParamDeclsRest->varDeclId->id->value.length() == 0) {
    diag->addErr(ERR_EXP_IDENTIFIER, lexer->getCurTokenIni(),
      lexer->getCursor());
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
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    varDeclId->addErr(diag->addErr(ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  varDeclId->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier
  parseArrayDepth(varDeclId->arrayDepth);
}

///  VariableDeclaratorRest: {'[' ']'} [ = VariableInitializer ]
void Parser::parseVariableDeclaratorRest(spVariableDeclaratorRest &varDeclRest) {
  // {'[' ']'}
  parseArrayDepth(varDeclRest->arrayDepth);

  // [ = VariableInitializer ]
  if (lexer->getCurToken() == TOK_OP_EQUALS) {
    varDeclRest->posEquals = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '='

    varDeclRest->varInit = spVariableInitializer(new VariableInitializer);
    parseVariableInitializer(varDeclRest->varInit);
    if (varDeclRest->varInit->err) {
      varDeclRest->addErr(-1);
    }
  }
}

/// VariableDeclarator: Identifier VariableDeclaratorRest
void Parser::parseVariableDeclarator(spVariableDeclarator &varDecl) {
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    varDecl->addErr(diag->addErr(ERR_EXP_IDENTIFIER, lexer->getCursor() - 1));
    return;
  }

  // Identifier
  varDecl->id = spIdentifier(
    new Identifier(lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier

  // VariableDeclaratorRest
  varDecl->varDeclRest = spVariableDeclaratorRest(new VariableDeclaratorRest);
  parseVariableDeclaratorRest(varDecl->varDeclRest);
  if (varDecl->varDeclRest->err) {
    varDecl->addErr(-1);
    return;
  }
}

/// VariableDeclarators: VariableDeclarator { , VariableDeclarator }
void Parser::parseVariableDeclarators(spVariableDeclarators &varDecls) {
  // VariableDeclarator
  varDecls->varDecl = spVariableDeclarator(new VariableDeclarator);
  parseVariableDeclarator(varDecls->varDecl);
  if (varDecls->varDecl->err) {
    varDecls->addErr(-1);
    return;
  }

  // { , VariableDeclarator }
  State state;
  while (lexer->getCurToken() == TOK_COMMA) {
    saveState(state);
    unsigned posComma = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ','

    spVariableDeclarator varDecl = spVariableDeclarator(new VariableDeclarator);
    parseVariableDeclarator(varDecl);
    if (varDecl->err) {
      restoreState(state);
      return;
    }

    std::pair<unsigned, spVariableDeclarator> pair;
    pair.first = posComma;
    pair.second = varDecl;

    varDecls->pairs.push_back(pair);
  }
}

/// VariableInitializer:
///   ArrayInitializer
///   Expression
void Parser::parseVariableInitializer(spVariableInitializer &varInit) {
  // ArrayInitializer
  if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
    varInit->opt = VariableInitializer::OPT_ARRAY_INITIALIZER;
    varInit->arrayInit = spArrayInitializer(new ArrayInitializer);
    parseArrayInitializer(varInit->arrayInit);
    if (varInit->arrayInit->err) { varInit->addErr(-1); }
    return;
  }

  // Expression
  varInit->opt = VariableInitializer::OPT_EXPRESSION;
  varInit->expr = spExpression(new Expression);
  parseExpression(varInit->expr);
  if (varInit->expr->isEmpty()) {
    varInit->addErr(-1);
  }
}

void Parser::parse() {
  buildParseTree();
  comments = lexer->getComments();
}

void Parser::buildParseTree() {
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
