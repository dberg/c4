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
    || isLiteral(token));
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

/// Annotation: @ QualifiedIdentifier [ ( [AnnotationElement] ) ]
spAnnotation Parser::parseAnnotation() {
  spAnnotation annotation = spAnnotation(new Annotation());
  annotation->posTokAt = lexer->getCursor() - 1;
  lexer->getNextToken(); // Consume '@'

  // QualifiedIdentifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    annotation->err = true;
    diag->addError(annotation->posTokAt, annotation->posTokAt + 1, ERR_EXP_QID);
    return annotation;
  }

  annotation->qualifiedId = parseQualifiedIdentifier();

  // If the current token is '(' we consume the token and expect
  // an optional AnnotaionElement followed by ')'
  if (lexer->getCurToken() == TOK_LPAREN) {
    int openParenPos = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '('

    // Empty annotation element
    if (lexer->getCurToken() != TOK_RPAREN) {
      annotation->elem = spAnnotationElement(new AnnotationElement());
      parseAnnotationElement(annotation->elem);
      if (annotation->elem->err) {
        annotation->err = true;
        diag->addError(annotation->posTokAt, openParenPos, ERR_NVAL_ANNOT_ELEM);
        return annotation;
      }
    }

    if (lexer->getCurToken() != TOK_RPAREN) {
      annotation->err = true;
      diag->addError(annotation->posTokAt, openParenPos, ERR_EXP_LPAREN);
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
  // TODO: handle errors.
  // TODO: assure that there's only one element value
  elem->opt = AnnotationElement::OPT_ELEMENT_VALUE;
  elem->value = spElementValue(new ElementValue());
  parseElementValue(elem->value);
}

/// Annotations.
void Parser::parseAnnotations(std::vector<spAnnotation> &annotations) {
  while (lexer->getCurToken() == TOK_ANNOTATION) {
    spAnnotation annotation = parseAnnotation();
    annotations.push_back(annotation);
  }
}

/// Arguments: '(' [ Expression { , Expression }] ')'
void Parser::parseArguments(spArguments &args) {

  if (lexer->getCurToken() != TOK_LPAREN) {
    args->addErr(diag->addError(
      lexer->getCursor() - 1, lexer->getCursor(), ERR_EXP_ARGUMENTS));
    return;
  }

  args->posLParen = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '('

  if (lexer->getCurToken() == TOK_RPAREN) {
    args->posRParen = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ')'
    return;
  }

  while (true) {
    spExpression expr = spExpression(new Expression());
    parseExpression(expr);
    if (expr->isEmpty()) {
      break;
    }
    args->exprs.push_back(expr);
  }

  if (lexer->getCurToken() == TOK_RPAREN) {
    args->posRParen = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ')'
    return;
  }

  // Error
  args->addErr(diag->addError(
    lexer->getCurTokenIni(),
    lexer->getCursor(),
    ERR_EXP_RCURLY_BRACKET));
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
  lexer->saveState(openBracketState);
  lexer->getNextToken(); // consume '['
  int lookahead = lexer->getCurToken();
  lexer->restoreState(openBracketState);

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
  arrayCreatorRest->opt2 = spArrayCreatorRestOpt2(new ArrayCreatorRestOpt2());
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
    opt1->addErr(diag->addError(
      lexer->getCursor() - 1, lexer->getCursor(), ERR_EXP_ARRAY));
    return;
  }

  if (lexer->getCurToken() == TOK_RCURLY_BRACKET) {
    opt1->addErr(diag->addError(
      lexer->getCursor() - 1, lexer->getCursor(), ERR_EXP_LCURLY_BRACKET));
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
  State openBracketState;

  // Expression in brackets
  while (true) {
    if (lexer->getCurToken() != TOK_LBRACKET) { return; }
    lexer->saveState(openBracketState);

    spExpressionInBrackets exprInBrackets
      = spExpressionInBrackets(new ExpressionInBrackets());
    exprInBrackets->posOpenBracket = lexer->getCurTokenIni();
    lexer->getNextToken(); // consume '['
    if (lexer->getCurToken() == TOK_RBRACKET) {
      // We have an empty array so we restore the state and break out.
      lexer->restoreState(openBracketState);
      break;
    }

    // Our only option is an expression in brackets.
    opt2->exprInBracketsList.push_back(exprInBrackets);

    spExpression expr = spExpression(new Expression());
    exprInBrackets->expr = expr;
    parseExpression(exprInBrackets->expr);

    // TODO: check for errors in the expression.

    if (lexer->getCurToken() != TOK_RBRACKET) {
      exprInBrackets->addErr(diag->addError(
        lexer->getCursor() - 1, lexer->getCursor(), ERR_EXP_RBRACKET));
      opt2->addErr(-1);
      return;
    }
  }

  // Check if we have at least one expression in brackets.
  if (opt2->exprInBracketsList.size() == 0) {
    opt2->addErr(diag->addError(lexer->getCursor() - 1, lexer->getCursor(),
      ERR_EXP_EXPRESSION_IN_BRACKETS));
    return;
  }

  // Array Depth
  if (lexer->getCurToken() == TOK_LBRACKET) {
    parseArrayDepth(opt2->arrayDepth);
  }
}

/// {[]}
void Parser::parseArrayDepth(ArrayDepth &arrayDepth) {
  while (lexer->getCurToken() == TOK_LBRACKET) {
    ArrayPair pair;
    pair.first = lexer->getCursor() - 1;

    lexer->getNextToken(); // consume '['

    if (lexer->getCurToken() != TOK_RBRACKET) {
      diag->addError(
        lexer->getCurTokenIni(), lexer->getCursor(), ERR_EXP_RBRACKET);
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
  // TODO:
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

void Parser::parseBooleanLiteral(spBooleanLiteral &boolLit) {
  boolLit->pos = lexer->getCurTokenIni();
  if (lexer->getCurTokenStr().compare("true") == 0) {
    boolLit->val = true;
  } else {
    boolLit->val = false;
  }
  lexer->getNextToken(); // consume 'true' or 'false'
}

void Parser::parseCharacterLiteral(spCharacterLiteral &charLit) {
  charLit->pos = lexer->getCurTokenIni();
  charLit->val = lexer->getCurTokenStr();
  lexer->getNextToken(); // consume character literal
}

/// Creator:
///   NonWildcardTypeArguments CreatedName ClassCreatorRest
///   CreatedName ( ClassCreatorRest | ArrayCreatorRest )
void Parser::parseCreator(spCreator &creator) {
  // Option 1
  if (lexer->getCurToken() == TOK_OP_LT) {
    creator->opt = Creator::OPT_NON_WILDCARD_TYPE_ARGUMENTS;
    creator->opt1 = spCreatorOpt1(new CreatorOpt1());
    parseCreatorOpt1(creator->opt1);
    return;
  }

  // Option 2
  creator->opt = Creator::OPT_CREATED_NAME;
  creator->opt2 = spCreatorOpt2(new CreatorOpt2());
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
  opt2->createdName = spCreatedName(new CreatedName());
  parseCreatedName(opt2->createdName);

  if (opt2->createdName->err) {
    opt2->addErr(-1);
    return;
  }

  // ( ClassCreatorRest | ArrayCreatorRest )
  // ClassCreatorRest
  if (lexer->getCurToken() == TOK_LPAREN) {
    opt2->classCreatorRest = spClassCreatorRest(new ClassCreatorRest());
    parseClassCreatorRest(opt2->classCreatorRest);
    return;
  }

  // ArrayCreatorRest
  if (lexer->getCurToken() == TOK_LBRACKET) {
    opt2->arrayCreatorRest = spArrayCreatorRest(new ArrayCreatorRest());
    parseArrayCreatorRest(opt2->arrayCreatorRest);
    return;
  }

  // Error
  opt2->addErr(diag->addError(lexer->getCursor() - 1, lexer->getCursor(),
    ERR_EXP_CLASS_OR_ARRAY_CREATOR_REST));
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

  // Primary { Selector } { PostfixOp }
  // TODO: One problem with isPrimary is the condition token == TOK_IDENTIFIER.
  // This condition might indicate the 7th Primary production rule:
  //   Identifier { . Identifier } [IdentifierSuffix]
  // TOK_IDENTIFIER  migh also indicate the 2nd Expression3 production rule:
  //   ( Expression | Type ) Expression3.
  // So, if we have a TOK_IDENTIFIER and Primary returns an error we should
  // backtrack and try the 2nd production rule of Expression3.
  if (isPrimary(lexer->getCurToken())) {
    spPrimary primary = spPrimary(new Primary());
    parsePrimary(primary);
    if (primary->isEmpty() == false) {
      expr3->opt = Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP;
      expr3->primary = primary;

      // { Selector }
      if (lexer->getCurToken() == TOK_PERIOD
        || lexer->getCurToken() == TOK_LBRACKET) {
        expr3->selector = spSelector(new Selector());
        parseSelector(expr3->selector);
        if (expr3->selector->err) {
          expr3->addErr(-1);
          return;
        }
      }

      // { PostfixOp }
      if (lexer->getCurToken() == TOK_OP_PLUS_PLUS
        || lexer->getCurToken() == TOK_OP_MINUS_MINUS) {
        expr3->postfixOp = spPostfixOp(new PostfixOp());
        parsePostfixOp(expr3->postfixOp);
      }
    }
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

  // TODO:
  // { , VariableDeclarator }
}

/// IdentifierSuffix:
///   '[' ( {'[' ']'} . class | Expression ) ']'
///   Arguments
///   . ( class | ExplicitGenericInvocation | this | super Arguments |
///       new [NonWildcardTypeArguments] InnerCreator )
void Parser::parseIdentifierSuffix(spIdentifierSuffix &idSuffix) {
  // opt1-2
  if (lexer->getCurToken() == TOK_LBRACKET) {
    idSuffix->arrayPair.first = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '['

    // opt1
    if (lexer->getCurToken() == TOK_RBRACKET
      || lexer->getCurToken() == TOK_COMMA) {
      idSuffix->opt = IdentifierSuffix::OPT_ARRAY_ARRAY_DEPTH_CLASS;
      // {'[' ']'} . class
      parseIdentifierSuffixOpt1Helper(idSuffix);
    } else {
      // opt2
      idSuffix->opt = IdentifierSuffix::OPT_ARRAY_EXPRESSION;
      idSuffix->expr = spExpression(new Expression());
      parseExpression(idSuffix->expr);
      // TODO: check if expression is valid
    }

    // Error: ']' expected
    if (lexer->getCurToken() != TOK_RBRACKET) {
      idSuffix->addErr(diag->addError(lexer->getCursor() - 1,
        lexer->getCursor(), ERR_EXP_RBRACKET));
      return;
    }

    idSuffix->arrayPair.second = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume ']'
    return;
  }

  // opt3: Arguments
  if (lexer->getCurToken() == TOK_LPAREN) {
    idSuffix->opt = IdentifierSuffix::OPT_ARGUMENTS;
    idSuffix->args = spArguments(new Arguments());
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

    // opt5
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

    // opt7:
    if (lexer->getCurToken() == TOK_KEY_SUPER) {
      idSuffix->opt = IdentifierSuffix::OPT_PERIOD_SUPER_ARGUMENTS;
      idSuffix->tokSuper = spTokenExp(new TokenExp(
        lexer->getCursor() - tokenUtil.getTokenLength(
          lexer->getCurToken()), lexer->getCurToken()));
      lexer->getNextToken(); // consume 'super'

      // Error: expected '('
      if (lexer->getCurToken() != TOK_LPAREN) {
        idSuffix->addErr(diag->addError(lexer->getCursor() - 1,
          lexer->getCursor(), ERR_EXP_LPAREN));
        return;
      }

      idSuffix->args = spArguments(new Arguments());
      parseArguments(idSuffix->args);
      if (idSuffix->args->err) { idSuffix->addErr(-1); }
      return;
    }

    // opt8:
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
      idSuffix->innerCreator = spInnerCreator(new InnerCreator());
      parseInnerCreator(idSuffix->innerCreator);

      // Error: invalid InnerCreator
      if (idSuffix->innerCreator->err) {
        idSuffix->addErr(-1);
        return;
      }
    }
  }

  // error
  idSuffix->addErr(diag->addError(lexer->getCursor() - 1, lexer->getCursor(),
    ERR_NVAL_IDENTIFIER_SUFFIX));
}

void Parser::parseIdentifierSuffixOpt1Helper(spIdentifierSuffix &idSuffix) {
  parseArrayDepth(idSuffix->arrayDepth);

  // Error: '.' expected
  if (lexer->getCurToken() != TOK_COMMA) {
    idSuffix->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_COMMA));
    return;
  }

  idSuffix->posPeriod = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '.'

  // Error: 'super' expected
  if (lexer->getCurToken() != TOK_KEY_SUPER) {
    idSuffix->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_SUPER));
    return;
  }

  // super
  idSuffix->tokSuper = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
    lexer->getCurToken()), lexer->getCurToken()));

  lexer->getNextToken(); // consume 'super'
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
    diag->addError(
      lexer->getCurTokenIni(), lexer->getCursor(), ERR_EXP_ELEMENT_VALUE);
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
  explGen->addErr(diag->addError(
        lexer->getCursor() - 1,
        lexer->getCursor(),
        ERR_NVAL_EXPLICIT_GENERIC_INVOCATION_SUFFIX));
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

/// PostfixOp: ++ | --
void Parser::parsePostfixOp(spPostfixOp &postfixOp) {
  if (lexer->getCurToken() == TOK_OP_MINUS_MINUS) {
    postfixOp->opt = PostfixOp::OPT_MINUS_MINUS;
    postfixOp->pos = lexer->getCursor() - 1;
    return;
  }

  if (lexer->getCurToken() == TOK_OP_PLUS_PLUS) {
    postfixOp->opt = PostfixOp::OPT_PLUS_PLUS;
    postfixOp->pos = lexer->getCursor() - 1;
    return;
  }
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

  return import;
}

/// InnerCreator:
///   Identifier [NonWildcardTypeArgumentsOrDiamond] ClassCreatorRest
void Parser::parseInnerCreator(spInnerCreator &innerCreator) {
  // Error: expected Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    innerCreator->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_IDENTIFIER));
    return;
  }

  // Identifier
  innerCreator->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume Identifier

  // NonWildcardTypeArgumentsOrDiamond
  innerCreator->nonWildcardOrDiam = spNonWildcardTypeArgumentsOrDiamond(
    new NonWildcardTypeArgumentsOrDiamond());
  parseNonWildcardTypeArgumentsOrDiamond(innerCreator->nonWildcardOrDiam);

  // Error: invalid NonWildcardTypeArgumentsOrDiamond
  if (innerCreator->nonWildcardOrDiam->err) {
    innerCreator->addErr(-1);
    return;
  }

  // ClassCreatorRest
  innerCreator->classCreatorRest = spClassCreatorRest(new ClassCreatorRest());
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

  // CharacterLiteral
  if (lexer->getCurToken() == TOK_CHARACTER_LITERAL) {
    literal->opt = Literal::OPT_CHAR;
    literal->charLiteral = spCharacterLiteral(new CharacterLiteral());
    parseCharacterLiteral(literal->charLiteral);
    return;
  }

  // StringLiteral
  if (lexer->getCurToken() == TOK_STRING_LITERAL) {
    literal->opt = Literal::OPT_STRING;
    literal->strLiteral = spStringLiteral(new StringLiteral());
    parseStringLiteral(literal->strLiteral);
    return;
  }

  if (lexer->getCurToken() == TOK_BOOLEAN_LITERAL) {
    literal->opt = Literal::OPT_BOOLEAN;
    literal->boolLiteral = spBooleanLiteral(new BooleanLiteral());
    parseBooleanLiteral(literal->boolLiteral);
    return;
  }

  if (lexer->getCurToken() == TOK_NULL_LITERAL) {
    literal->opt = Literal::OPT_NULL;
    literal->nullLiteral = spNullLiteral(new NullLiteral());
    parseNullLiteral(literal->nullLiteral);
  }
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
  // NonWildcardTypeArguments
  if (lexer->getCurToken() == TOK_OP_LT) {
    primary->opt = Primary::OPT_NON_WILDCARD_TYPE_ARGUMENTS;
    primary->nonWildcardTypeArguments = spPrimaryNonWildcardTypeArguments(
      new PrimaryNonWildcardTypeArguments());
    parsePrimaryNonWildcardTypeArguments(primary->nonWildcardTypeArguments);
    return;
  }

  // ParExpression
  if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
    primary->opt = Primary::OPT_PAR_EXPRESSION;
    primary->pairExpr = spPairExpression(new PairExpression());
    parsePairExpression(primary->pairExpr);
    return;
  }

  // this [Arguments]
  if (lexer->getCurToken() == TOK_KEY_THIS) {
    primary->opt = Primary::OPT_THIS_ARGUMENTS;
    primary->thisArgs = spPrimaryThisArguments(new PrimaryThisArguments());
    parsePrimaryThisArguments(primary->thisArgs);
    return;
  }

  // super SuperSuffix
  if (lexer->getCurToken() == TOK_KEY_SUPER) {
    primary->opt = Primary::OPT_SUPER_SUPER_SUFFIX;
    primary->superSuperSuffix = spPrimarySuperSuperSuffix(
      new PrimarySuperSuperSuffix());
    parsePrimarySuperSuperSuffix(primary->superSuperSuffix);
    return;
  }

  // new Creator
  if (lexer->getCurToken() == TOK_KEY_NEW) {
    primary->opt = Primary::OPT_NEW_CREATOR;
    primary->newCreator = spPrimaryNewCreator(new PrimaryNewCreator());
    parsePrimaryNewCreator(primary->newCreator);
    return;
  }

  // Identifier { . Identifier } [IdentifierSuffix]
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    primary->opt = Primary::OPT_IDENTIFIER;
    primary->primaryId = spPrimaryIdentifier(new PrimaryIdentifier());
    parsePrimaryIdentifier(primary->primaryId);
    return;
  }

  // BasicType {[]} . class
  if (isBasicType(lexer->getCurToken())) {
    primary->opt = Primary::OPT_BASIC_TYPE;
    primary->primaryBasicType = spPrimaryBasicType(new PrimaryBasicType());
    parsePrimaryBasicType(primary->primaryBasicType);
    return;
  }

  // void . class
  if (lexer->getCurToken() == TOK_KEY_VOID) {
    primary->opt = Primary::OPT_VOID_CLASS;
    primary->primaryVoidClass = spPrimaryVoidClass(new PrimaryVoidClass());
    parsePrimaryVoidClass(primary->primaryVoidClass);
    return;
  }

  // Literal
  spLiteral literal = spLiteral(new Literal());
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

  // {[]}
  parseArrayDepth(primaryBasicType->arrayDepth);

  // '.'
  if (lexer->getCurToken() != TOK_COMMA) {
    primaryBasicType->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_COMMA));
    return;
  }

  primaryBasicType->posComma = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '.'

  // 'class'
  if (lexer->getCurToken() != TOK_KEY_CLASS) {
    primaryBasicType->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_CLASS));
    return;
  }

  primaryBasicType->tokClass = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'class'
}

/// Primary: Identifier { . Identifier } [IdentifierSuffix]
void Parser::parsePrimaryIdentifier(spPrimaryIdentifier &primaryId) {
  State backup;
  while (lexer->getCurToken() == TOK_IDENTIFIER) {
    spIdentifier id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier

    primaryId->ids.push_back(id);

    if (lexer->getCurToken() != TOK_PERIOD) {
      break;
    }

    lexer->saveState(backup);
    lexer->getNextToken(); // consume '.'

    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      lexer->restoreState(backup);
      break;
    }
  }

  // [IdentifierSuffix]
  if (lexer->getCurToken() == TOK_LBRACKET
    || lexer->getCurToken() == TOK_LPAREN
    || lexer->getCurToken() == TOK_PERIOD) {

    primaryId->idSuffix = spIdentifierSuffix(new IdentifierSuffix());
    parseIdentifierSuffix(primaryId->idSuffix);
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
  if (lexer->getCurToken() == TOK_LPAREN)
    parseArguments(primaryThisArgs->args);
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
    primarySuperSuperSuffix->superSuffix = spSuperSuffix(new SuperSuffix());
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
  primaryNewCreator->creator = spCreator(new Creator());
  parseCreator(primaryNewCreator->creator);
}

/// Primary:
///   NonWildcardTypeArguments
///     ( ExplicitGenericInvocationSuffix | this Arguments )
void Parser::parsePrimaryNonWildcardTypeArguments(
  spPrimaryNonWildcardTypeArguments &primaryNonWildcard) {

  // NonWildcardTypeArguments
  primaryNonWildcard->nonWildcardTypeArguments = spNonWildcardTypeArguments(
    new NonWildcardTypeArguments());
  parseNonWildcardTypeArguments(primaryNonWildcard->nonWildcardTypeArguments);

  if (primaryNonWildcard->err) {
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
      primaryNonWildcard->addErr(diag->addError(
        lexer->getCursor() - 1, lexer->getCursor(), ERR_EXP_ARGUMENTS));
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
    primaryVoidClass->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_VOID));
    return;
  }

  // 'void'
  primaryVoidClass->tokVoid = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'void'

  // '.'
  if (lexer->getCurToken() != TOK_COMMA) {
    primaryVoidClass->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_COMMA));
    return;
  }

  primaryVoidClass->posComma = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '.'

  // 'class'
  if (lexer->getCurToken() != TOK_KEY_CLASS) {
    primaryVoidClass->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_CLASS));
    return;
  }

  primaryVoidClass->tokClass = spTokenExp(new TokenExp(
    lexer->getCursor() - tokenUtil.getTokenLength(
      lexer->getCurToken()), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'class'
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

/// ReferenceType:
///    Identifier [TypeArguments] { . Identifier [TypeArguments] }
void Parser::parseReferenceType(spReferenceType &refType) {
  // indentifier
  refType->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume identifier

  // [TypeArguments]
  if (lexer->getCurToken() == TOK_OP_LT) {
    refType->typeArgs = spTypeArguments(new TypeArguments());
    parseTypeArguments(refType->typeArgs);
  }

  // { . Identifier [TypeArguments] }
  // TODO:
}

/// Selector:
///   . Identifier [Arguments]
///   . ExplicitGenericInvocation
///   . this
///   . super SuperSuffix
///   . new [NonWildcardTypeArguments] InnerCreator
///   '[' Expression ']'
void Parser::parseSelector(spSelector &selector) {
  if (lexer->getCurToken() == TOK_COMMA) {
    selector->posComma = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '.'

    // . Identifier [Arguments]
    if (lexer->getCurToken() == TOK_IDENTIFIER) {
      selector->opt = Selector::OPT_IDENTIFIER_ARGUMENTS;
      selector->id = spIdentifier(new Identifier(
        lexer->getCurTokenIni(), lexer->getCurTokenStr()));
      lexer->getNextToken(); // consume Identifier

      // [Arguments]
      if (lexer->getCurToken() == TOK_LPAREN) {
        selector->args = spArguments(new Arguments());
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
    selector->arrayPair.first = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '['
    selector->expr = spExpression(new Expression());
    parseExpression(selector->expr);
    selector->arrayPair.second = lexer->getCursor() - 1;
    // TODO: check error
    lexer->getNextToken(); // consume ']'
    return;
  }

  selector->addErr(diag->addError(
    lexer->getCursor() - 1, lexer->getCursor(), ERR_NVAL_SELECTOR));
}

/// TypeArgument:
///   ReferenceType
///   ? [(extends|super) ReferenceType]
void Parser::parseTypeArgument(spTypeArgument &typeArg) {
  // option 1
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    typeArg->opt = TypeArgument::OPT_REFERENCE_TYPE;
    typeArg->refType = spReferenceType(new ReferenceType());
    parseReferenceType(typeArg->refType);
    return;
  }

  // option 2
  if (lexer->getCurToken() == TOK_OP_QUESTION_MARK) {
    typeArg->opt = TypeArgument::OPT_QUESTION_MARK;
    typeArg->opt2 = spTypeArgumentOpt2(new TypeArgumentOpt2());
    parseTypeArgumentOpt2(typeArg->opt2);
    return;
  }

  // error
  typeArg->addErr(diag->addError(
    lexer->getCursor() - 1, lexer->getCursor(), ERR_NVAL_TYPE_ARGUMENT));
}

/// TypeArgument: ? [(extends|super) ReferenceType]
void Parser::parseTypeArgumentOpt2(spTypeArgumentOpt2 &opt2) {
  opt2->posQuestionMark = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '?'

  // [(extends|super) Reference]
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
    opt2->addErr(diag->addError(
      lexer->getCursor() - 1, lexer->getCursor(), ERR_EXP_REFTYPE));
    return;
  }

  // ReferenceType
  opt2->refType = spReferenceType(new ReferenceType());
  parseReferenceType(opt2->refType);
}

/// TypeArguments: < TypeArgument { , TypeArgument } >
void Parser::parseTypeArguments(spTypeArguments &typeArgs) {
  typeArgs->posLt = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '<'

  spTypeArgument typeArg = spTypeArgument(new TypeArgument());
  typeArgs->typeArg = typeArg;
  parseTypeArgument(typeArgs->typeArg);
  if (typeArg->err) {
    typeArgs->addErr(-1);
    return;
  }

  // Additional TypeArgument list
  while (true) {
    if (lexer->getCurToken() != TOK_COMMA) {
      break;
    }

    lexer->getNextToken(); // consume ','

    spTypeArgument typeArgTmp = spTypeArgument(new TypeArgument());
    parseTypeArgument(typeArgTmp);
    typeArgs->typeArgs.push_back(typeArgTmp);

    if (typeArgTmp->err) {
      typeArgs->addErr(-1);
      return;
    }
  }

  if (lexer->getCurToken() == TOK_OP_GT) {
    typeArgs->posGt = lexer->getCursor() - 1;
    lexer->getNextToken();
    return;
  }

  // error
  typeArgs->addErr(diag->addError(
    lexer->getCursor() - 1, lexer->getCursor(), ERR_EXP_OP_GT));
}

/// TypeArgumentsOrDiamond:
///   < >
///   TypeArguments
void Parser::parseTypeArgumentsOrDiamond(
  spTypeArgumentsOrDiamond &typeArgsOrDiam) {

  int posLt = lexer->getCursor() - 1;
  if (lexer->getCurToken() != TOK_OP_LT) {
    typeArgsOrDiam->addErr(diag->addError(posLt,
      lexer->getCursor(), ERR_EXP_OP_LT));
    return;
  }

  // We need to look ahead for the next token.
  State ltState;
  lexer->saveState(ltState);
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
  lexer->restoreState(ltState);
  typeArgsOrDiam->opt = TypeArgumentsOrDiamond::OPT_TYPE_ARGUMENTS;
  // GCC(4.6.3). If we don't create typeArgs separately and then assign
  // it to typeArgsOrDiam things get weird with the last 'if' condition.
  // Clang is fine either way.
  spTypeArguments typeArgs = spTypeArguments(new TypeArguments());
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
  if (lexer->getCurToken() != TOK_KEY_CLASS) {
    nClassDecl->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_CLASS));
    return;
  }

  nClassDecl->classTok = spTokenExp(new TokenExp(lexer->getCursor()
    - tokenUtil.getTokenLength(TOK_KEY_CLASS), lexer->getCurToken()));
  lexer->getNextToken(); // consume 'class'

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    nClassDecl->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_IDENTIFIER));
    return;
  }

  int pos = lexer->getCurTokenIni();
  nClassDecl->identifier = spIdentifier(new Identifier(
    pos, lexer->getCurTokenStr()));
  st.addSym(ST_CLASS, lexer->getCurToken(), pos, src->getLine(),
    lexer->getCurTokenStr());
  lexer->getNextToken(); // consume Identifier

  // TODO: [TypeParameters]

  // [extends Type]
  if (lexer->getCurToken() == TOK_KEY_EXTENDS) {
    nClassDecl->extendsTok = spTokenExp(new TokenExp(lexer->getCursor()
      - tokenUtil.getTokenLength(TOK_KEY_EXTENDS), lexer->getCurToken()));
    lexer->getNextToken(); // consume 'extends'

    // Type. We can only inherit from a ReferenceType with no array depth.
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      nClassDecl->addErr(diag->addError(lexer->getCursor() - 1,
        lexer->getCursor(), ERR_EXP_IDENTIFIER));
      return;
    }

    nClassDecl->type = spType(new Type());
    nClassDecl->type->opt = Type::OPT_REFERENCE_TYPE;
    nClassDecl->type->refType = spReferenceType(new ReferenceType());
    parseReferenceType(nClassDecl->type->refType);
  }

  // TODO: [implements TypeList]

  nClassDecl->classBody = spClassBody(new ClassBody());
  parseClassBody(nClassDecl->classBody);
}

void Parser::parseNullLiteral(spTokenExp &nullLiteral) {
  nullLiteral->pos = lexer->getCurTokenIni();
  nullLiteral->type = TOK_NULL_LITERAL;
  lexer->getNextToken(); // consume 'null'
}

/// PairExpression: ( Expression )
void Parser::parsePairExpression(spPairExpression &pairExpr) {
  int pos = src->getCursor();
  lexer->getNextToken(); // consume '('

  pairExpr->expr = spExpression(new Expression());
  parseExpression(pairExpr->expr);

  if (lexer->getCurToken() == TOK_RCURLY_BRACKET) {
    lexer->getNextToken(); // consume ')'
    return;
  }

  // Error
  diag->addError(pos, lexer->getCursor(), ERR_EXP_LPAREN);
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
    decl->modifier = spModifier(new Modifier());
    parseModifier(decl->modifier);
    decl->memberDecl = spMemberDecl(new MemberDecl());
    parseMemberDecl(decl->memberDecl);
    return;
  }

  // TODO: [static] Block

  // TODO:
  lexer->getNextToken();
}

/// ClassCreatorRest: Arguments [ClassBody]
void Parser::parseClassCreatorRest(spClassCreatorRest &classCreatorRest) {
  // Arguments
  spArguments args = spArguments(new Arguments());
  classCreatorRest->args = args;
  parseArguments(classCreatorRest->args);
  if (classCreatorRest->args->err) {
    classCreatorRest->addErr(-1);
  }

  // ClassBody
  if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
    spClassBody classBody = spClassBody(new ClassBody());
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
  // Or, our identifier can represent the constructor of the following class:
  //     class MyClass { MyClass() {...} }
  //                     ---------
  // We consult the symbol to check if the Identifier name is the same as the
  // class name in our current scope.
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    // (3) Identifier ConstructorDeclaratorRest
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

  // (1) MethodOrFieldDecl
  if (lexer->getCurToken() == TOK_IDENTIFIER
    || isBasicType(lexer->getCurToken())) {
    memberDecl->opt = MemberDecl::OPT_METHOD_OR_FIELD_DECL;
    memberDecl->methodOrFieldDecl
      = spMethodOrFieldDecl(new MethodOrFieldDecl());
    parseMethodOrFieldDecl(memberDecl->methodOrFieldDecl);
    return;
  }

  // TODO: void Identifier VoidMethodDeclaratorRest
  // TODO: GenericMethodOrConstructorDecl
  // TODO: ClassDeclaration
  // TODO: InterfaceDeclaration

  // TODO:
  lexer->getNextToken();
}

/// MethodOrFieldDecl: Type Identifier MethodOrFieldRest
void Parser::parseMethodOrFieldDecl(spMethodOrFieldDecl &methodOrFieldDecl) {
  // Type
  methodOrFieldDecl->type = spType(new Type());
  parseType(methodOrFieldDecl->type);
  if (methodOrFieldDecl->type->err) {
    methodOrFieldDecl->addErr(-1);
    return;
  }

  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    methodOrFieldDecl->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_IDENTIFIER));
    return;
  }

  methodOrFieldDecl->id = spIdentifier(new Identifier(
    lexer->getCurTokenIni(), lexer->getCurTokenStr()));
  lexer->getNextToken(); // consume identifier

  // MethodOrFieldRest
  methodOrFieldDecl->methodOrFieldRest
    = spMethodOrFieldRest(new MethodOrFieldRest());
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
    // TODO:
    return;
  }

  // (1) FieldDeclaratorsRest ;
  methodOrFieldRest->opt = MethodOrFieldRest::OPT_FIELD;
  methodOrFieldRest->fieldDeclsRest
    = spFieldDeclaratorsRest(new FieldDeclaratorsRest());
  parseFieldDeclaratorsRest(methodOrFieldRest->fieldDeclsRest);
  if (methodOrFieldRest->fieldDeclsRest->err) {
    methodOrFieldRest->addErr(-1);
    return;
  }

  // ';'
  if (lexer->getCurToken() != TOK_SEMICOLON) {
    methodOrFieldRest->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_SEMICOLON));
  }

  methodOrFieldRest->posSemiColon = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume ';'
}

/// NonWildcardTypeArguments: < TypeList >
void Parser::parseNonWildcardTypeArguments(
  spNonWildcardTypeArguments &nonWildcardTypeArguments) {

  // TOK_OP_LT
  if (lexer->getCurToken() != TOK_OP_LT) {
    nonWildcardTypeArguments->addErr(diag->addError(
      lexer->getCursor() - 1, lexer->getCursor(), ERR_EXP_OP_LT));
    return;
  }

  nonWildcardTypeArguments->posLt = lexer->getCursor() - 1;
  lexer->getNextToken(); // consume '<'

  nonWildcardTypeArguments->typeList = spTypeList(new TypeList());
  parseTypeList(nonWildcardTypeArguments->typeList);
  if (nonWildcardTypeArguments->typeList->err) {
    nonWildcardTypeArguments->addErr(-1);
    return;
  }

  // TOK_OP_GT
  if (lexer->getCurToken() != TOK_OP_GT) {
    nonWildcardTypeArguments->addErr(diag->addError(
      lexer->getCursor() - 1, lexer->getCursor(), ERR_EXP_OP_GT));
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
    nonWildcardOrDiam->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_OP_LT));
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
  while (true) {
    // Comma
    if (lexer->getCurToken() != TOK_COMMA) { return; }
    lexer->getNextToken(); // consume '.'

    spCreatedName createdNameTmp = spCreatedName(new CreatedName());
    parseCreatedNameHelper(createdNameTmp);
    createdName->createdNames.push_back(createdNameTmp);

    if (createdNameTmp->err) {
      createdName->addErr(-1);
      return;
    }
  }
}

void Parser::parseCreatedNameHelper(spCreatedName &createdName) {
  // Identifier
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    createdName->addErr(diag->addError(lexer->getCursor() - 1,
      lexer->getCursor(), ERR_EXP_IDENTIFIER));
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

/// FormalParameters: ( [FormalParameterDecls] )
void Parser::parseFormalParameters(spFormalParameters &formParams) {
  if (lexer->getCurToken() != TOK_LPAREN) {
    diag->addError(lexer->getCurTokenIni(), lexer->getCursor(), ERR_EXP_LPAREN);
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
    diag->addError(lexer->getCurTokenIni(), lexer->getCursor(), ERR_EXP_RPAREN);
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
    diag->addError(lexer->getCurTokenIni(), lexer->getCursor(), ERR_EXP_TYPE);
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
    lexer->getNextToken(); // consume '.'

    // Error
    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      superSuffix->err = diag->addError(lexer->getCurTokenIni(),
        lexer->getCursor(), ERR_EXP_IDENTIFIER);
      return;
    }

    // Identifier
    superSuffix->id = spIdentifier(new Identifier(
      lexer->getCurTokenIni(), lexer->getCurTokenStr()));
    lexer->getNextToken(); // consume Identifier

    if (lexer->getCurToken() == TOK_RPAREN) {
      superSuffix->args = spArguments(new Arguments());
      parseArguments(superSuffix->args);
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
    parseArrayDepth(type->arrayDepth);
    return;
  }

  // ReferenceType
  if (lexer->getCurToken() == TOK_IDENTIFIER) {
    type->opt = Type::OPT_REFERENCE_TYPE;
    type->refType = spReferenceType(new ReferenceType());
    parseReferenceType(type->refType);
  }
}

/// TypeList: ReferenceType {, ReferenceType }
void Parser::parseTypeList(spTypeList &typeList) {
  if (lexer->getCurToken() != TOK_IDENTIFIER) {
    unsigned int cursor = lexer->getCursor();
    typeList->addErr(diag->addError(cursor - 1, cursor, ERR_EXP_IDENTIFIER));
    return;
  }

  typeList->refType = spReferenceType(new ReferenceType());
  parseReferenceType(typeList->refType);

  while (true) {
    if (lexer->getCurToken() != TOK_COMMA) {
      return;
    }

    lexer->getNextToken(); // consume ','

    if (lexer->getCurToken() != TOK_IDENTIFIER) {
      unsigned int cursor = lexer->getCursor();
      typeList->addErr(diag->addError(cursor - 1, cursor, ERR_EXP_IDENTIFIER));
      return;
    }

    spReferenceType refType = spReferenceType(new ReferenceType());
    parseReferenceType(refType);

    if (refType->err) {
      return;
    }

    typeList->refTypes.push_back(refType);
  }
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
      diag->addError(
        lexer->getCurTokenIni(), lexer->getCursor(), ERR_EXP_IDENTIFIER);
      return;
    }

    parseVariableDeclaratorId(formParamDeclsRest->varDeclId);

    // Corner case in the grammar. The array form is invalid in the form:
    // (int ... a[])
    if (formParamDeclsRest->varDeclId->arrayDepth.size() > 0) {
      diag->addError(
        lexer->getCurTokenIni(), lexer->getCursor(), ERR_NVAL_ARRAY);
    }

    return;
  }

  formParamDeclsRest->opt = FormalParameterDeclsRest::OPT_VAR_DECL_ID;

  // VariableDeclaratorId
  parseVariableDeclaratorId(formParamDeclsRest->varDeclId);

  // Handle error
  if (formParamDeclsRest->varDeclId->identifier->value.length() == 0) {
    diag->addError(
      lexer->getCurTokenIni(), lexer->getCursor(), ERR_EXP_IDENTIFIER);
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
    parseArrayDepth(varDeclId->arrayDepth);
  }
}

///  VariableDeclaratorRest {'[' ']'} [ = VariableInitializer ]
void Parser::parseVariableDeclaratorRest(spVariableDeclaratorRest &varDeclRest) {
  // {'[' ']'}
  parseArrayDepth(varDeclRest->arrayDepth);

  // [ = VariableInitializer ]
  if (lexer->getCurToken() == TOK_OP_EQUALS) {
    varDeclRest->posEquals = lexer->getCursor() - 1;
    lexer->getNextToken(); // consume '='

    varDeclRest->varInit = spVariableInitializer(new VariableInitializer());
    parseVariableInitializer(varDeclRest->varInit);
    if (varDeclRest->varInit->err) {
      varDeclRest->addErr(-1);
    }
  }
}

/// VariableInitializer:
///   ArrayInitializer
///   Expression
void Parser::parseVariableInitializer(spVariableInitializer &varInit) {
  // ArrayInitializer
  if (lexer->getCurToken() == TOK_LCURLY_BRACKET) {
    varInit->opt = VariableInitializer::OPT_ARRAY_INITIALIZER;
    varInit->arrayInit = spArrayInitializer(new ArrayInitializer());
    parseArrayInitializer(varInit->arrayInit);
    if (varInit->arrayInit->err) { varInit->addErr(-1); }
    return;
  }

  // Expression
  varInit->opt = VariableInitializer::OPT_EXPRESSION;
  varInit->expr = spExpression(new Expression());
  parseExpression(varInit->expr);
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
