#include "Output.h"

namespace djp {
void Output::build() {
  output = "(";
  if (compilationUnit->pkgDecl) {
    setPackageDeclaration(compilationUnit->pkgDecl);
  }

  if (compilationUnit->impDecls) {
    setImportDeclarations(compilationUnit->impDecls);
  }

  if (compilationUnit->typeDecls.size()) {
    setTypeDeclarations(compilationUnit->typeDecls);
  }

  setComments();

  if (diag->errors.size()) {
    setErrors(diag->errors);
  }

  output += ")";
}

void Output::setAnnotationElement(const spAnnotationElement elem) {
  if (elem->opt == AnnotationElement::OPT_ELEMENT_VALUE_PAIRS) {
    for (unsigned i = 0; i < elem->pairs.size(); i++) {
      setElementValuePair(elem->pairs[i]);
    }
    return;
  }

  if (elem->opt == AnnotationElement::OPT_ELEMENT_VALUE) {
    if (elem->value) {
      setElementValue(elem->value);
    }
  }
}

void Output::setAnnotation(const spAnnotation &annotation) {
  if (annotation->qualifiedId) {
    // '@'
    output += "(djp-node-annotation-tok-at "
      + itos(annotation->posTokAt + 1) + ")";

    // QualifiedId
    int ini = annotation->qualifiedId->ini + 1;
    int end = annotation->qualifiedId->end + 2;
    setQualifiedId(ini, end);
  }

  if (annotation->elem) {
    setAnnotationElement(annotation->elem);
  }
}

void Output::setAnnotations(
  const std::vector<spAnnotation> &annotations) {

  for (std::size_t i = 0; i < annotations.size(); i++) {
    setAnnotation(annotations[i]);
  }
}

void Output::setArguments(const spArguments &args) {
  if (args->posLParen) { setOp(args->posLParen); }
  if (args->posRParen) { setOp(args->posRParen); }
  if (args->expr) { setExpression(args->expr); }

  for (unsigned int i = 0; i < args->exprs.size(); i++) {
    setOp(args->exprs[i].first);
    setExpression(args->exprs[i].second);
  }
}

void Output::setArrayCreatorRest(
  const spArrayCreatorRest &arrayCreatorRest) {

  if (arrayCreatorRest->opt == ArrayCreatorRest::OPT_ARRAY_INITIALIZER) {
    if (!arrayCreatorRest->opt1) { return; }
    setArrayDepth(arrayCreatorRest->opt1->arrayDepth);
    if (arrayCreatorRest->opt1->arrayInitializer) {
      setArrayInitializer(arrayCreatorRest->opt1->arrayInitializer);
    }

    return;
  }

  if (arrayCreatorRest->opt == ArrayCreatorRest::OPT_EXPRESSION) {
    if (!arrayCreatorRest->opt2) { return; }

    for (unsigned i = 0;
         i < arrayCreatorRest->opt2->exprInBracketsList.size(); i++) {

      spExpressionInBrackets exprInBr
        = arrayCreatorRest->opt2->exprInBracketsList[i];

      if (exprInBr->posOpenBracket) {
        setOp(exprInBr->posOpenBracket);
      }

      if (exprInBr->posCloseBracket) {
        setOp(exprInBr->posCloseBracket);
      }

      if (exprInBr->expr) {
        setExpression(exprInBr->expr);
      }

    }
    setArrayDepth(arrayCreatorRest->opt2->arrayDepth);
  }
}

void Output::setArrayDepth(ArrayDepth &arrayDepth) {
  for (unsigned int i = 0; i < arrayDepth.size(); i++) {
    unsigned posOpen = arrayDepth[i].first;
    unsigned posClose = arrayDepth[i].second;
    setOp(posOpen);
    setOp(posClose);
  }
}

void Output::setArrayInitializer(const spArrayInitializer arrayInit) {
  if (arrayInit->posOpenCBrace) {
    setOp(arrayInit->posOpenCBrace);
  }

  if (arrayInit->posOpenCBrace) {
    setOp(arrayInit->posOpenCBrace);
  }

  for (unsigned i = 0; i < arrayInit->varInitList.size(); i++) {
    setVariableInitializer(arrayInit->varInitList[i]);
  }
}

void Output::setBlock(const spBlock &block) {
  if (block->posLCBracket) { setOp(block->posLCBracket); }

  for (unsigned int i = 0; i < block->blockStmts.size(); i++) {
    setBlockStatement(block->blockStmts[i]);
  }

  if (block->posRCBracket) { setOp(block->posRCBracket); }
}

void Output::setBlockStatement(const spBlockStatement &blockStmt) {
  if (blockStmt->opt == BlockStatement::OPT_LOCAL_VAR) {
    if (blockStmt->localVar) {
      setLocalVariableDeclarationStatement(blockStmt->localVar);
    }
    return;
  }

  if (blockStmt->opt == BlockStatement::OPT_CLASS_OR_INTERFACE_DECL) {
    // TODO:
    return;
  }

  if (blockStmt->opt == BlockStatement::OPT_ID_STMT) {
    if (blockStmt->id) {
      setIdentifier(blockStmt->id);
      if (blockStmt->posColon) {
        setOp(blockStmt->posColon);
      }
    }

    if (blockStmt->stmt) {
      setStatement(blockStmt->stmt);
    }
  }
}

void Output::setCatches(const spCatches &catches) {
  if (catches->catchClause) { setCatchClause(catches->catchClause); }
  for (unsigned int i = 0; i < catches->catchClauses.size(); i++) {
    setCatchClause(catches->catchClauses[0]);
  }
}

void Output::setCatchClause(const spCatchClause &catchClause) {
  if (catchClause->tokCatch) { setKeyword(catchClause->tokCatch); }
  if (catchClause->posLParen) { setOp(catchClause->posLParen); }
  // TODO: {VariableModifier}
  if (catchClause->catchType) { setCatchType(catchClause->catchType); }
  if (catchClause->id) { setIdentifier(catchClause->id); }
  if (catchClause->posRParen) { setOp(catchClause->posRParen); }
  if (catchClause->block) { setBlock(catchClause->block); }
}

void Output::setCatchType(const spCatchType &catchType) {
  if (catchType->id) {
    setIdentifier(catchType->id, Output::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  for (unsigned i = 0; i < catchType->pipeAndId.size(); i++) {
    std::pair<unsigned, spIdentifier> pair = catchType->pipeAndId[i];
    setOp(pair.first);
    setIdentifier(pair.second, Output::OPT_IDENTIFIER_REFERENCE_TYPE);
  }
}

void Output::setClassBody(const spClassBody &classBody) {
  for (std::size_t i = 0; i < classBody->decls.size(); i++) {
    setClassBodyDeclaration(classBody->decls[i]);
  }
}

/// We have 2 options export:
///   (djp-member-decl-modifier-member-decl ...) -> {Modifier} MemberDecl
///   (djp-member-decl-modifier-static-block ...) -> [static] Block
void Output::setClassBodyDeclaration(const spClassBodyDeclaration &decl) {
  if (decl->opt == ClassBodyDeclaration::OPT_MODIFIER_MEMBER_DECL) {
    output += "(djp-member-decl-modifier-member-decl ";
    if (decl->modifier) {
      setModifier(decl->modifier);
    }

    if (decl->memberDecl) {
      setMemberDecl(decl->memberDecl);
    }

    output += ")";
    return;
  }

  // TODO:
  if (decl->opt == ClassBodyDeclaration::OPT_STATIC_BLOCK) {
    //output += "(djp-member-decl-modifier-static-block ";
    return;
  }
}

void Output::setClassCreatorRest(const spClassCreatorRest &classCreatorRest) {
  if (classCreatorRest->args) {
    setArguments(classCreatorRest->args);
  }

  if (classCreatorRest->classBody) {
    setClassBody(classCreatorRest->classBody);
  }
}

void Output::setClassOrInterfaceDeclaration(
  const spClassOrInterfaceDeclaration &decl) {

  output += "(djp-class-or-interface-declaration ";

  if (decl->modifier) {
    setModifier(decl->modifier);
  }

  if (decl->classDecl) {
    if (decl->classDecl->nClassDecl) {
      setNormalClassDeclaration(decl->classDecl->nClassDecl);
    } else if (decl->classDecl->enumDecl) {
      // TODO:
      //setEnumDeclaration(decl->classDecl->enumDecl);
    }
  }

  output += ")";
}

void Output::setComments() {
  if (comments.size() == 0) { return; }

  output += "(djp-comments ";
  for (unsigned int i = 0; i < comments.size(); i++) {
    output += "(djp-comment " + itos(comments[i]->posIni + 1)
      + " " + itos(comments[i]->posEnd + 2) + ")";
  }

  output += ")";
}

void Output::setConstructorDeclaratorRest(
  const spConstructorDeclaratorRest &constDeclRest) {
  output += "(djp-constructor-declarator-rest ";

  if (constDeclRest->formParams && constDeclRest->formParams->formParamDecls) {
    setFormalParameterDecls(constDeclRest->formParams->formParamDecls);
  }

  if (constDeclRest->block) {
    setBlock(constDeclRest->block);
  }

  output += ")";
}

void Output::setCreatedName(const spCreatedName &createdName) {
  if (createdName->id) {
    setIdentifier(createdName->id, Output::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  if (createdName->typeArgsOrDiam) {
    setTypeArgumentsOrDiamond(createdName->typeArgsOrDiam);
  }

  for (unsigned i = 0; i < createdName->createdNames.size(); i++) {
    setCreatedName(createdName->createdNames[i]);
  }
}

void Output::setCreator(const spCreator &creator) {
  if (creator->opt == Creator::OPT_NON_WILDCARD_TYPE_ARGUMENTS) {
    if (creator->opt1->nonWildcardTypeArguments) {
      setNonWildcardTypeArguments(creator->opt1->nonWildcardTypeArguments);
    }

    if (creator->opt1->createdName) {
      setCreatedName(creator->opt1->createdName);
    }

    if (creator->opt1->classCreatorRest) {
      setClassCreatorRest(creator->opt1->classCreatorRest);
    }

    return;
  }

  if (creator->opt == Creator::OPT_CREATED_NAME) {
    if (!creator->opt2) { return; }

    if (creator->opt2->createdName) {
      setCreatedName(creator->opt2->createdName);
    }

    if (creator->opt2->classCreatorRest) {
      setClassCreatorRest(creator->opt2->classCreatorRest);
    }

    if (creator->opt2->arrayCreatorRest) {
      setArrayCreatorRest(creator->opt2->arrayCreatorRest);
    }
  }
}

void Output::setElementValue(const spElementValue &value) {
  if (value->opt == ElementValue::OPT_ANNOTATION) {
    if (value->annotation) {
      setAnnotation(value->annotation);
    }
    return;
  }

  if (value->opt == ElementValue::OPT_EXPRESSION1) {
    if (value->expr1) {
      setExpression1(value->expr1);
    }
    return;
  }

  if (value->opt == ElementValue::OPT_ELEMENT_VALUE_ARRAY_INITIALIZER) {
    if (value->elemValArrayInit) {
      // TODO:
      //setElementValueArrayInitializer(value->elemValArrayInit);
    }
  }
}

void Output::setElementValuePair(const spElementValuePair &pair) {
  if (pair->id) {
    setIdentifier(pair->id);
  }

  if (pair->value) {
    setElementValue(pair->value);
  }
}

void Output::setErrors(const std::vector<spError> &errors) {
  for (std::size_t i = 0; i < errors.size(); i++) {
    output += "(djp-error "
      + itos(errors[i]->ini + 1) + " "
      + itos(errors[i]->end + 1) + " \""
      + errUtil.getMessage(errors[i]->type) + "\")";

  }
}

void Output::setExpression(const spExpression &expr) {
  if (expr->expr1) {
    setExpression1(expr->expr1);
  }

  if (expr->assignOp && expr->assignOp->tok) {
    setOp(expr->assignOp->tok->pos,
      tokenUtil.getTokenLength(expr->assignOp->tok->type));

    if (expr->assignExpr1) {
      setExpression1(expr->assignExpr1);
    }
  }
}

void Output::setExpression1(const spExpression1 &expr1) {
  if (expr1->expr2) {
    setExpression2(expr1->expr2);
  }

  if (expr1->expr1Rest) {
    // TODO:
    //setExpression1Rest(expr1->expr1Rest);
  }
}

void Output::setExpression2(const spExpression2 &expr2) {
  if (expr2->expr3) {
    setExpression3(expr2->expr3);
  }

  if (expr2->expr2Rest) {
    setExpression2Rest(expr2->expr2Rest);
  }
}

void Output::setExpression2Rest(const spExpression2Rest &expr2Rest) {
  if (expr2Rest->opt == Expression2Rest::OPT_INFIXOP_EXPR3) {
    for (unsigned i = 0; i < expr2Rest->pairs.size(); i++) {
      spTokenExp op = expr2Rest->pairs[0].first;
      setOp(op->pos, tokenUtil.getTokenLength(op->type));
      setExpression3(expr2Rest->pairs[0].second);
    }
    return;
  }

  if (expr2Rest->opt == Expression2Rest::OPT_INSTANCEOF_TYPE) {
    if (expr2Rest->tokInstanceOf) {
      setKeyword(expr2Rest->tokInstanceOf);
    }

    if (expr2Rest->type) {
      setType(expr2Rest->type);
    }
    return;
  }
}

void Output::setExpression3(const spExpression3 &expr3) {
  if (expr3->opt == Expression3::OPT_PREFIXOP_EXPRESSION3) {
    if (expr3->prefixOp && expr3->prefixOp->pos) {
      setOp(expr3->prefixOp->pos,
        tokenUtil.getTokenLength(expr3->prefixOp->token));
    }

    if (expr3->expr3) {
      setExpression3(expr3->expr3);
    }

    return;
  }

  if (expr3->opt == Expression3::OPT_EXPRESSION_TYPE_EXPRESSION3) {
    if (expr3->expr) {
      setExpression(expr3->expr);
    }

    if (expr3->type) {
      setType(expr3->type);
    }

    if (expr3->expr3) {
      setExpression3(expr3->expr3);
    }

    return;
  }

  if (expr3->opt == Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP) {
    if (expr3->primary) {
      setPrimary(expr3->primary);
    }

    for (unsigned i = 0; i < expr3->selectors.size(); i++) {
      setSelector(expr3->selectors[i]);
    }

    for (unsigned i = 0; i < expr3->postfixOps.size(); i++) {
      setOp(expr3->postfixOps[i]->pos, 2);
    }
  }
}

void Output::setFieldDeclsRest(const spFieldDeclaratorsRest &fieldDeclsRest) {
  if (fieldDeclsRest->varDeclRest) {
    setVariableDeclaratorRest(fieldDeclsRest->varDeclRest);
  }

  // TODO:
  //std::vector<std::pair<unsigned int, spVariableDeclarator> > pairsCommaVarDecl;
}

void Output::setFinally(const spFinally &finally) {
  if (finally->tokFinally) { setKeyword(finally->tokFinally); }
  if (finally->block) { setBlock(finally->block); }
}

void Output::setFormalParameters(const spFormalParameters &formParams) {
  if (formParams->posLParen) {
    setOp(formParams->posLParen);
  }

  if (formParams->posRParen) {
    setOp(formParams->posRParen);
  }

  if (formParams->formParamDecls) {
    setFormalParameterDecls(formParams->formParamDecls);
  }
}

void Output::setFormalParameterDecls(
  const spFormalParameterDecls &formParamDecls) {
  if (formParamDecls->varModifier) {
    setVariableModifier(formParamDecls->varModifier);
  }

  if (formParamDecls->type) {
    setType(formParamDecls->type);
  }

  if (formParamDecls->formParamDeclsRest) {
    setFormalParameterDeclsRest(formParamDecls->formParamDeclsRest);
  }
}

void Output::setFormalParameterDeclsRest(
  const spFormalParameterDeclsRest &formParamDeclsRest) {

  if (formParamDeclsRest->opt ==
    FormalParameterDeclsRest::OPT_VAR_DECL_ID) {
    setVariableDeclaratorId(formParamDeclsRest->varDeclId);
    if (formParamDeclsRest->formParamDecls) {
      setFormalParameterDecls(formParamDeclsRest->formParamDecls);
    }
    return;
  }

  if (formParamDeclsRest->opt ==
      FormalParameterDeclsRest::OPT_VAR_ARITY) {
    setVariableDeclaratorId(formParamDeclsRest->varDeclId);
  }
}

void Output::setIdentifier(const spIdentifier &identifier, IdentifierOpt opt) {
  int ini = identifier->pos + 1;
  int end = ini + identifier->value.length();

  if (opt == Output::OPT_IDENTIFIER_REFERENCE_TYPE) {
    output += "(djp-node-reference-type-id "
      + itos(ini) + " " + itos(end) + ")";
    return;
  }

  output += "(djp-node-identifier " + itos(ini) + " " + itos(end) + ")";
}

void Output::setIdentifierSuffix(const spIdentifierSuffix &idSuffix) {
  if (idSuffix->opt == IdentifierSuffix::OPT_ARRAY_ARRAY_DEPTH_CLASS) {
    // TODO:
    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_ARRAY_EXPRESSION) {
    // TODO:
    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_ARGUMENTS) {
    if (idSuffix->args) {
      setArguments(idSuffix->args);
    }
    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_PERIOD_CLASS) {
    if (idSuffix->posPeriod) { setOp(idSuffix->posPeriod); }
    if (idSuffix->tokClass) { setKeyword(idSuffix->tokClass); }
    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_PERIOD_EXPLICIT_GENERIC_INVOCATION) {
    // TODO:
    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_PERIOD_THIS) {
    // TODO:
    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_PERIOD_SUPER_ARGUMENTS) {
    // TODO:
    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_NEW) {
    // TODO:
    return;
  }
}

void Output::setImportDeclaration(const spImportDeclaration &import) {
  output += "(djp-import-declaration ";
  setKeyword(import->posTokImport + 1,
    import->posTokImport + tokenUtil.getTokenLength(TOK_KEY_IMPORT) + 1);

  if (import->posTokStatic > 0) {
    setKeyword(import->posTokStatic + 1,
      import->posTokStatic + tokenUtil.getTokenLength(TOK_KEY_STATIC) + 1);
    }

  if (import->qualifiedId) {
    int ini = import->qualifiedId->ini + 1;
    int end = import->qualifiedId->end + 2;
    if (import->iniOnDemand > 0) {
      end = import->endOnDemand + 2;
    }
    setQualifiedId(ini, end);
  }

  output += ")";
}

void Output::setImportDeclarations(const spImportDeclarations &impDecls) {
  output += "(djp-import-declarations ";
  for (std::string::size_type i = 0; i < impDecls->imports.size(); i++) {
    setImportDeclaration(impDecls->imports[i]);
  }
  output += ")";
}

void Output::setKeyword(const spTokenExp &token) {
  setKeyword(token->pos + 1, token->pos + 1
    + tokenUtil.getTokenLength(token->type));
}

void Output::setKeyword(int ini, int end) {
  output += "(djp-node-keyword " + itos(ini) + " " + itos(end) + ")";
}

void Output::setLiteral(const spLiteral &literal) {
  if (literal->opt == Literal::OPT_INTEGER) {
    // TODO:
    return;
  }

  if (literal->opt == Literal::OPT_FLOATING_POINT) {
    // TODO:
    return;
  }

  if (literal->opt == Literal::OPT_CHAR) {
    // TODO:
    return;
  }

  if (literal->opt == Literal::OPT_STRING) {
    if (literal->strLiteral) {
      setStringLiteral(literal->strLiteral);
    }
    return;
  }

  if (literal->opt == Literal::OPT_BOOLEAN) {
    // TODO:
    return;
  }

  if (literal->opt == Literal::OPT_NULL) {
    if (literal->nullLiteral) {
      setKeyword(literal->nullLiteral);
    }
    return;
  }
}

void Output::setLocalVariableDeclarationStatement(
  const spLocalVariableDeclarationStatement &localVar) {

  if (localVar->varModifier) {
    setVariableModifier(localVar->varModifier);
  }

  if (localVar->type) {
    setType(localVar->type);
  }

  if (localVar->varDecls) {
    setVariableDeclarators(localVar->varDecls);
  }

  if (localVar->posSemiColon) {
    setOp(localVar->posSemiColon);
  }
}


void Output::setMemberDecl(const spMemberDecl &memberDecl) {
  if (memberDecl->opt == MemberDecl::OPT_METHOD_OR_FIELD_DECL) {
    if (memberDecl->methodOrFieldDecl) {
      setMethodOrFieldDecl(memberDecl->methodOrFieldDecl);
    }
    return;
  }

  if (memberDecl->opt ==
    MemberDecl::OPT_VOID_IDENTIFIER_VOID_METHOD_DECLARATOR_REST) {
    if (memberDecl->tokVoid) {
      setKeyword(memberDecl->tokVoid);
    }

    if (memberDecl->id) {
      setIdentifier(memberDecl->id);
    }

    if (memberDecl->voidMethDeclRest) {
      setVoidMethodDeclaratorRest(memberDecl->voidMethDeclRest);
    }

    return;
  }

  if (memberDecl->opt ==
    MemberDecl::OPT_IDENTIFIER_CONSTRUCTOR_DECLARATOR_REST) {
    if (memberDecl->id) {
      setIdentifier(memberDecl->id);
    }

    if (memberDecl->constDeclRest) {
      setConstructorDeclaratorRest(memberDecl->constDeclRest);
    }
    return;
  }

  // TODO:
  if (memberDecl->opt == MemberDecl::OPT_GENERIC_METHOD_OR_CONSTRUCTOR_DECL) {
    return;
  }

  // TODO:
  if (memberDecl->opt == MemberDecl::OPT_CLASS_DECLARATION) {
    return;
  }

  // TODO:
  if (memberDecl->opt == MemberDecl::OPT_INTERFACE_DECLARATION) {
    return;
  }
}

void Output::setMethodDeclaratorRest(
  const spMethodDeclaratorRest &methodDeclRest) {

  if (methodDeclRest->formParams) {
    setFormalParameters(methodDeclRest->formParams);
  }

  setArrayDepth(methodDeclRest->arrayDepth);

  if (methodDeclRest->tokThrows) {
    setKeyword(methodDeclRest->tokThrows);
  }

  // TODO:
  //spQualifiedIdentifierList qualifiedIdList;

  if (methodDeclRest->block) {
    setBlock(methodDeclRest->block);
  }

  if (methodDeclRest->posSemiColon) {
    setOp(methodDeclRest->posSemiColon);
  }
}

void Output::setMethodOrFieldDecl(
  const spMethodOrFieldDecl &methodOrFieldDecl) {

  if (methodOrFieldDecl->type) {
    setType(methodOrFieldDecl->type);
  }

  if (methodOrFieldDecl->id) {
    setIdentifier(methodOrFieldDecl->id);
  }

  if (methodOrFieldDecl->methodOrFieldRest) {
    setMethodOrFieldRest(methodOrFieldDecl->methodOrFieldRest);
  }
}

void Output::setMethodOrFieldRest(
  const spMethodOrFieldRest &methodOrFieldRest) {

  if (methodOrFieldRest->opt == MethodOrFieldRest::OPT_FIELD) {
    if (methodOrFieldRest->fieldDeclsRest) {
      setFieldDeclsRest(methodOrFieldRest->fieldDeclsRest);
    }

    if (methodOrFieldRest->posSemiColon) {
      setOp(methodOrFieldRest->posSemiColon);
    }

    return;
  }

  if (methodOrFieldRest->opt == MethodOrFieldRest::OPT_METHOD) {
    if (methodOrFieldRest->methodDeclRest) {
      setMethodDeclaratorRest(methodOrFieldRest->methodDeclRest);
    }
  }
}

void Output::setModifier(const spModifier &modifier) {
  if (modifier->annotations.size()) {
    setAnnotations(modifier->annotations);
  }

  for (std::size_t i = 0; i < modifier->tokens.size(); i++) {
    setKeyword(modifier->tokens[i]);
  }
}

void Output::setNonWildcardTypeArguments(
  const spNonWildcardTypeArguments &nonWildcardTypeArguments) {
  // TODO:
}


void Output::setNormalClassDeclaration(
  const spNormalClassDeclaration &nClassDecl) {

  output += "(djp-normal-class-declaration ";

  if (nClassDecl->classTok) {
    setKeyword(nClassDecl->classTok);
  }

  if (nClassDecl->identifier) {
    setIdentifier(nClassDecl->identifier,
      Output::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  if (nClassDecl->classBody) {
    setClassBody(nClassDecl->classBody);
  }

  if (nClassDecl->extendsTok) {
    setKeyword(nClassDecl->extendsTok);
    if (nClassDecl->type) {
      setType(nClassDecl->type);
    }
  }

  // TODO: [TypeParameters]
  // TODO: [implements TypeList]

  output += ")";
}

void Output::setOp(unsigned int ini, int len) {
  ini++;
  unsigned int end = ini + len;
  output += "(djp-node-op " + itos(ini) + " " + itos(end) + ")";
}

void Output::setPackageDeclaration(const spPackageDeclaration &pkgDecl) {
  output += "(djp-package-declaration ";
  setAnnotations(pkgDecl->annotations);

  // package keyword
  setKeyword(pkgDecl->pkgTokPos + 1,
    pkgDecl->pkgTokPos + tokenUtil.getTokenLength(TOK_KEY_PACKAGE) + 1);

  // package qualified identifier
  if (pkgDecl->qualifiedId) {
    setQualifiedId(pkgDecl->qualifiedId->ini + 1,
      pkgDecl->qualifiedId->end + 2);
  }

  output += ")";
}

void Output::setParExpression(const spParExpression &parExpr) {
  if (parExpr->posLParen) { setOp(parExpr->posLParen); }
  if (parExpr->expr) { setExpression(parExpr->expr); }
  if (parExpr->posRParen) { setOp(parExpr->posRParen); }
}

void Output::setPrimary(const spPrimary &primary) {
  if (primary->opt == Primary::OPT_LITERAL) {
    if (primary->literal) {
      setLiteral(primary->literal);
    }
    return;
  }

  if (primary->opt == Primary::OPT_PAR_EXPRESSION) {
    // TODO:
    return;
  }

  if (primary->opt == Primary::OPT_THIS_ARGUMENTS) {
    // TODO:
    return;
  }

  if (primary->opt == Primary::OPT_SUPER_SUPER_SUFFIX) {
    // TODO:
    return;
  }

  if (primary->opt == Primary::OPT_NEW_CREATOR) {
    if (primary->newCreator) {
      if (primary->newCreator->tokNew) {
        setKeyword(primary->newCreator->tokNew);
      }

      if (primary->newCreator->creator) {
        setCreator(primary->newCreator->creator);
      }
    }

    return;
  }

  if (primary->opt == Primary::OPT_NON_WILDCARD_TYPE_ARGUMENTS) {
    // TODO:
    return;
  }

  if (primary->opt == Primary::OPT_IDENTIFIER) {
    if (primary->primaryId) {
      setPrimaryIdentifier(primary->primaryId);
    }
    return;
  }

  if (primary->opt == Primary::OPT_BASIC_TYPE) {
    // TODO:
    return;
  }

  if (primary->opt == Primary::OPT_VOID_CLASS) {
    // TODO:
    return;
  }
}

void Output::setPrimaryIdentifier(const spPrimaryIdentifier &primaryId) {
  for (unsigned int i = 0; i < primaryId->ids.size(); i++) {
    setIdentifier(primaryId->ids[i]);
  }

  if (primaryId->idSuffix) {
    setIdentifierSuffix(primaryId->idSuffix);
  }
}

void Output::setQualifiedId(int ini, int end) {
  output += "(djp-node-qualified-id "
    + itos(ini) + " " + itos(end) + ")";
}

void Output::setReferenceType(const spReferenceType &refType) {
  if (refType->id) {
    setIdentifier(refType->id, Output::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  if (refType->typeArgs) { setTypeArguments(refType->typeArgs); }
  for (unsigned int i = 0; i < refType->refTypes.size(); i++) {
    setReferenceType(refType->refTypes[i]);
  }
}

void Output::setSelector(const spSelector &selector) {
  if (selector->opt == Selector::OPT_IDENTIFIER_ARGUMENTS) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    if (selector->id) { setIdentifier(selector->id); }
    if (selector->args) { setArguments(selector->args); }
  }

  if (selector->opt == Selector::OPT_EXPLICIT_GENERIC_INVOCATION) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    // TODO:
    //if (selector->explGenInvocation) {
    //  setExplicitGenericInvocation(selector->explGenInvocation);
    //}
  }

  if (selector->opt == Selector::OPT_THIS) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    if (selector->tokThis) { setKeyword(selector->tokThis); }
  }

  if (selector->opt == Selector::OPT_SUPER_SUPER_SUFFIX) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    if (selector->tokSuper) { setKeyword(selector->tokSuper); }
    // TODO:
    //if (selector->superSuffix) { setSuperSuffix(selector->superSuffix); }
  }

  if (selector->opt == Selector::OPT_NEW) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    if (selector->tokNew) { setKeyword(selector->tokNew); }
    if (selector->nonWildcardTypeArguments) {
      setNonWildcardTypeArguments(selector->nonWildcardTypeArguments);
    }
    // TODO:
    //if (selector->innerCreator) {
    //  setInnerCreator(selector->innerCreator);
    //}
  }

  if (selector->opt == Selector::OPT_EXPRESSION) {
    if (selector->arrayPair.first) {
      setOp(selector->arrayPair.first);
    }

    if (selector->expr) {
      setExpression(selector->expr);
    }

    if (selector->arrayPair.second) {
      setOp(selector->arrayPair.second);
    }
  }
}

void Output::setStatement(const spStatement &stmt) {
  if (stmt->opt == Statement::OPT_BLOCK) {
    if (stmt->block) {
      setBlock(stmt->block);
    }
    return;
  }

  if (stmt->opt == Statement::OPT_SEMI_COLON) {
    if (stmt->posSemiColon) {
      setOp(stmt->posSemiColon);
    }

    return;
  }

  if (stmt->opt == Statement::OPT_ID_STMT) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_STMT_EXPR) {
    if (stmt->stmtExpr) {
      setStatementExpression(stmt->stmtExpr);
    }

    if (stmt->posSemiColon) {
      setOp(stmt->posSemiColon);
    }
    return;
  }

  if (stmt->opt == Statement::OPT_IF) {
    if (stmt->tokIf) {
      setKeyword(stmt->tokIf);
    }

    if (stmt->parExpr) {
      setParExpression(stmt->parExpr);
    }

    if (stmt->stmtIf) {
      setStatement(stmt->stmtIf);
    }

    if (stmt->tokElse) {
      setKeyword(stmt->tokElse);
    }

    if (stmt->stmtElse) {
      setStatement(stmt->stmtElse);
    }

    return;
  }

  if (stmt->opt == Statement::OPT_ASSERT) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_SWITCH) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_WHILE) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_DO) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_BREAK) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_CONTINUE) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_RETURN) {
    if (stmt->tokReturn) {
      setKeyword(stmt->tokReturn);
    }

    if (stmt->exprReturn) {
      setExpression(stmt->exprReturn);
    }

    if (stmt->posSemiColon) {
      setOp(stmt->posSemiColon);
    }

    return;
  }

  if (stmt->opt == Statement::OPT_THROW) {
    if (stmt->tokThrow) {
      setKeyword(stmt->tokThrow);
    }

    if (stmt->throwExpr) {
      setExpression(stmt->throwExpr);
    }

    if (stmt->posSemiColon) {
      setOp(stmt->posSemiColon);
    }

    return;
  }

  if (stmt->opt == Statement::OPT_SYNC) {
    // TODO:
    return;
  }

  // try Block ( Catches | [Catches] Finally )
  if (stmt->opt == Statement::OPT_TRY_BLOCK) {
    if (stmt->tokTry) { setKeyword(stmt->tokTry); }
    if (stmt->block) { setBlock(stmt->block); }
    if (stmt->catches) { setCatches(stmt->catches); }
    if (stmt->finally) { setFinally(stmt->finally); }
    return;
  }

  if (stmt->opt == Statement::OPT_TRY_RESOURCE) {
    // TODO:
    return;
  }
}

void Output::setStatementExpression(const spStatementExpression &stmtExpr) {
  if (stmtExpr->expr) {
    setExpression(stmtExpr->expr);
  }
}

void Output::setStringLiteral(const spStringLiteral &strLiteral) {
  unsigned ini = strLiteral->pos + 1;
  unsigned end = ini + strLiteral->val.length();
  output += "(djp-node-string-literal "
    + itos(ini) + " " + itos(end) + ")";
}

void Output::setType(const spType &type) {
  if (type->opt == Type::OPT_BASIC_TYPE) {
    if (type->basicType->token) {
      setKeyword(type->basicType->token);
    }
    setArrayDepth(type->arrayDepth);
    return;
  }

  if (type->opt == Type::OPT_REFERENCE_TYPE && type->refType) {
    setReferenceType(type->refType);
    setArrayDepth(type->arrayDepth);
  }
}

void Output::setTypeArgument(const spTypeArgument &typeArg) {
  if (typeArg->opt == TypeArgument::OPT_REFERENCE_TYPE) {
    if (typeArg->refType) { setReferenceType(typeArg->refType); }
  }

  if (typeArg->opt == TypeArgument::OPT_QUESTION_MARK) {
    if (!typeArg->opt2) { return; }

    setOp(typeArg->opt2->posQuestionMark, 1);

    if (typeArg->opt2->tokExtendsOrSuper) {
      setKeyword(typeArg->opt2->tokExtendsOrSuper);
    }

    if (typeArg->opt2->refType) {
      setReferenceType(typeArg->opt2->refType);
    }
  }
}

void Output::setTypeArguments(const spTypeArguments &typeArgs) {
  if (typeArgs->posLt) { setOp(typeArgs->posLt, 1); }
  if (typeArgs->posGt) { setOp(typeArgs->posGt, 1); }
  if (typeArgs->typeArg) { setTypeArgument(typeArgs->typeArg); }
  for (unsigned int i = 0; i < typeArgs->typeArgs.size(); i++) {
    setTypeArgument(typeArgs->typeArgs[i]);
  }
}

void Output::setTypeDeclarations(
  const std::vector<spTypeDeclaration> &typeDecls) {

  for (std::size_t i = 0; i < typeDecls.size(); i++) {
    if (typeDecls[i]->decl) {
      setClassOrInterfaceDeclaration(typeDecls[i]->decl);
    }
  }
}

void Output::setTypeArgumentsOrDiamond(
  const spTypeArgumentsOrDiamond typeArgsOrDiam) {
  // TODO:
}

void Output::setVariableDeclarator(const spVariableDeclarator &varDecl) {
  if (varDecl->id) {
    setIdentifier(varDecl->id);
  }

  if (varDecl->varDeclRest) {
    setVariableDeclaratorRest(varDecl->varDeclRest);
  }
}

void Output::setVariableDeclaratorId(
  const spVariableDeclaratorId &varDeclId) {
  if (varDeclId->identifier) {
    setIdentifier(varDeclId->identifier);
  }
}

void Output::setVariableDeclaratorRest(
  const spVariableDeclaratorRest &varDeclRest) {

  setArrayDepth(varDeclRest->arrayDepth);

  if (varDeclRest->posEquals) {
    setOp(varDeclRest->posEquals);
  }

  if (varDeclRest->varInit) {
    setVariableInitializer(varDeclRest->varInit);
  }
}

void Output::setVariableDeclarators(const spVariableDeclarators &varDecls) {
  if (varDecls->varDecl) {
    setVariableDeclarator(varDecls->varDecl);
  }

  for (unsigned i = 0; i < varDecls->semiColonAndVarDecls.size(); i++) {
    std::pair<unsigned, spVariableDeclarator> pair =
      varDecls->semiColonAndVarDecls[i];
    setOp(pair.first);
    setVariableDeclarator(pair.second);
  }
}

void Output::setVariableInitializer(const spVariableInitializer &varInit) {
  if (varInit->opt == VariableInitializer::OPT_ARRAY_INITIALIZER) {
    if (varInit->arrayInit) {
      setArrayInitializer(varInit->arrayInit);
    }
    return;
  }

  if (varInit->opt == VariableInitializer::OPT_EXPRESSION) {
    if (varInit->expr) {
      setExpression(varInit->expr);
    }
    return;
  }
}

void Output::setVariableModifier(const spVariableModifier &varModifier) {
  if (varModifier->tokFinal) {
    setKeyword(varModifier->tokFinal);
  }

  setAnnotations(varModifier->annotations);
}

void Output::setVoidMethodDeclaratorRest(
  const spVoidMethodDeclaratorRest &voidMethDeclRest) {

  if (voidMethDeclRest->formParams) {
    setFormalParameters(voidMethDeclRest->formParams);
  }

  if (voidMethDeclRest->tokThrows) {
    setKeyword(voidMethDeclRest->tokThrows);
  }

  if (voidMethDeclRest->block) {
    setBlock(voidMethDeclRest->block);
  }

  if (voidMethDeclRest->posSemiColon) {
    setOp(voidMethDeclRest->posSemiColon);
  }
}

// Helper methods
const std::string Output::itos(int i) {
  std::stringstream s;
  s << i;
  return s.str();
}

} // namespace
