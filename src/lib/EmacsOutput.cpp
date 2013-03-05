#include "djp/EmacsOutput.h"

namespace djp {
void EmacsOutput::build() {
  buildSH();
  buildST();
}

void EmacsOutput::buildSH() {
  outSH = "(";
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

  outSH += ")";
}

void EmacsOutput::buildST() {
  outST = "[";
  for (unsigned i = 0; i < st.symbols.size(); i++) {
    outST += "("
      + getSymbolTableType(st.symbols[i]->type) + " "
      + itos(st.symbols[i]->scope) + " "
      + itos(st.symbols[i]->pos) + " "
      + itos(st.symbols[i]->end) + " "
      + itos(st.symbols[i]->line);
    if (st.symbols[i]->metadata.size()) {
      outST += " " + st.symbols[i]->metadata;
    }
    outST += ")";
  }
  outST += "]";
}

const std::string EmacsOutput::getSymbolTableType(int type) {
  STTypes::iterator it = stTypes.find(type);
  if (it == stTypes.end()) {
    return "unknown";
  }

  return it->second;
}

void EmacsOutput::setAnnotationElement(const spAnnotationElement elem) {
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

void EmacsOutput::setAnnotation(const spAnnotation &annotation) {
  if (annotation->qualifiedId) {
    // '@'
    outSH += "(djp-node-annotation-tok-at "
      + itos(annotation->posTokAt + 1) + ")";
    setQualifiedId(annotation->qualifiedId);
  }

  if (annotation->elem) {
    setAnnotationElement(annotation->elem);
  }
}

void EmacsOutput::setAnnotations(
  const std::vector<spAnnotation> &annotations) {

  for (std::size_t i = 0; i < annotations.size(); i++) {
    setAnnotation(annotations[i]);
  }
}

void EmacsOutput::setAnnotationMethodOrConstantRest(
  const spAnnotationMethodOrConstantRest &methodOrConstRest) {

  if (methodOrConstRest->opt
    == AnnotationMethodOrConstantRest::OPT_ANNOTATION_METHOD_REST) {

    if (methodOrConstRest->methRest) {
      setAnnotationMethodRest(methodOrConstRest->methRest);
    }

    return;
  }

  if (methodOrConstRest->opt
    == AnnotationMethodOrConstantRest::OPT_CONSTANT_DECLARATORS_REST) {

    if (methodOrConstRest->constRest) {
      setConstantDeclaratorsRest(methodOrConstRest->constRest);
    }
  }
}

void EmacsOutput::setAnnotationMethodRest(
  const spAnnotationMethodRest &methRest) {

  if (methRest->posLParen) {
    setOp(methRest->posLParen);
  }

  if (methRest->posRParen) {
    setOp(methRest->posRParen);
  }

  if (methRest->posLBracket) {
    setOp(methRest->posLBracket);
  }

  if (methRest->posRBracket) {
    setOp(methRest->posRBracket);
  }

  if (methRest->tokDefault) {
    setKeyword(methRest->tokDefault);
  }

  if (methRest->elemVal) {
    setElementValue(methRest->elemVal);
  }
}

void EmacsOutput::setAnnotationTypeBody(
  const spAnnotationTypeBody &annTypeBody) {

  if (annTypeBody->elemDecls) {
    setAnnotationTypeElementDeclarations(annTypeBody->elemDecls);
  }
}

void EmacsOutput::setAnnotationTypeDeclaration(
  const spAnnotationTypeDeclaration &annotationDecl) {

  if (annotationDecl->posAt) {
    outSH +=
      "(djp-node-annotation-tok-at "
      + itos(annotationDecl->posAt + 1)
      + ")";
  }

  if (annotationDecl->tokInterface) {
    setKeyword(annotationDecl->tokInterface);
  }

  if (annotationDecl->id) {
    setIdentifier(annotationDecl->id,
      EmacsOutput::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  if (annotationDecl->annTypeBody) {
    setAnnotationTypeBody(annotationDecl->annTypeBody);
  }
}

void EmacsOutput::setAnnotationTypeElementDeclaration(
  const spAnnotationTypeElementDeclaration &elemDecl) {

  if (elemDecl->modifier) {
    setModifier(elemDecl->modifier);
  }

  if (elemDecl->elemRest) {
    setAnnotationTypeElementRest(elemDecl->elemRest);
  }
}

void EmacsOutput::setAnnotationTypeElementDeclarations(
  const spAnnotationTypeElementDeclarations &elemDecls) {

  for (unsigned i = 0; i < elemDecls->elemDecls.size(); i++) {
    setAnnotationTypeElementDeclaration(elemDecls->elemDecls[i]);
  }
}

void EmacsOutput::setAnnotationTypeElementRest(
  const spAnnotationTypeElementRest &elemRest) {

  if (elemRest->opt == AnnotationTypeElementRest::OPT_METHOD_OR_CONSTANT) {
    if (elemRest->type) {
      setType(elemRest->type);
    }

    if (elemRest->id) {
      setIdentifier(elemRest->id);
    }

    if (elemRest->methodOrConstRest) {
      setAnnotationMethodOrConstantRest(elemRest->methodOrConstRest);
    }

    if (elemRest->posSemiColon) {
      setOp(elemRest->posSemiColon);
    }

    return;
  }

  if (elemRest->opt == AnnotationTypeElementRest::OPT_CLASS_DECLARATION) {
    if (elemRest->classDecl) {
      setClassDeclaration(elemRest->classDecl);
    }
    return;
  }

  if (elemRest->opt == AnnotationTypeElementRest::OPT_INTERFACE_DECLARATION) {
    if (elemRest->interfaceDecl) {
      setInterfaceDeclaration(elemRest->interfaceDecl);
    }
    return;
  }

  if (elemRest->opt == AnnotationTypeElementRest::OPT_ENUM_DECLARATION) {
    if (elemRest->enumDecl) {
      setEnumDeclaration(elemRest->enumDecl);
    }
    return;
  }

  if (elemRest->opt == AnnotationTypeElementRest::OPT_ANNOTATION_DECLARATION) {

    if (elemRest->annotationDecl) {
      setAnnotationTypeDeclaration(elemRest->annotationDecl);
    }
    return;
  }
}

void EmacsOutput::setArguments(const spArguments &args) {
  if (args->posLParen) { setOp(args->posLParen); }
  if (args->posRParen) { setOp(args->posRParen); }
  if (args->expr) { setExpression(args->expr); }

  for (unsigned int i = 0; i < args->exprs.size(); i++) {
    setOp(args->exprs[i].first);
    setExpression(args->exprs[i].second);
  }
}

void EmacsOutput::setArrayCreatorRest(
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

      if (exprInBr->posLBracket) {
        setOp(exprInBr->posLBracket);
      }

      if (exprInBr->posRBracket) {
        setOp(exprInBr->posRBracket);
      }

      if (exprInBr->expr) {
        setExpression(exprInBr->expr);
      }

    }
    setArrayDepth(arrayCreatorRest->opt2->arrayDepth);
  }
}

void EmacsOutput::setArrayDepth(ArrayDepth &arrayDepth) {
  for (unsigned int i = 0; i < arrayDepth.size(); i++) {
    unsigned posOpen = arrayDepth[i].first;
    unsigned posClose = arrayDepth[i].second;
    setOp(posOpen);
    setOp(posClose);
  }
}

void EmacsOutput::setArrayInitializer(const spArrayInitializer arrayInit) {
  if (arrayInit->posLCBrace) {
    setOp(arrayInit->posLCBrace);
  }

  if (arrayInit->varInit) {
    setVariableInitializer(arrayInit->varInit);
  }

  for (unsigned i = 0; i < arrayInit->pairs.size(); i++) {
    setOp(arrayInit->pairs[i].first);
    setVariableInitializer(arrayInit->pairs[i].second);
  }

  if (arrayInit->posComma) {
    setOp(arrayInit->posComma);
  }

  if (arrayInit->posRCBrace) {
    setOp(arrayInit->posRCBrace);
  }
}

void EmacsOutput::setBasicType(const spBasicType &basicType) {
  if (basicType->token) {
    setKeyword(basicType->token);
  }
}

void EmacsOutput::setBlock(const spBlock &block) {
  if (block->posLCBracket) { setOp(block->posLCBracket); }

  for (unsigned int i = 0; i < block->blockStmts.size(); i++) {
    setBlockStatement(block->blockStmts[i]);
  }

  if (block->posRCBracket) { setOp(block->posRCBracket); }
}

void EmacsOutput::setBlockStatement(const spBlockStatement &blockStmt) {
  if (blockStmt->opt == BlockStatement::OPT_LOCAL_VAR) {
    if (blockStmt->localVar) {
      setLocalVariableDeclarationStatement(blockStmt->localVar);
    }
    return;
  }

  if (blockStmt->opt == BlockStatement::OPT_CLASS_OR_INTERFACE_DECL) {
    if (blockStmt->classOrIntDecl) {
      setClassOrInterfaceDeclaration(blockStmt->classOrIntDecl);
    }
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

void EmacsOutput::setBound(const spBound &bound) {
  if (bound->refType) {
    setReferenceType(bound->refType);
  }

  for (unsigned i = 0; i < bound->pairs.size(); i++) {
    setOp(bound->pairs[i].first);
    setReferenceType(bound->pairs[i].second);
  }
}

void EmacsOutput::setCatches(const spCatches &catches) {
  if (catches->catchClause) { setCatchClause(catches->catchClause); }
  for (unsigned int i = 0; i < catches->catchClauses.size(); i++) {
    setCatchClause(catches->catchClauses[0]);
  }
}

void EmacsOutput::setCatchClause(const spCatchClause &catchClause) {
  if (catchClause->tokCatch) { setKeyword(catchClause->tokCatch); }
  if (catchClause->posLParen) { setOp(catchClause->posLParen); }
  if (catchClause->varMod) { setVariableModifier(catchClause->varMod); }
  if (catchClause->catchType) { setCatchType(catchClause->catchType); }
  if (catchClause->id) { setIdentifier(catchClause->id); }
  if (catchClause->posRParen) { setOp(catchClause->posRParen); }
  if (catchClause->block) { setBlock(catchClause->block); }
}

void EmacsOutput::setCatchType(const spCatchType &catchType) {
  if (catchType->qualifiedId) {
    setQualifiedId(catchType->qualifiedId);
  }

  for (unsigned i = 0; i < catchType->pairs.size(); i++) {
    std::pair<unsigned, spQualifiedIdentifier> pair = catchType->pairs[i];
    setOp(pair.first);
    setQualifiedId(pair.second);
  }
}

void EmacsOutput::setCharacterLiteral(const spCharacterLiteral &charLiteral) {
  if (charLiteral->pos && charLiteral->val.size()) {
    outSH += "(djp-node-literal-char "
      + itos(charLiteral->pos + 1)
      + " "
      + itos(charLiteral->pos + charLiteral->val.size() + 1);
    outSH += ")";
  }
}

void EmacsOutput::setClassBody(const spClassBody &classBody) {
  for (std::size_t i = 0; i < classBody->classBodyDecls.size(); i++) {
    setClassBodyDeclaration(classBody->classBodyDecls[i]);
  }
}

/// We have 3 options export:
///   (djp-member-decl-modifier-member-decl ...) -> {Modifier} MemberDecl
///   (djp-member-decl-modifier-static-block ...) -> [static] Block
///   (djp-op ...)
void EmacsOutput::setClassBodyDeclaration(const spClassBodyDeclaration &decl) {
  if (decl->opt == ClassBodyDeclaration::OPT_MODIFIER_MEMBER_DECL) {
    outSH += "(djp-member-decl-modifier-member-decl ";
    if (decl->modifier) {
      setModifier(decl->modifier);
    }

    if (decl->memberDecl) {
      setMemberDecl(decl->memberDecl);
    }

    outSH += ")";
    return;
  }

  if (decl->opt == ClassBodyDeclaration::OPT_STATIC_BLOCK) {
    outSH += "(djp-member-decl-modifier-static-block ";
    if (decl->tokStatic) {
      setKeyword(decl->tokStatic);
    }

    if (decl->block) {
      setBlock(decl->block);
    }

    outSH += ")";
    return;
  }

  if (decl->opt == ClassBodyDeclaration::OPT_SEMICOLON) {
    if (decl->posSemiColon) {
      setOp(decl->posSemiColon);
    }
  }
}

void EmacsOutput::setClassCreatorRest(const spClassCreatorRest &classCreatorRest) {
  if (classCreatorRest->args) {
    setArguments(classCreatorRest->args);
  }

  if (classCreatorRest->classBody) {
    setClassBody(classCreatorRest->classBody);
  }
}

void EmacsOutput::setClassDeclaration(const spClassDeclaration &classDecl) {
  if (classDecl->nClassDecl) {
    setNormalClassDeclaration(classDecl->nClassDecl);
    return;
  }

  if (classDecl->enumDecl) {
    setEnumDeclaration(classDecl->enumDecl);
    return;
  }
}

void EmacsOutput::setClassOrInterfaceDeclaration(
  const spClassOrInterfaceDeclaration &decl) {

  outSH += "(djp-class-or-interface-declaration ";

  if (decl->modifier) {
    setModifier(decl->modifier);
  }

  if (decl->opt == ClassOrInterfaceDeclaration::OPT_CLASS) {
    if (decl->classDecl) {
      setClassDeclaration(decl->classDecl);
    }
  } else if (decl->opt == ClassOrInterfaceDeclaration::OPT_INTERFACE) {
    if (decl->interfaceDecl) {
      setInterfaceDeclaration(decl->interfaceDecl);
    }
  }

  outSH += ")";
}

void EmacsOutput::setComments() {
  if (comments.size() == 0) { return; }

  outSH += "(djp-comments ";
  for (unsigned int i = 0; i < comments.size(); i++) {
    outSH += "(djp-comment " + itos(comments[i]->posIni + 1)
      + " " + itos(comments[i]->posEnd + 2) + ")";
  }

  outSH += ")";
}

void EmacsOutput::setConstantDeclaratorRest(
  const spConstantDeclaratorRest &constDeclRest) {

  setArrayDepth(constDeclRest->arrayDepth);

  if (constDeclRest->posEquals) {
    setOp(constDeclRest->posEquals);
  }

  if (constDeclRest->varInit) {
    setVariableInitializer(constDeclRest->varInit);
  }
}

void EmacsOutput::setConstantDeclaratorsRest(
  const spConstantDeclaratorsRest &constDeclsRest) {

  if (constDeclsRest->constDeclRest) {
    setConstantDeclaratorRest(constDeclsRest->constDeclRest);
  }

  for (unsigned i = 0; i < constDeclsRest->pairs.size(); i++) {
    setOp(constDeclsRest->pairs[i].first);
    setConstantDeclaratorRest(constDeclsRest->pairs[i].second);
  }
}

void EmacsOutput::setConstructorDeclaratorRest(
  const spConstructorDeclaratorRest &constDeclRest) {
  outSH += "(djp-constructor-declarator-rest ";

  if (constDeclRest->formParams && constDeclRest->formParams->formParamDecls) {
    setFormalParameterDecls(constDeclRest->formParams->formParamDecls);
  }

  if (constDeclRest->block) {
    setBlock(constDeclRest->block);
  }

  outSH += ")";
}

void EmacsOutput::setCreatedName(const spCreatedName &createdName) {
  if (createdName->id) {
    setIdentifier(createdName->id, EmacsOutput::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  if (createdName->typeArgsOrDiam) {
    setTypeArgumentsOrDiamond(createdName->typeArgsOrDiam);
  }

  for (unsigned i = 0; i < createdName->createdNames.size(); i++) {
    setCreatedName(createdName->createdNames[i]);
  }
}

void EmacsOutput::setCreator(const spCreator &creator) {
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

    return;
  }

  if (creator->opt == Creator::OPT_BASIC_TYPE) {
    if (!creator->opt3) { return; }

    if (creator->opt3->basicType && creator->opt3->basicType->token) {
      setKeyword(creator->opt3->basicType->token);
    }

    if (creator->opt3->arrayCreatorRest) {
      setArrayCreatorRest(creator->opt3->arrayCreatorRest);
    }
  }
}

void EmacsOutput::setElementValue(const spElementValue &value) {
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
      setElementValueArrayInitializer(value->elemValArrayInit);
    }
  }
}

void EmacsOutput::setElementValues(const spElementValues &values) {
  if (values->elemVal) {
    setElementValue(values->elemVal);
  }

  for (unsigned i = 0; i < values->pairs.size(); i++) {
    setOp(values->pairs[i].first);
    setElementValue(values->pairs[i].second);
  }
}

void EmacsOutput::setElementValueArrayInitializer(
  const spElementValueArrayInitializer &elemValArrayInit) {

  if (elemValArrayInit->posLCBrace) {
    setOp(elemValArrayInit->posLCBrace);
  }

  if (elemValArrayInit->elemVals) {
    setElementValues(elemValArrayInit->elemVals);
  }

  if (elemValArrayInit->posComma) {
    setOp(elemValArrayInit->posComma);
  }

  if (elemValArrayInit->posRCBrace) {
    setOp(elemValArrayInit->posRCBrace);
  }
}

void EmacsOutput::setElementValuePair(const spElementValuePair &pair) {
  if (pair->id) {
    setIdentifier(pair->id);
  }

  if (pair->value) {
    setElementValue(pair->value);
  }
}

void EmacsOutput::setEnumBody(const spEnumBody &enumBody) {
  if (enumBody->posLCBrace) {
    setOp(enumBody->posLCBrace);
  }

  if (enumBody->enumConsts) {
    setEnumConstants(enumBody->enumConsts);
  }

  if (enumBody->bodyDecls) {
    setEnumBodyDeclarations(enumBody->bodyDecls);
  }

  if (enumBody->posRCBrace) {
    setOp(enumBody->posRCBrace);
  }
}

void EmacsOutput::setEnumBodyDeclarations(
  const spEnumBodyDeclarations &bodyDecls) {

  if (bodyDecls->posSemiColon) {
    setOp(bodyDecls->posSemiColon);
  }

  for (unsigned i = 0; i < bodyDecls->classBodyDecls.size(); i++) {
    setClassBodyDeclaration(bodyDecls->classBodyDecls[i]);
  }
}

void EmacsOutput::setEnumConstant(const spEnumConstant &enumConst) {
  setAnnotations(enumConst->annotations);

  if (enumConst->id) {
    setIdentifier(enumConst->id);
  }

  if (enumConst->args) {
    setArguments(enumConst->args);
  }

  if (enumConst->classBody) {
    setClassBody(enumConst->classBody);
  }
}

void EmacsOutput::setEnumConstants(const spEnumConstants &enumConsts) {
  if (enumConsts->enumConst) {
    setEnumConstant(enumConsts->enumConst);
  }

  for (unsigned i = 0; i < enumConsts->pairs.size(); i++) {
    setOp(enumConsts->pairs[i].first);
    setEnumConstant(enumConsts->pairs[i].second);
  }
}


void EmacsOutput::setEnumDeclaration(spEnumDeclaration &enumDecl) {
  if (enumDecl->tokEnum) {
    setKeyword(enumDecl->tokEnum);
  }

  if (enumDecl->id) {
    setIdentifier(enumDecl->id, EmacsOutput::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  if (enumDecl->tokImpl) {
    setKeyword(enumDecl->tokImpl);
  }

  if (enumDecl->typeList) {
    setTypeList(enumDecl->typeList);
  }

  if (enumDecl->enumBody) {
    setEnumBody(enumDecl->enumBody);
  }
}

void EmacsOutput::setErrors(const std::vector<spError> &errors) {
  for (std::size_t i = 0; i < errors.size(); i++) {
    outSH += "(djp-error "
      + itos(errors[i]->ini + 1) + " "
      + itos(errors[i]->end + 1) + " \""
      + errUtil.getMessage(errors[i]->type) + "\")";

  }
}

void EmacsOutput::setExplicitGenericInvocation(
  const spExplicitGenericInvocation &explGenInvocation) {

  if (explGenInvocation->nonWildcardTypeArguments) {
    setNonWildcardTypeArguments(explGenInvocation->nonWildcardTypeArguments);
  }

  if (explGenInvocation->explGen) {
    setExplicitGenericInvocationSuffix(explGenInvocation->explGen);
  }
}

void EmacsOutput::setExplicitGenericInvocationSuffix(
  const spExplicitGenericInvocationSuffix &explGen) {

  if (explGen->opt == ExplicitGenericInvocationSuffix::OPT_SUPER) {
    if (explGen->tokSuper) {
      setKeyword(explGen->tokSuper);
    }

    if (explGen->superSuffix) {
      setSuperSuffix(explGen->superSuffix);
    }

    return;
  }

  if (explGen->opt == ExplicitGenericInvocationSuffix::OPT_IDENTIFIER) {
    if (explGen->id) {
      setIdentifier(explGen->id);
    }

    if (explGen->args) {
      setArguments(explGen->args);
    }
  }
}

void EmacsOutput::setExpression(const spExpression &expr) {
  if (expr->expr1) {
    setExpression1(expr->expr1);
  }

  if (expr->assignOp && expr->assignOp->tok) {
    setOp(expr->assignOp->tok->pos,
      tokenUtil.getTokenLength(expr->assignOp->tok->type));

    if (expr->assignExpr) {
      setExpression(expr->assignExpr);
    }
  }
}

void EmacsOutput::setExpression1(const spExpression1 &expr1) {
  if (expr1->expr2) {
    setExpression2(expr1->expr2);
  }

  if (expr1->expr1Rest) {
    setExpression1Rest(expr1->expr1Rest);
  }
}

void EmacsOutput::setExpression1Rest(const spExpression1Rest &expr1Rest) {
  if (expr1Rest->posQuestionMark) {
    setOp(expr1Rest->posQuestionMark);
  }

  if (expr1Rest->expr) {
    setExpression(expr1Rest->expr);
  }

  if (expr1Rest->posColon) {
    setOp(expr1Rest->posColon);
  }

  if (expr1Rest->expr1) {
    setExpression1(expr1Rest->expr1);
  }
}

void EmacsOutput::setExpression2(const spExpression2 &expr2) {
  if (expr2->expr3) {
    setExpression3(expr2->expr3);
  }

  if (expr2->expr2Rest) {
    setExpression2Rest(expr2->expr2Rest);
  }
}

void EmacsOutput::setExpression2Rest(const spExpression2Rest &expr2Rest) {
  for (unsigned int i = 0; i < expr2Rest->pairs.size(); i++) {
    if (expr2Rest->pairs[i]->opt == Expression2RestHelper::OPT_INFIXOP_EXPR3) {
      setOp(expr2Rest->pairs[i]->tokInfixOp->pos,
        tokenUtil.getTokenLength(expr2Rest->pairs[i]->tokInfixOp->type));
      setExpression3(expr2Rest->pairs[i]->expr3);
      continue;
    }

    if (expr2Rest->pairs[i]->opt
      == Expression2RestHelper::OPT_INSTANCEOF_TYPE) {

      setKeyword(expr2Rest->pairs[i]->tokInstanceOf);
      setType(expr2Rest->pairs[i]->type);
    }
  }
}

void EmacsOutput::setExpression3(const spExpression3 &expr3) {
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

  if (expr3->opt == Expression3::OPT_TYPE_EXPRESSION3) {
    if (expr3->opt2->posLParen) {
      setOp(expr3->opt2->posLParen);
    }

    if (expr3->opt2->type) {
      setType(expr3->opt2->type);
    }

    if (expr3->opt2->expr3) {
      setExpression3(expr3->opt2->expr3);
    }

    if (expr3->opt2->posRParen) {
      setOp(expr3->opt2->posRParen);
    }

    return;
  }

  if (expr3->opt == Expression3::OPT_TYPE_EXPRESSION3) {
    if (expr3->opt3->posLParen) {
      setOp(expr3->opt3->posLParen);
    }

    if (expr3->opt3->expr) {
      setExpression(expr3->opt3->expr);
    }

    if (expr3->opt3->expr3) {
      setExpression3(expr3->opt3->expr3);
    }

    if (expr3->opt3->posRParen) {
      setOp(expr3->opt3->posRParen);
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

void EmacsOutput::setFieldDeclsRest(
  const spFieldDeclaratorsRest &fieldDeclsRest) {

  if (fieldDeclsRest->varDeclRest) {
    setVariableDeclaratorRest(fieldDeclsRest->varDeclRest);
  }

  for (unsigned i = 0; i < fieldDeclsRest->pairs.size(); i++) {
    setOp(fieldDeclsRest->pairs[i].first);
    setVariableDeclarator(fieldDeclsRest->pairs[i].second);
  }
}

void EmacsOutput::setFinally(const spFinally &finally) {
  if (finally->tokFinally) { setKeyword(finally->tokFinally); }
  if (finally->block) { setBlock(finally->block); }
}

void EmacsOutput::setFloatingPointLiteral(
  const spFloatingPointLiteral &fpLiteral) {

  if (fpLiteral->pos && fpLiteral->value.size()) {
    outSH += "(djp-node-literal-number "
      + itos(fpLiteral->pos + 1)
      + " "
      + itos(fpLiteral->pos + fpLiteral->value.size() + 1);
    outSH += ")";
  }
}

void EmacsOutput::setForControl(const spForControl &forCtrl) {
  if (forCtrl->opt == ForControl::OPT_FOR_VAR_CTRL) {
    if (forCtrl->varCtrl) {
      setForVarControl(forCtrl->varCtrl);
    }
    return;
  }

  if (forCtrl->opt == ForControl::OPT_FOR_INIT) {
    if (forCtrl->forInit) {
      setForInit(forCtrl->forInit);
    }

    if (forCtrl->posSemiColon1) {
      setOp(forCtrl->posSemiColon1);
    }

    if (forCtrl->expr) {
      setExpression(forCtrl->expr);
    }

    if (forCtrl->posSemiColon2) {
      setOp(forCtrl->posSemiColon2);
    }

    if (forCtrl->forUpdate) {
      setForUpdate(forCtrl->forUpdate);
    }
  }
}

void EmacsOutput::setForInit(const spForInit &forInit) {
  if (forInit->stmtExpr) {
    setStatementExpression(forInit->stmtExpr);
  }

  for (unsigned i = 0; i < forInit->pairs.size(); i++) {
    setOp(forInit->pairs[i].first);
    setStatementExpression(forInit->pairs[i].second);
  }
}

void EmacsOutput::setForUpdate(const spForUpdate &forUpdate) {
  setForInit(forUpdate);
}

void EmacsOutput::setForVarControl(const spForVarControl &varCtrl) {
  if (varCtrl->varMod) {
    setVariableModifier(varCtrl->varMod);
  }

  if (varCtrl->type) {
    setType(varCtrl->type);
  }

  if (varCtrl->varDeclId) {
    setVariableDeclaratorId(varCtrl->varDeclId);
  }

  if (varCtrl->forVarCtrlRest) {
    setForVarControlRest(varCtrl->forVarCtrlRest);
  }
}

void EmacsOutput::setForVarControlRest(const spForVarControlRest &forVarCtrlRest) {
  if (forVarCtrlRest->opt == ForVarControlRest::OPT_FOR_VAR_DECLS_REST) {
    if (forVarCtrlRest->forVarDeclsRest) {
      setForVariableDeclaratorsRest(forVarCtrlRest->forVarDeclsRest);
    }

    if (forVarCtrlRest->posSemiColon1) {
      setOp(forVarCtrlRest->posSemiColon1);
    }

    if (forVarCtrlRest->expr) {
      setExpression(forVarCtrlRest->expr);
    }

    if (forVarCtrlRest->posSemiColon2) {
      setOp(forVarCtrlRest->posSemiColon2);
    }

    if (forVarCtrlRest->forUpdate) {
      setForUpdate(forVarCtrlRest->forUpdate);
    }

    return;
  }

  if (forVarCtrlRest->opt == ForVarControlRest::OPT_COLON_EXPR) {
    if (forVarCtrlRest->posColon) {
      setOp(forVarCtrlRest->posColon);
    }

    if (forVarCtrlRest->expr) {
      setExpression(forVarCtrlRest->expr);
    }
  }
}

void EmacsOutput::setForVariableDeclaratorsRest(
  const spForVariableDeclaratorsRest &forVarDeclsRest) {

  if (forVarDeclsRest->posEquals) {
    setOp(forVarDeclsRest->posEquals);

    if (forVarDeclsRest->varInit) {
      setVariableInitializer(forVarDeclsRest->varInit);
    }
  }

  for (unsigned i = 0; i < forVarDeclsRest->pairs.size(); i++) {
    setOp(forVarDeclsRest->pairs[i].first);
    setVariableDeclarator(forVarDeclsRest->pairs[i].second);
  }
}

void EmacsOutput::setFormalParameters(const spFormalParameters &formParams) {
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

void EmacsOutput::setFormalParameterDecls(
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

void EmacsOutput::setFormalParameterDeclsRest(
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

void EmacsOutput::setGenericMethodOrConstructorDecl(
  const spGenericMethodOrConstructorDecl &genMethodOrConstDecl) {

  if (genMethodOrConstDecl->typeParams) {
    setTypeParameters(genMethodOrConstDecl->typeParams);
  }

  if (genMethodOrConstDecl->rest) {
    setGenericMethodOrConstructorRest(genMethodOrConstDecl->rest);
  }
}

void EmacsOutput::setGenericMethodOrConstructorRest(
  const spGenericMethodOrConstructorRest &rest) {

  if (rest->opt == GenericMethodOrConstructorRest::OPT_TYPE_IDENTIFIER) {
    if (rest->type) {
      setType(rest->type);
    }

    if (rest->id) {
      setIdentifier(rest->id);
    }

    if (rest->methodDeclRest) {
      setMethodDeclaratorRest(rest->methodDeclRest);
    }

    return;
  }

  if (rest->opt == GenericMethodOrConstructorRest::OPT_VOID_IDENTIFIER) {
    if (rest->tokVoid) {
      setKeyword(rest->tokVoid);
    }

    if (rest->id) {
      setIdentifier(rest->id);
    }

    if (rest->methodDeclRest) {
      setMethodDeclaratorRest(rest->methodDeclRest);
    }

    return;
  }

  if (rest->opt == GenericMethodOrConstructorRest::OPT_IDENTIFIER_CONSTRUCTOR) {
    if (rest->id) {
      setIdentifier(rest->id);
    }

    if (rest->constDeclRest) {
      setConstructorDeclaratorRest(rest->constDeclRest);
    }

    return;
  }
}

void EmacsOutput::setIdentifier(const spIdentifier &identifier, IdentifierOpt opt) {
  int ini = identifier->pos + 1;
  int end = ini + identifier->value.length();

  if (opt == EmacsOutput::OPT_IDENTIFIER_REFERENCE_TYPE) {
    outSH += "(djp-node-reference-type-id "
      + itos(ini) + " " + itos(end) + ")";
    return;
  }

  outSH += "(djp-node-identifier " + itos(ini) + " " + itos(end) + ")";
}

void EmacsOutput::setIdentifierSuffix(const spIdentifierSuffix &idSuffix) {
  if (idSuffix->opt == IdentifierSuffix::OPT_ARRAY_ARRAY_DEPTH_CLASS) {
    // '['
    if (idSuffix->arrayPair.first) {
      setOp(idSuffix->arrayPair.first);
    }

    // { '[]' }
    setArrayDepth(idSuffix->arrayDepth);

    // '.'
    if (idSuffix->posPeriod) {
      setOp(idSuffix->posPeriod);
    }

    // 'class'
    if (idSuffix->tokClass) {
      setKeyword(idSuffix->tokClass);
    }

    // ']'
    if (idSuffix->arrayPair.second) {
      setOp(idSuffix->arrayPair.second);
    }

    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_ARRAY_EXPRESSION) {
    // '['
    if (idSuffix->arrayPair.first) {
      setOp(idSuffix->arrayPair.first);
    }

    // Expression
    if (idSuffix->expr) {
      setExpression(idSuffix->expr);
    }

    // ']'
    if (idSuffix->arrayPair.second) {
      setOp(idSuffix->arrayPair.second);
    }

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

  if (idSuffix->opt
     == IdentifierSuffix::OPT_PERIOD_EXPLICIT_GENERIC_INVOCATION) {

    if (idSuffix->posPeriod) {
      setOp(idSuffix->posPeriod);
    }

    if (idSuffix->explGenInvocation) {
      setExplicitGenericInvocation(idSuffix->explGenInvocation);
    }

    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_PERIOD_THIS) {
    if (idSuffix->posPeriod) {
      setOp(idSuffix->posPeriod);
    }

    if (idSuffix->tokThis) {
      setKeyword(idSuffix->tokThis);
    }

    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_PERIOD_SUPER_ARGUMENTS) {

    if (idSuffix->posPeriod) {
      setOp(idSuffix->posPeriod);
    }

    if (idSuffix->tokSuper) {
      setKeyword(idSuffix->tokSuper);
    }

    if (idSuffix->args) {
      setArguments(idSuffix->args);
    }

    return;
  }

  if (idSuffix->opt == IdentifierSuffix::OPT_NEW) {
    if (idSuffix->posPeriod) {
      setOp(idSuffix->posPeriod);
    }

    if (idSuffix->tokNew) {
      setKeyword(idSuffix->tokNew);
    }

    if (idSuffix->nonWildcardTypeArguments) {
      setNonWildcardTypeArguments(idSuffix->nonWildcardTypeArguments);
    }

    if (idSuffix->innerCreator) {
      setInnerCreator(idSuffix->innerCreator);
    }

    return;
  }
}

void EmacsOutput::setImportDeclaration(const spImportDeclaration &import) {
  outSH += "(djp-import-declaration ";
  setKeyword(import->posTokImport + 1,
    import->posTokImport + tokenUtil.getTokenLength(TOK_KEY_IMPORT) + 1);

  if (import->posTokStatic > 0) {
    setKeyword(import->posTokStatic + 1,
      import->posTokStatic + tokenUtil.getTokenLength(TOK_KEY_STATIC) + 1);
    }

  if (import->qualifiedId) {
    setQualifiedId(import->qualifiedId);
  }

  if (import->iniOnDemand && import->endOnDemand) {
    setOp(import->iniOnDemand, 1 + import->endOnDemand - import->iniOnDemand);
  }

  outSH += ")";
}

void EmacsOutput::setImportDeclarations(const spImportDeclarations &impDecls) {
  outSH += "(djp-import-declarations ";
  for (std::string::size_type i = 0; i < impDecls->imports.size(); i++) {
    setImportDeclaration(impDecls->imports[i]);
  }
  outSH += ")";
}

void EmacsOutput::setInnerCreator(const spInnerCreator &innerCreator) {
  if (innerCreator->id) {
    setIdentifier(innerCreator->id);
  }

  if (innerCreator->nonWildcardOrDiam) {
    setNonWildcardTypeArgumentsOrDiamond(innerCreator->nonWildcardOrDiam);
  }

  if (innerCreator->classCreatorRest) {
    setClassCreatorRest(innerCreator->classCreatorRest);
  }
}

void EmacsOutput::setIntegerLiteral(const spIntegerLiteral &intLiteral) {
  if (intLiteral->pos && intLiteral->value.size()) {
    outSH += "(djp-node-literal-number "
      + itos(intLiteral->pos + 1)
      + " "
      + itos(intLiteral->pos + intLiteral->value.size() + 1);
    outSH += ")";
  }
}

void EmacsOutput::setInterfaceBody(const spInterfaceBody &body) {
  if (body->posLCBrace) {
    setOp(body->posLCBrace);
  }

  for (unsigned i = 0; i < body->bodyDecls.size(); i++) {
    setInterfaceBodyDeclaration(body->bodyDecls[i]);
  }

  if (body->posRCBrace) {
    setOp(body->posRCBrace);
  }
}

void EmacsOutput::setInterfaceBodyDeclaration(
  const spInterfaceBodyDeclaration &bodyDecl) {

  if (bodyDecl->opt == InterfaceBodyDeclaration::OPT_SEMICOLON) {
    if (bodyDecl->posSemiColon) {
      setOp(bodyDecl->posSemiColon);
    }
    return;
  }

  if (bodyDecl->opt == InterfaceBodyDeclaration::OPT_MEMBER_DECL) {
    if (bodyDecl->modifier) {
      setModifier(bodyDecl->modifier);
    }

    if (bodyDecl->memberDecl) {
      setInterfaceMemberDeclaration(bodyDecl->memberDecl);
    }
  }
}

void EmacsOutput::setInterfaceDeclaration(
  const spInterfaceDeclaration &interfaceDecl) {

  if (interfaceDecl->opt == InterfaceDeclaration::OPT_NORMAL) {
    if (interfaceDecl->normalDecl) {
      setNormalInterfaceDeclaration(interfaceDecl->normalDecl);
    }
    return;
  }

  if (interfaceDecl->opt == InterfaceDeclaration::OPT_ANNOTATION) {
    if (interfaceDecl->annotationDecl) {
      setAnnotationTypeDeclaration(interfaceDecl->annotationDecl);
    }
  }
}

void EmacsOutput::setInterfaceGenericMethodDecl(
  const spInterfaceGenericMethodDecl &genMethodDecl) {

  if (genMethodDecl->typeParams) {
    setTypeParameters(genMethodDecl->typeParams);
  }

  if (genMethodDecl->type) {
    setType(genMethodDecl->type);
  }

  if (genMethodDecl->tokVoid) {
    setKeyword(genMethodDecl->tokVoid);
  }

  if (genMethodDecl->id) {
    setIdentifier(genMethodDecl->id);
  }

  if (genMethodDecl->methDeclRest) {
    setInterfaceMethodDeclaratorRest(genMethodDecl->methDeclRest);
  }
}

void EmacsOutput::setInterfaceMethodDeclaratorRest(
  const spInterfaceMethodDeclaratorRest &methDeclRest) {

  if (methDeclRest->formParams) {
    setFormalParameters(methDeclRest->formParams);
  }

  if (methDeclRest->tokThrows) {
    setKeyword(methDeclRest->tokThrows);
  }

  if (methDeclRest->qualifiedIdList) {
    setQualifiedIdentifierList(methDeclRest->qualifiedIdList);
  }

  if (methDeclRest->posSemiColon) {
    setOp(methDeclRest->posSemiColon);
  }
}

void EmacsOutput::setInterfaceMemberDeclaration(
  const spInterfaceMemberDecl &memberDecl) {

  if (memberDecl->opt
    == InterfaceMemberDecl::OPT_INTERFACE_METHOD_OR_FIELD_DECL) {

    if (memberDecl->methodOrFieldDecl) {
      setInterfaceMethodOrFieldDecl(memberDecl->methodOrFieldDecl);
    }

    return;
  }

  if (memberDecl->opt == InterfaceMemberDecl::OPT_VOID_IDENTIFIER) {
    if (memberDecl->tokVoid) {
      setKeyword(memberDecl->tokVoid);
    }

    if (memberDecl->id) {
      setIdentifier(memberDecl->id);
    }

    if (memberDecl->voidMethDeclRest) {
      setVoidInterfaceMethodDeclaratorRest(memberDecl->voidMethDeclRest);
    }

    return;
  }

  if (memberDecl->opt == InterfaceMemberDecl::OPT_INTERFACE_GENERIC) {
    if (memberDecl->genMethodDecl) {
      setInterfaceGenericMethodDecl(memberDecl->genMethodDecl);
    }

    return;
  }

  if (memberDecl->opt == InterfaceMemberDecl::OPT_CLASS_DECLARATION) {
    if (memberDecl->classDecl) {
      setClassDeclaration(memberDecl->classDecl);
    }

    return;
  }

  if (memberDecl->opt == InterfaceMemberDecl::OPT_INTERFACE_DECLARATION) {
    if (memberDecl->interfaceDecl) {
      setInterfaceDeclaration(memberDecl->interfaceDecl);
    }

    return;
  }
}

void EmacsOutput::setInterfaceMethodOrFieldDecl(
  const spInterfaceMethodOrFieldDecl &methodOrFieldDecl) {

  if (methodOrFieldDecl->type) {
    setType(methodOrFieldDecl->type);
  }

  if (methodOrFieldDecl->id) {
    setIdentifier(methodOrFieldDecl->id);
  }

  if (methodOrFieldDecl->rest) {
    setInterfaceMethodOrFieldRest(methodOrFieldDecl->rest);
  }
}

void EmacsOutput::setInterfaceMethodOrFieldRest(
  const spInterfaceMethodOrFieldRest &rest) {

  if (rest->opt == InterfaceMethodOrFieldRest::OPT_CONSTANT_REST) {
    if (rest->constDeclsRest) {
      setConstantDeclaratorsRest(rest->constDeclsRest);
    }
    return;
  }

  if (rest->opt == InterfaceMethodOrFieldRest::OPT_METHOD_REST) {
    if (rest->methDeclRest) {
      setInterfaceMethodDeclaratorRest(rest->methDeclRest);
    }
  }
}

void EmacsOutput::setKeyword(const spTokenExp &token) {
  setKeyword(token->pos + 1, token->pos + 1
    + tokenUtil.getTokenLength(token->type));
}

void EmacsOutput::setKeyword(int ini, int end) {
  outSH += "(djp-node-keyword " + itos(ini) + " " + itos(end) + ")";
}

void EmacsOutput::setLiteral(const spLiteral &literal) {
  if (literal->opt == Literal::OPT_INTEGER) {
    if (literal->intLiteral) {
      setIntegerLiteral(literal->intLiteral);
    }
    return;
  }

  if (literal->opt == Literal::OPT_FLOATING_POINT) {
    if (literal->fpLiteral) {
      setFloatingPointLiteral(literal->fpLiteral);
    }
    return;
  }

  if (literal->opt == Literal::OPT_CHAR) {
    if (literal->charLiteral) {
      setCharacterLiteral(literal->charLiteral);
    }
    return;
  }

  if (literal->opt == Literal::OPT_STRING) {
    if (literal->strLiteral) {
      setStringLiteral(literal->strLiteral);
    }
    return;
  }

  if (literal->opt == Literal::OPT_BOOLEAN) {
    if (literal->boolLiteral) {
      if (literal->boolLiteral->val) {
        // true
        setKeyword(literal->boolLiteral->pos, literal->boolLiteral->pos + 5);
      } else {
        // false
        setKeyword(literal->boolLiteral->pos, literal->boolLiteral->pos + 6);
      }
    }
    return;
  }

  if (literal->opt == Literal::OPT_NULL) {
    if (literal->nullLiteral) {
      setKeyword(literal->nullLiteral);
    }
    return;
  }
}

void EmacsOutput::setLocalVariableDeclarationStatement(
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


void EmacsOutput::setMemberDecl(const spMemberDecl &memberDecl) {
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

  if (memberDecl->opt == MemberDecl::OPT_GENERIC_METHOD_OR_CONSTRUCTOR_DECL) {
    if (memberDecl->genMethodOrConstDecl) {
      setGenericMethodOrConstructorDecl(memberDecl->genMethodOrConstDecl);
    }
    return;
  }

  if (memberDecl->opt == MemberDecl::OPT_CLASS_DECLARATION) {
    if (memberDecl->classDecl) {
      setClassDeclaration(memberDecl->classDecl);
    }
    return;
  }

  if (memberDecl->opt == MemberDecl::OPT_INTERFACE_DECLARATION) {
    if (memberDecl->interfaceDecl) {
      setInterfaceDeclaration(memberDecl->interfaceDecl);
    }
    return;
  }
}

void EmacsOutput::setMethodDeclaratorRest(
  const spMethodDeclaratorRest &methodDeclRest) {

  if (methodDeclRest->formParams) {
    setFormalParameters(methodDeclRest->formParams);
  }

  setArrayDepth(methodDeclRest->arrayDepth);

  if (methodDeclRest->tokThrows) {
    setKeyword(methodDeclRest->tokThrows);
  }

  if (methodDeclRest->qualifiedIdList) {
    setQualifiedIdentifierList(methodDeclRest->qualifiedIdList);
  }

  if (methodDeclRest->block) {
    setBlock(methodDeclRest->block);
  }

  if (methodDeclRest->posSemiColon) {
    setOp(methodDeclRest->posSemiColon);
  }
}

void EmacsOutput::setMethodOrFieldDecl(
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

void EmacsOutput::setMethodOrFieldRest(
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

void EmacsOutput::setModifier(const spModifier &modifier) {
  if (modifier->annotations.size()) {
    setAnnotations(modifier->annotations);
  }

  for (std::size_t i = 0; i < modifier->tokens.size(); i++) {
    setKeyword(modifier->tokens[i]);
  }
}

void EmacsOutput::setNonWildcardTypeArguments(
  const spNonWildcardTypeArguments &nonWildcardTypeArguments) {

  if (nonWildcardTypeArguments->posLt) {
    setOp(nonWildcardTypeArguments->posLt);
  }

  if (nonWildcardTypeArguments->typeList2) {
    setTypeList2(nonWildcardTypeArguments->typeList2);
  }

  if (nonWildcardTypeArguments->posGt) {
    setOp(nonWildcardTypeArguments->posGt);
  }
}

void EmacsOutput::setNonWildcardTypeArgumentsOrDiamond(
  const spNonWildcardTypeArgumentsOrDiamond &nonWildcardOrDiam) {

  if (nonWildcardOrDiam->opt
    == NonWildcardTypeArgumentsOrDiamond::OPT_DIAMOND) {

    if (nonWildcardOrDiam->diamond.first) {
      setOp(nonWildcardOrDiam->diamond.first);
    }

    if (nonWildcardOrDiam->diamond.second) {
      setOp(nonWildcardOrDiam->diamond.second);
    }

    return;
  }

  if (nonWildcardOrDiam->opt
    == NonWildcardTypeArgumentsOrDiamond::OPT_NON_WILDCARD_TYPE_ARGUMENTS) {

    if (nonWildcardOrDiam->nonWildcardTypeArguments) {
      setNonWildcardTypeArguments(nonWildcardOrDiam->nonWildcardTypeArguments);
    }
  }
}

void EmacsOutput::setNormalClassDeclaration(
  const spNormalClassDeclaration &nClassDecl) {

  outSH += "(djp-normal-class-declaration ";

  if (nClassDecl->classTok) {
    setKeyword(nClassDecl->classTok);
  }

  if (nClassDecl->id) {
    setIdentifier(nClassDecl->id,
      EmacsOutput::OPT_IDENTIFIER_REFERENCE_TYPE);
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

  if (nClassDecl->typeParams) {
    setTypeParameters(nClassDecl->typeParams);
  }

  if (nClassDecl->implementsTok) {
    setKeyword(nClassDecl->implementsTok);
    if (nClassDecl->typeList) {
      setTypeList(nClassDecl->typeList);
    }
  }

  outSH += ")";
}

void EmacsOutput::setNormalInterfaceDeclaration(
  const spNormalInterfaceDeclaration &normalDecl) {
  if (normalDecl->tokInterface) {
    setKeyword(normalDecl->tokInterface);
  }

  if (normalDecl->id) {
    setIdentifier(normalDecl->id,
      EmacsOutput::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  if (normalDecl->typeParams) {
    setTypeParameters(normalDecl->typeParams);
  }

  if (normalDecl->tokExtends) {
    setKeyword(normalDecl->tokExtends);
  }

  if (normalDecl->typeList) {
    setTypeList(normalDecl->typeList);
  }

  if (normalDecl->body) {
    setInterfaceBody(normalDecl->body);
  }
}

void EmacsOutput::setOp(unsigned int ini, int len) {
  ini++;
  unsigned int end = ini + len;
  outSH += "(djp-node-op " + itos(ini) + " " + itos(end) + ")";
}

void EmacsOutput::setPackageDeclaration(const spPackageDeclaration &pkgDecl) {
  outSH += "(djp-package-declaration ";
  setAnnotations(pkgDecl->annotations);

  // package keyword
  setKeyword(pkgDecl->pkgTokPos + 1,
    pkgDecl->pkgTokPos + tokenUtil.getTokenLength(TOK_KEY_PACKAGE) + 1);

  // package qualified identifier
  if (pkgDecl->qualifiedId) {
    setQualifiedId(pkgDecl->qualifiedId);
  }

  outSH += ")";
}

void EmacsOutput::setParExpression(const spParExpression &parExpr) {
  if (parExpr->posLParen) { setOp(parExpr->posLParen); }
  if (parExpr->expr) { setExpression(parExpr->expr); }
  if (parExpr->posRParen) { setOp(parExpr->posRParen); }
}

void EmacsOutput::setPrimary(const spPrimary &primary) {
  if (primary->opt == Primary::OPT_LITERAL) {
    if (primary->literal) {
      setLiteral(primary->literal);
    }
    return;
  }

  if (primary->opt == Primary::OPT_PAR_EXPRESSION
    && primary->parExpr) {
    setParExpression(primary->parExpr);
    return;
  }

  if (primary->opt == Primary::OPT_THIS_ARGUMENTS
    && primary->thisArgs) {

    if (primary->thisArgs->tokThis) {
      setKeyword(primary->thisArgs->tokThis);
    }

    if (primary->thisArgs->args) {
      setArguments(primary->thisArgs->args);
    }

    return;
  }

  if (primary->opt == Primary::OPT_SUPER_SUPER_SUFFIX
    && primary->superSuperSuffix) {
    if (primary->superSuperSuffix->tokSuper) {
      setKeyword(primary->superSuperSuffix->tokSuper);
    }

    if (primary->superSuperSuffix->superSuffix) {
      setSuperSuffix(primary->superSuperSuffix->superSuffix);
    }

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

  if (primary->opt == Primary::OPT_NON_WILDCARD_TYPE_ARGUMENTS
    && primary->nonWildcardTypeArguments) {

    if (primary->nonWildcardTypeArguments->nonWildcardTypeArguments) {
      setNonWildcardTypeArguments(
        primary->nonWildcardTypeArguments->nonWildcardTypeArguments);
    }

    if (primary->nonWildcardTypeArguments->explGen) {
      setExplicitGenericInvocationSuffix(
        primary->nonWildcardTypeArguments->explGen);
    }

    if (primary->nonWildcardTypeArguments->tokThis) {
      setKeyword(primary->nonWildcardTypeArguments->tokThis);
    }

    if (primary->nonWildcardTypeArguments->args) {
      setArguments(primary->nonWildcardTypeArguments->args);
    }

    return;
  }

  if (primary->opt == Primary::OPT_IDENTIFIER) {
    if (primary->primaryId) {
      setPrimaryIdentifier(primary->primaryId);
    }
    return;
  }

  if (primary->opt == Primary::OPT_BASIC_TYPE && primary->primaryBasicType) {
    if (primary->primaryBasicType->basicType) {
      setBasicType(primary->primaryBasicType->basicType);
    }

    setArrayDepth(primary->primaryBasicType->arrayDepth);

    if (primary->primaryBasicType->posPeriod) {
      setOp(primary->primaryBasicType->posPeriod);
    }

    if (primary->primaryBasicType->tokClass) {
      setKeyword(primary->primaryBasicType->tokClass);
    }

    return;
  }

  if (primary->opt == Primary::OPT_VOID_CLASS && primary->primaryVoidClass) {
    if (primary->primaryVoidClass->tokVoid) {
      setKeyword(primary->primaryVoidClass->tokVoid);
    }

    if (primary->primaryVoidClass->posPeriod) {
      setOp(primary->primaryVoidClass->posPeriod);
    }

    if (primary->primaryVoidClass->tokClass) {
      setKeyword(primary->primaryVoidClass->tokClass);
    }

    return;
  }
}

void EmacsOutput::setPrimaryIdentifier(const spPrimaryIdentifier &primaryId) {
  for (unsigned int i = 0; i < primaryId->ids.size(); i++) {
    setIdentifier(primaryId->ids[i]);
  }

  if (primaryId->idSuffix) {
    setIdentifierSuffix(primaryId->idSuffix);
  }
}

void EmacsOutput::setQualifiedId(const spQualifiedIdentifier &qualifiedId) {
  if (qualifiedId->id) {
    setIdentifier(qualifiedId->id);
  }

  for (unsigned i = 0; i < qualifiedId->pairs.size(); i++) {
    setOp(qualifiedId->pairs[i].first);
    setIdentifier(qualifiedId->pairs[i].second);
  }
}

void EmacsOutput::setQualifiedIdentifierList(
  const spQualifiedIdentifierList &qualifiedIdList) {

  if (qualifiedIdList->qualifiedId) {
    setQualifiedId(qualifiedIdList->qualifiedId);
  }

  for (unsigned i = 0; i < qualifiedIdList->pairs.size(); i++) {
    setOp(qualifiedIdList->pairs[i].first);
    setQualifiedId(qualifiedIdList->pairs[i].second);
  }
}

void EmacsOutput::setReferenceType(const spReferenceType &refType) {
  if (refType->id) {
    setIdentifier(refType->id, EmacsOutput::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  if (refType->typeArgs) {
    setTypeArguments(refType->typeArgs);
  }

  for (unsigned i = 0; i < refType->triplets.size(); i++) {
    setOp(refType->triplets[i]->posPeriod);

    setIdentifier(refType->triplets[i]->id,
      EmacsOutput::OPT_IDENTIFIER_REFERENCE_TYPE);

    if (refType->triplets[i]->typeArgs) {
      setTypeArguments(refType->triplets[i]->typeArgs);
    }
  }
}

void EmacsOutput::setResource(const spResource &res) {
  if (res->varModifier) {
    setVariableModifier(res->varModifier);
  }

  if (res->refType) {
    setReferenceType(res->refType);
  }

  if (res->varDeclId) {
    setVariableDeclaratorId(res->varDeclId);
  }

  if (res->posEquals) {
    setOp(res->posEquals);
  }

  if (res->expr) {
    setExpression(res->expr);
  }
}

void EmacsOutput::setResources(const spResources &resources) {
  if (resources->res) {
    setResource(resources->res);
  }

  for (unsigned i = 0; i < resources->pairs.size(); i++) {
    setOp(resources->pairs[i].first);
    setResource(resources->pairs[i].second);
  }
}

void EmacsOutput::setResourceSpecification(const spResourceSpecification &resSpec) {
  if (resSpec->posLParen) {
    setOp(resSpec->posLParen);
  }

  if (resSpec->resources) {
    setResources(resSpec->resources);
  }

  if (resSpec->posSemiColon) {
    setOp(resSpec->posSemiColon);
  }

  if (resSpec->posRParen) {
    setOp(resSpec->posRParen);
  }
}

void EmacsOutput::setSelector(const spSelector &selector) {
  if (selector->opt == Selector::OPT_IDENTIFIER_ARGUMENTS) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    if (selector->id) { setIdentifier(selector->id); }
    if (selector->args) { setArguments(selector->args); }
  }

  if (selector->opt == Selector::OPT_EXPLICIT_GENERIC_INVOCATION) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    if (selector->explGenInvocation) {
      setExplicitGenericInvocation(selector->explGenInvocation);
    }
  }

  if (selector->opt == Selector::OPT_THIS) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    if (selector->tokThis) { setKeyword(selector->tokThis); }
  }

  if (selector->opt == Selector::OPT_SUPER_SUPER_SUFFIX) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    if (selector->tokSuper) { setKeyword(selector->tokSuper); }
    if (selector->superSuffix) { setSuperSuffix(selector->superSuffix); }
  }

  if (selector->opt == Selector::OPT_NEW) {
    if (selector->posPeriod) { setOp(selector->posPeriod); }
    if (selector->tokNew) { setKeyword(selector->tokNew); }
    if (selector->nonWildcardTypeArguments) {
      setNonWildcardTypeArguments(selector->nonWildcardTypeArguments);
    }
    if (selector->innerCreator) {
      setInnerCreator(selector->innerCreator);
    }
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

void EmacsOutput::setStatement(const spStatement &stmt) {
  if (stmt->opt == Statement::OPT_BLOCK) {
    if (stmt->block) { setBlock(stmt->block); }
    return;
  }

  if (stmt->opt == Statement::OPT_SEMI_COLON) {
    if (stmt->posSemiColon) { setOp(stmt->posSemiColon); }
    return;
  }

  if (stmt->opt == Statement::OPT_ID_STMT) {
    if (stmt->id) { setIdentifier(stmt->id); }
    if (stmt->posColon) { setOp(stmt->posColon); }
    if (stmt->stmt) { setStatement(stmt->stmt); }
    return;
  }

  if (stmt->opt == Statement::OPT_STMT_EXPR) {
    if (stmt->stmtExpr) { setStatementExpression(stmt->stmtExpr); }
    if (stmt->posSemiColon) { setOp(stmt->posSemiColon); }
    return;
  }

  if (stmt->opt == Statement::OPT_IF) {
    if (stmt->tokIf) { setKeyword(stmt->tokIf); }
    if (stmt->parExpr) { setParExpression(stmt->parExpr); }
    if (stmt->stmtIf) { setStatement(stmt->stmtIf); }
    if (stmt->tokElse) { setKeyword(stmt->tokElse); }
    if (stmt->stmtElse) { setStatement(stmt->stmtElse); }
    return;
  }

  if (stmt->opt == Statement::OPT_ASSERT) {
    if (stmt->tokAssert) { setKeyword(stmt->tokAssert); }
    if (stmt->exprAssert1) { setExpression(stmt->exprAssert1); }
    if (stmt->posColon) { setOp(stmt->posColon); }
    if (stmt->exprAssert2) { setExpression(stmt->exprAssert2); }
    if (stmt->posSemiColon) { setOp(stmt->posSemiColon); }
    return;
  }

  if (stmt->opt == Statement::OPT_SWITCH) {
    if (stmt->tokSwitch) {
      setKeyword(stmt->tokSwitch);
    }

    if (stmt->parExpr) {
      setParExpression(stmt->parExpr);
    }

    if (stmt->posLCBrace) {
      setOp(stmt->posLCBrace);
    }

    if (stmt->switchStmtGroups) {
      for (unsigned i = 0; i < stmt->switchStmtGroups->groups.size(); i++) {
        setSwitchBlockStatementGroup(stmt->switchStmtGroups->groups[i]);
      }
    }

    if (stmt->posRCBrace) {
      setOp(stmt->posRCBrace);
    }

    return;
  }

  if (stmt->opt == Statement::OPT_WHILE) {
    if (stmt->tokWhile) { setKeyword(stmt->tokWhile); }
    if (stmt->parExpr) { setParExpression(stmt->parExpr); }
    if (stmt->stmtWhile) { setStatement(stmt->stmtWhile); }
    return;
  }

  if (stmt->opt == Statement::OPT_DO) {
    if (stmt->tokDo) { setKeyword(stmt->tokDo); }
    if (stmt->stmtDo) { setStatement(stmt->stmtDo); }
    if (stmt->tokWhile) { setKeyword(stmt->tokWhile); }
    if (stmt->parExpr) { setParExpression(stmt->parExpr); }
    if (stmt->posSemiColon) { setOp(stmt->posSemiColon); }

    return;
  }

  if (stmt->opt == Statement::OPT_FOR) {
    if (stmt->tokFor) { setKeyword(stmt->tokFor); }
    if (stmt->posLParen) { setOp(stmt->posLParen); }
    if (stmt->forCtrl) { setForControl(stmt->forCtrl); }
    if (stmt->posRParen) { setOp(stmt->posRParen); }
    if (stmt->stmtFor) { setStatement(stmt->stmtFor); }

    return;
  }

  if (stmt->opt == Statement::OPT_BREAK) {
    if (stmt->tokBreak) { setKeyword(stmt->tokBreak); }
    if (stmt->id) { setIdentifier(stmt->id); }
    if (stmt->posSemiColon) { setOp(stmt->posSemiColon); }
    return;
  }

  if (stmt->opt == Statement::OPT_CONTINUE) {
    if (stmt->tokContinue) { setKeyword(stmt->tokContinue); }
    if (stmt->id) { setIdentifier(stmt->id); }
    if (stmt->posSemiColon) { setOp(stmt->posSemiColon); }
    return;
  }

  if (stmt->opt == Statement::OPT_RETURN) {
    if (stmt->tokReturn) { setKeyword(stmt->tokReturn); }
    if (stmt->exprReturn) { setExpression(stmt->exprReturn); }
    if (stmt->posSemiColon) { setOp(stmt->posSemiColon); }
    return;
  }

  if (stmt->opt == Statement::OPT_THROW) {
    if (stmt->tokThrow) { setKeyword(stmt->tokThrow); }
    if (stmt->throwExpr) { setExpression(stmt->throwExpr); }
    if (stmt->posSemiColon) { setOp(stmt->posSemiColon); }

    return;
  }

  if (stmt->opt == Statement::OPT_SYNC) {
    if (stmt->tokSync) { setKeyword(stmt->tokSync); }
    if (stmt->parExpr) { setParExpression(stmt->parExpr); }
    if (stmt->block) { setBlock(stmt->block); }
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
    if (stmt->tokTry) { setKeyword(stmt->tokTry); }
    if (stmt->resSpec) { setResourceSpecification(stmt->resSpec); }
    if (stmt->block) { setBlock(stmt->block); }
    if (stmt->catches) { setCatches(stmt->catches); }
    if (stmt->finally) { setFinally(stmt->finally); }
    return;
  }
}

void EmacsOutput::setStatementExpression(const spStatementExpression &stmtExpr) {
  if (stmtExpr->expr) {
    setExpression(stmtExpr->expr);
  }
}

void EmacsOutput::setStringLiteral(const spStringLiteral &strLiteral) {
  unsigned ini = strLiteral->pos + 1;
  unsigned end = ini + strLiteral->val.length();
  outSH += "(djp-node-string-literal "
    + itos(ini) + " " + itos(end) + ")";
}

void EmacsOutput::setSuperSuffix(const spSuperSuffix &superSuffix) {

  if (superSuffix->opt == SuperSuffix::OPT_ARGUMENTS) {
    if (superSuffix->args) {
      setArguments(superSuffix->args);
    }
    return;
  }

  if (superSuffix->opt == SuperSuffix::OPT_IDENTIFIER_ARGUMENTS) {
    if (superSuffix->posPeriod) {
      setOp(superSuffix->posPeriod);
    }

    if (superSuffix->id) {
      setIdentifier(superSuffix->id);
    }

    if (superSuffix->args) {
      setArguments(superSuffix->args);
    }
  }
}

void EmacsOutput::setSwitchBlockStatementGroup(
  const spSwitchBlockStatementGroup &group) {

  if (group->labels) {
    setSwitchLabels(group->labels);
  }

  for (unsigned i = 0; i < group->blockStmts.size(); i++) {
    setBlockStatement(group->blockStmts[i]);
  }
}

void EmacsOutput::setSwitchLabel(const spSwitchLabel &label) {
  if (label->opt == SwitchLabel::OPT_EXPRESSION) {
    if (label->tokCase) { setKeyword(label->tokCase); }
    if (label->expr) { setExpression(label->expr); }
    if (label->posColon) { setOp(label->posColon); }
    return;
  }

  if (label->opt == SwitchLabel::OPT_ENUM) {
    if (label->tokCase) { setKeyword(label->tokCase); }

    if (label->enumConstName && label->enumConstName->id) {
      setIdentifier(label->enumConstName->id);
    }

    if (label->posColon) { setOp(label->posColon); }

    return;
  }

  if (label->opt == SwitchLabel::OPT_DEFAULT) {
    if (label->tokCase) { setKeyword(label->tokCase); }
    if (label->posColon) { setOp(label->posColon); }
  }
}

void EmacsOutput::setSwitchLabels(const spSwitchLabels &labels) {
  if (labels->label) {
    setSwitchLabel(labels->label);
  }

  for (unsigned i = 0; i < labels->labels.size(); i++) {
    setSwitchLabel(labels->labels[i]);
  }
}

void EmacsOutput::setType(const spType &type) {
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

void EmacsOutput::setTypeArgument(const spTypeArgument &typeArg) {
  if (typeArg->opt == TypeArgument::OPT_TYPE) {
    if (typeArg->type) { setType(typeArg->type); }
  }

  if (typeArg->opt == TypeArgument::OPT_QUESTION_MARK) {
    if (!typeArg->opt2) { return; }

    setOp(typeArg->opt2->posQuestionMark, 1);

    if (typeArg->opt2->tokExtendsOrSuper) {
      setKeyword(typeArg->opt2->tokExtendsOrSuper);
    }

    if (typeArg->opt2->type) {
      setType(typeArg->opt2->type);
    }
  }
}

void EmacsOutput::setTypeArguments(const spTypeArguments &typeArgs) {
  if (typeArgs->posLt) { setOp(typeArgs->posLt, 1); }
  if (typeArgs->posGt) { setOp(typeArgs->posGt, 1); }
  if (typeArgs->typeArg) { setTypeArgument(typeArgs->typeArg); }
  for (unsigned int i = 0; i < typeArgs->typeArgs.size(); i++) {
    setTypeArgument(typeArgs->typeArgs[i]);
  }
}

void EmacsOutput::setTypeDeclarations(
  const std::vector<spTypeDeclaration> &typeDecls) {

  for (std::size_t i = 0; i < typeDecls.size(); i++) {
    if (typeDecls[i]->classOrIntDecl) {
      setClassOrInterfaceDeclaration(typeDecls[i]->classOrIntDecl);
    }
  }
}

void EmacsOutput::setTypeList(const spTypeList &typeList) {
  if (typeList->refType) {
    setReferenceType(typeList->refType);
  }

  for (unsigned i = 0; i < typeList->refTypes.size(); i++) {
    setReferenceType(typeList->refTypes[i]);
  }
}

void EmacsOutput::setTypeList2(const spTypeList2 &typeList2) {
  if (typeList2->type) {
    setType(typeList2->type);
  }

  for (unsigned int i = 0; i < typeList2->pairs.size(); i++) {
    setOp(typeList2->pairs[i].first);
    setType(typeList2->pairs[i].second);
  }
}

void EmacsOutput::setTypeParameter(const spTypeParameter &typeParam) {
  if (typeParam->id) {
    setIdentifier(typeParam->id,
      EmacsOutput::OPT_IDENTIFIER_REFERENCE_TYPE);
  }

  if (typeParam->tokExtends) {
    setKeyword(typeParam->tokExtends);
  }

  if (typeParam->bound) {
    setBound(typeParam->bound);
  }
}

void EmacsOutput::setTypeParameters(const spTypeParameters &typeParams) {
  if (typeParams->posLt) {
    setOp(typeParams->posLt);
  }

  if (typeParams->typeParam) {
    setTypeParameter(typeParams->typeParam);
  }

  for (unsigned i = 0; i < typeParams->pairs.size(); i++) {
    setOp(typeParams->pairs[i].first);
    setTypeParameter(typeParams->pairs[i].second);
  }

  if (typeParams->posGt) {
    setOp(typeParams->posGt);
  }
}

void EmacsOutput::setTypeArgumentsOrDiamond(
  const spTypeArgumentsOrDiamond &typeArgsOrDiam) {

  if (typeArgsOrDiam->opt == TypeArgumentsOrDiamond::OPT_DIAMOND) {
    if (typeArgsOrDiam->posLt) {
      setOp(typeArgsOrDiam->posLt);
    }

    if (typeArgsOrDiam->posGt) {
      setOp(typeArgsOrDiam->posGt);
    }

    return;
  }

  if (typeArgsOrDiam->opt == TypeArgumentsOrDiamond::OPT_TYPE_ARGUMENTS) {
    if (typeArgsOrDiam->typeArgs) {
      setTypeArguments(typeArgsOrDiam->typeArgs);
    }
  }
}

void EmacsOutput::setVariableDeclarator(const spVariableDeclarator &varDecl) {
  if (varDecl->id) {
    setIdentifier(varDecl->id);
  }

  if (varDecl->varDeclRest) {
    setVariableDeclaratorRest(varDecl->varDeclRest);
  }
}

void EmacsOutput::setVariableDeclaratorId(
  const spVariableDeclaratorId &varDeclId) {
  if (varDeclId->id) {
    setIdentifier(varDeclId->id);
  }
}

void EmacsOutput::setVariableDeclaratorRest(
  const spVariableDeclaratorRest &varDeclRest) {

  setArrayDepth(varDeclRest->arrayDepth);

  if (varDeclRest->posEquals) {
    setOp(varDeclRest->posEquals);
  }

  if (varDeclRest->varInit) {
    setVariableInitializer(varDeclRest->varInit);
  }
}

void EmacsOutput::setVariableDeclarators(const spVariableDeclarators &varDecls) {
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

void EmacsOutput::setVariableInitializer(const spVariableInitializer &varInit) {
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

void EmacsOutput::setVariableModifier(const spVariableModifier &varModifier) {
  if (varModifier->tokFinal) {
    setKeyword(varModifier->tokFinal);
  }

  setAnnotations(varModifier->annotations);
}

void EmacsOutput::setVoidInterfaceMethodDeclaratorRest(
  const spVoidInterfaceMethodDeclaratorRest &voidMethDeclRest) {

  if (voidMethDeclRest->formParams) {
    setFormalParameters(voidMethDeclRest->formParams);
  }

  if (voidMethDeclRest->tokThrows) {
    setKeyword(voidMethDeclRest->tokThrows);
  }

  if (voidMethDeclRest->qualifiedIdList) {
    setQualifiedIdentifierList(voidMethDeclRest->qualifiedIdList);
  }

  if (voidMethDeclRest->posSemiColon) {
    setOp(voidMethDeclRest->posSemiColon);
  }
}

void EmacsOutput::setVoidMethodDeclaratorRest(
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
const std::string EmacsOutput::itos(int i) {
  std::stringstream s;
  s << i;
  return s.str();
}

} // namespace
