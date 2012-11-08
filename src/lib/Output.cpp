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

/// We return zero or more of the form:
/// (annotation INI ERR (qualified-id INI END))
/// where the annotation ini marks the position of the token '@' and we delimit
/// qualified-id as one element being the ini the first identifier and the end
/// the last identifier.
void Output::setAnnotations(
  const std::vector<spAnnotation> &annotations) {

  for (std::size_t i = 0; i < annotations.size(); i++) {
    int ini = 0; int end = 0;
    if (annotations[i]->qualifiedId) {
      ini = annotations[i]->qualifiedId->ini + 1;
      end = annotations[i]->qualifiedId->end + 2;
    }

    output += "(djp-node-annotation "
      + itos(annotations[i]->posTokAt + 1) + " "
      + itos((int) annotations[i]->err)
      + " (djp-node-qualified-id "
      + itos(ini) + " "
      + itos(end) + "))";
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

void Output::setArrayDepth(ArrayDepth &arrayDepth) {
  for (unsigned int i = 0; i < arrayDepth.size(); i++) {
    unsigned posOpen = arrayDepth[i].first;
    unsigned posClose = arrayDepth[i].second;
    setOp(posOpen, 1);
    setOp(posClose, 1);
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
    // TODO:
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

  // TODO:
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
    // TODO:
    //setExpression2Rest(expr2->expr3Rest);
  }
}

void Output::setExpression3(const spExpression3 &expr3) {
  // TODO:
  /*
  if (expr3->opt == Expression3::OPT_PREFIXOP_EXPRESSION3) {
    if (expr3->prefixOp) {
      setPrefixOp(expr3->prefixOp);
    }

    if (expr3->expr3) {
      setExpression3(expr3->expr3);
    }
    return;
  }
  */

  // TODO:
  /*
  if (expr3->opt == Expression3::OPT_EXPRESSION_TYPE_EXPRESSION3) {
    if (expr3->expr) {
      setExpression(expr3->expr);
    }

    if (expr3->type) {
      setType(expr3->type);
    }
    return;
  }
  */

  if (expr3->opt == Expression3::OPT_PRIMARY_SELECTOR_POSTFIXOP) {
    if (expr3->primary) {
      setPrimary(expr3->primary);
    }

    // TODO:
    /*
    if (expr3->selector) {
      setSelector(expr3->selector);
    }

    if (expr3->postfixOp) {
      setPostfixOp(expr3->postfixOp)
    }
    */
  }
}

void Output::setFieldDeclsRest(const spFieldDeclaratorsRest &fieldDeclsRest) {
  if (fieldDeclsRest->varDeclRest) {
    setVariableDeclaratorRest(fieldDeclsRest->varDeclRest);
  }

  // TODO:
  //std::vector<std::pair<unsigned int, spVariableDeclarator> > pairsCommaVarDecl;
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

void Output::setIdentifier(const spIdentifier &identifier) {
  int ini = identifier->pos + 1;
  int end = ini + identifier->value.length();
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

void Output::setNormalClassDeclaration(
  const spNormalClassDeclaration &nClassDecl) {

  output += "(djp-normal-class-declaration ";

  if (nClassDecl->classTok) {
    setKeyword(nClassDecl->classTok);
  }

  if (nClassDecl->identifier) {
    setIdentifier(nClassDecl->identifier);
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
  unsigned int end = ini + len + 1;
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

void Output::setPrimary(const spPrimary &primary) {
  if (primary->opt == Primary::OPT_LITERAL) {
    // TODO:
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
    // TODO:
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
  if (refType->id) { setIdentifier(refType->id); }
  if (refType->typeArgs) { setTypeArguments(refType->typeArgs); }
  for (unsigned int i = 0; i < refType->refTypes.size(); i++) {
    setReferenceType(refType->refTypes[i]);
  }
}

void Output::setStatement(const spStatement &stmt) {
  if (stmt->opt == Statement::OPT_BLOCK) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_SEMI_COLON) {
    // TODO:
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
    // TODO:
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
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_THROW) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_SYNC) {
    // TODO:
    return;
  }

  if (stmt->opt == Statement::OPT_TRY_BLOCK) {
    // TODO:
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

void Output::setVariableInitializer(const spVariableInitializer &varInit) {
  if (varInit->opt == VariableInitializer::OPT_ARRAY_INITIALIZER) {
    if (varInit->arrayInit) {
      // TODO:
      //setArrayInitializer(varInit->arrayInit);
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
