//-*- C++ -*-
#ifndef __OUTPUT_H__
#define __OUTPUT_H__
#include "AST.h"
#include "Diagnosis.h"
#include "ErrorCodes.h"
#include "Token.h"
#include <iostream>
#include <sstream>

namespace djp {

/// Emacs output for syntax highlighing.
/// The first position in the buffer is 1.
class Output {
  spCompilationUnit compilationUnit;
  std::vector<spComment> comments;
  spDiagnosis diag;
  TokenUtil tokenUtil;
  ErrorUtil errUtil;

  enum IdentifierOpt {
    OPT_UNDEFINED,
    OPT_IDENTIFIER_REFERENCE_TYPE,
  };

  void setAnnotationElement(const spAnnotationElement elem);
  void setAnnotation(const spAnnotation &annotation);
  void setAnnotations(const std::vector<spAnnotation> &annotations);
  void setArguments(const spArguments &args);
  void setArrayCreatorRest(const spArrayCreatorRest &arrayCreatorRest);
  void setArrayDepth(ArrayDepth &arrayDepth);
  void setArrayInitializer(const spArrayInitializer arrayInit);
  void setBlock(const spBlock &block);
  void setBlockStatement(const spBlockStatement &blockStmt);
  void setCatches(const spCatches &catches);
  void setCatchClause(const spCatchClause &catchClause);
  void setCatchType(const spCatchType &catchType);
  void setClassBody(const spClassBody &classBody);
  void setClassBodyDeclaration(const spClassBodyDeclaration &decl);
  void setClassCreatorRest(const spClassCreatorRest &classCreatorRest);
  void setClassOrInterfaceDeclaration(
    const spClassOrInterfaceDeclaration &decl);
  void setComments();
  void setConstructorDeclaratorRest(
    const spConstructorDeclaratorRest &constDeclRest);
  void setCreatedName(const spCreatedName &createdName);
  void setCreator(const spCreator &creator);
  void setElementValue(const spElementValue &value);
  void setElementValuePair(const spElementValuePair &pair);
  void setErrors(const std::vector<spError> &errors);
  void setExpression(const spExpression &expr);
  void setExpression1(const spExpression1 &expr1);
  void setExpression2(const spExpression2 &expr2);
  void setExpression2Rest(const spExpression2Rest &expr2Rest);
  void setExpression3(const spExpression3 &expr3);
  void setFieldDeclsRest(const spFieldDeclaratorsRest &fieldDeclsRest);
  void setFinally(const spFinally &finally);
  void setForControl(const spForControl &forCtrl);
  void setForInit(const spForInit &forInit);
  void setForUpdate(const spForUpdate &forUpdate);
  void setForVarControl(const spForVarControl &varCtrl);
  void setForVarControlRest(const spForVarControlRest &forVarCtrlRest);
  void setForVariableDeclaratorsRest(
    const spForVariableDeclaratorsRest &forVarDeclsRest);
  void setFormalParameters(const spFormalParameters &formParams);
  void setFormalParameterDecls(const spFormalParameterDecls &formParamDecls);
  void setFormalParameterDeclsRest(
    const spFormalParameterDeclsRest &formParamDeclsRest);
  void setIdentifier(const spIdentifier &identifier,
    IdentifierOpt opt = Output::OPT_UNDEFINED);
  void setIdentifierSuffix(const spIdentifierSuffix &idSuffix);
  void setImportDeclaration(const spImportDeclaration &import);
  void setImportDeclarations(const spImportDeclarations &impDecls);
  void setKeyword(int ini, int end);
  void setKeyword(const spTokenExp &token);
  void setLiteral(const spLiteral &literal);
  void setLocalVariableDeclarationStatement(
    const spLocalVariableDeclarationStatement &localVar);
  void setMemberDecl(const spMemberDecl &memberDecl);
  void setMethodDeclaratorRest(const spMethodDeclaratorRest &methodDeclRest);
  void setMethodOrFieldDecl(const spMethodOrFieldDecl &methodOrFieldDecl);
  void setMethodOrFieldRest(const spMethodOrFieldRest &methodOrFieldRest);
  void setModifier(const spModifier &modifier);
  void setNonWildcardTypeArguments(
    const spNonWildcardTypeArguments &nonWildcardTypeArguments);
  void setNormalClassDeclaration(const spNormalClassDeclaration &nClassDecl);
  void setOp(unsigned int ini, int len = 1);
  void setPackageDeclaration(const spPackageDeclaration &pkgDecl);
  void setParExpression(const spParExpression &parExpr);
  void setPrimary(const spPrimary &primary);
  void setPrimaryIdentifier(const spPrimaryIdentifier &primaryId);
  void setQualifiedId(int ini, int end);
  void setReferenceType(const spReferenceType &refType);
  void setSelector(const spSelector &selector);
  void setStatement(const spStatement &stmt);
  void setStatementExpression(const spStatementExpression &stmtExpr);
  void setStringLiteral(const spStringLiteral &strLiteral);
  void setType(const spType &type);
  void setTypeArgument(const spTypeArgument &typeArg);
  void setTypeArguments(const spTypeArguments &typeArgs);
  void setTypeArgumentsOrDiamond(const spTypeArgumentsOrDiamond typeArgsOrDiam);
  void setTypeDeclarations(const std::vector<spTypeDeclaration> &typeDecls);
  void setVariableDeclarator(const spVariableDeclarator &varDecl);
  void setVariableDeclaratorId(const spVariableDeclaratorId &varDeclId);
  void setVariableDeclaratorRest(const spVariableDeclaratorRest &varDeclRest);
  void setVariableDeclarators(const spVariableDeclarators &varDecls);
  void setVariableInitializer(const spVariableInitializer &varInit);
  void setVariableModifier(const spVariableModifier &varModifier);
  void setVoidMethodDeclaratorRest(
    const spVoidMethodDeclaratorRest &voidMethDeclRest);

  // Helper methods
  const std::string itos(int i);

public:
  std::string output;

  Output(const spCompilationUnit &_compilationUnit,
    std::vector<spComment> &_comments, spDiagnosis &_diag)
    : compilationUnit(_compilationUnit), comments(_comments), diag(_diag) {}

  void build();
};

}

#endif
