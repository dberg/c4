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
  spDiagnosis diag;
  spCompilationUnit compilationUnit;
  TokenUtil tokenUtil;
  ErrorUtil errUtil;

  void setAnnotations(const std::vector<spAnnotation> &annotations);
  void setArrayDepth(ArrayDepth &arrayDepth);
  void setBlock(const spBlock &block);
  void setClassBody(const spClassBody &classBody);
  void setClassBodyDeclaration(const spClassBodyDeclaration &decl);
  void setClassOrInterfaceDeclaration(
    const spClassOrInterfaceDeclaration &decl);
  void setConstructorDeclaratorRest(
    const spConstructorDeclaratorRest &constDeclRest);
  void setErrors(const std::vector<spError> &errors);
  void setFormalParameterDecls(const spFormalParameterDecls &formParamDecls);
  void setFormalParameterDeclsRest(
    const spFormalParameterDeclsRest &formParamDeclsRest);
  void setIdentifier(const spIdentifier &identifier);
  void setImportDeclaration(const spImportDeclaration &import);
  void setImportDeclarations(const spImportDeclarations &impDecls);
  void setKeyword(int ini, int end);
  void setKeyword(const spTokenExp &token);
  void setMemberDecl(const spMemberDecl &memberDecl);
  void setModifier(const spModifier &modifier);
  void setNormalClassDeclaration(const spNormalClassDeclaration &nClassDecl);
  void setOp(unsigned int ini, int len);
  void setPackageDeclaration(const spPackageDeclaration &pkgDecl);
  void setQualifiedId(int ini, int end);
  void setReferenceType(const spReferenceType &refType);
  void setType(const spType &type);
  void setTypeArgument(const spTypeArgument &typeArg);
  void setTypeArguments(const spTypeArguments &typeArgs);
  void setTypeDeclarations(const std::vector<spTypeDeclaration> &typeDecls);
  void setVariableDeclaratorId(const spVariableDeclaratorId &varDeclId);
  void setVariableModifier(const spVariableModifier &varModifier);

  // Helper methods
  const std::string itos(int i);

public:
  std::string output;

  Output(const spCompilationUnit &_compilationUnit, spDiagnosis &_diag)
    : diag(_diag), compilationUnit(_compilationUnit) {}

  void build();
};

}

#endif
