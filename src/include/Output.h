//-*- C++ -*-
#ifndef __OUTPUT_H__
#define __OUTPUT_H__
#include "AST.h"
#include "ErrorCodes.h"
#include "Token.h"
#include <iostream>
#include <sstream>

namespace djp {

/// Emacs output for syntax highlighing.
/// The first position in the buffer is 1.
class Output {
  spCompilationUnit compilationUnit;
  TokenUtil tokenUtil;
  ErrorUtil errUtil;

  void setAnnotations(const std::vector<spAnnotation> &annotations);
  void setClassBody(const spClassBody &classBody);
  void setClassBodyDeclaration(const spClassBodyDeclaration &decl);
  void setClassOrInterfaceDeclaration(
    const spClassOrInterfaceDeclaration &decl);
  void setConstructorDeclaratorRest(
    const spConstructorDeclaratorRest &constDeclRest);
  void setErrors(const std::vector<spError> &errors);
  void setIdentifier(const spIdentifier &identifier);
  void setImportDeclarations(const spImportDeclarations &impDecls);
  void setImportDeclaration(const spImportDeclaration &import);
  void setKeyword(int ini, int end);
  void setKeyword(const spTokenExp &token);
  void setMemberDecl(const spMemberDecl &memberDecl);
  void setModifier(const spModifier &modifier);
  void setNormalClassDeclaration(const spNormalClassDeclaration &nClassDecl);
  void setPackageDeclaration(const spPackageDeclaration &pkgDecl);
  void setQualifiedId(int ini, int end);
  void setTypeDeclarations(const std::vector<spTypeDeclaration> &typeDecls);

  // Helper methods
  const std::string itos(int i);

public:
  std::string output;

  Output(const spCompilationUnit &_compilationUnit)
    : compilationUnit(_compilationUnit) {}

  void build();
};

}

#endif
