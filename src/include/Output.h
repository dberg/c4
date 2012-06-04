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

  void setAnnotations(std::vector<spAnnotation> &annotations);
  void setClassOrInterfaceDeclaration(spClassOrInterfaceDeclaration &decl);
  void setErrors(std::vector<spError> &errors);
  void setIdentifier(spIdentifier &identifier);
  void setImportDeclarations(spImportDeclarations &impDecls);
  void setImportDeclaration(spImportDeclaration &import);
  void setKeyword(int ini, int end);
  void setKeyword(spTokenExp &token);
  void setModifier(spModifier &modifier);
  void setNormalClassDeclaration(spNormalClassDeclaration &nClassDecl);
  void setPackageDeclaration(spPackageDeclaration &pkgDecl);
  void setQualifiedId(int ini, int end);
  void setTypeDeclarations(std::vector<spTypeDeclaration> &typeDecls);

  // Helper methods
  const std::string itos(int i);

public:
  std::string output;

  Output(spCompilationUnit _compilationUnit)
    : compilationUnit(_compilationUnit) {}

  void build();
};

}

#endif
