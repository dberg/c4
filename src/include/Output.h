//-*- C++ -*-
#ifndef __OUTPUT_H__
#define __OUTPUT_H__
#include "AST.h"
#include <iostream>
#include <sstream>

namespace djp {

/// Emacs output for syntax highlighing.
/// The first position in the buffer is 1.
class Output {
  spCompilationUnit compilationUnit;

  void setAnnotations(std::vector<spAnnotation> &annotations);
  void setPackageDeclaration(spPackageDeclaration &pkgDecl);
  void setImportDeclarations(spImportDeclarations &impDecls);
  void setImportDeclaration(spImportDeclaration &import);
  void setKeyword(int ini, int end);
  void setQualifiedId(int ini, int end);

public:
  std::string output;

  Output(spCompilationUnit _compilationUnit)
    : compilationUnit(_compilationUnit) {}

  void build();
};

}

#endif
