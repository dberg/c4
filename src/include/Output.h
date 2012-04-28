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
  std::string output;

  void setAnnotations(std::vector<spAnnotation> &annotations);
  void setPackageDeclaration(spPackageDeclaration &pkgDecl);

public:
  Output(spCompilationUnit _compilationUnit)
    : compilationUnit(_compilationUnit) {}

  void print();
};

}

#endif
