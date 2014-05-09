//-*- C++ -*-
#ifndef __COMPILATION_UNIT_H__
#define __COMPILATION_UNIT_H__

#include <memory>
#include <string>

namespace c4 {

class CompilationUnit;
typedef std::shared_ptr<CompilationUnit> spCompilationUnit;

class CompilationUnit {
public:

  const std::string filename;
  const std::string buffer;

  // The emacs output of this compilation.
  std::string output;

  CompilationUnit(std::string f, std::string b): filename(f), buffer(b) {}
};

}

#endif
