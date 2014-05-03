//-*- C++ -*-
#ifndef __COMPILATION_UNIT__
#define __COMPILATION_UNIT__

#include <memory>
#include <string>

namespace c4 {

class CompilationUnit;
typedef std::shared_ptr<CompilationUnit> spCompilationUnit;

class CompilationUnit {
public:
  std::string filename;
  std::string buffer;

  CompilationUnit(std::string f, std::string b): filename(f), buffer(b) {}
};

}

#endif
