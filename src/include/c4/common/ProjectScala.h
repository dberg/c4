//-*- C++ -*-
#ifndef __PROJECT_SCALA_H__
#define __PROJECT_SCALA_H__

#include <memory>
#include "c4/common/CompilationUnit.h"

namespace c4 {

class ProjectScala;
typedef std::shared_ptr<ProjectScala> spProjectScala;

class ProjectScala {

public:
  ProjectScala() {}
  void compile(spCompilationUnit unit);
};

} // namespace

#endif
