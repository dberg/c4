//-*- C++ -*-
#ifndef __C4_COMMON_PROJECT_SCALA_H__
#define __C4_COMMON_PROJECT_SCALA_H__

#include <memory>
#include "c4/common/Compilation.h"

namespace c4 {

class ProjectScala;
typedef std::shared_ptr<ProjectScala> spProjectScala;

class ProjectScala {

public:
  ProjectScala() {}
  void compile(spCompilation comp);
};

} // namespace

#endif
