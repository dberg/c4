//-*- C++ -*-
#ifndef __C4_COMMON_PROJECT_JAVA_H__
#define __C4_COMMON_PROJECT_JAVA_H__

#include <memory>
#include "c4/common/CompilationRequest.h"
#include "c4/java/EmacsOutput.h"
#include "c4/java/Parser.h"

namespace c4 {

class ProjectJava;
typedef std::shared_ptr<ProjectJava> spProjectJava;

class ProjectJava {

public:
  ProjectJava() {}
  void compile(spCompilationRequest compReq);
};

} // namespace

#endif
