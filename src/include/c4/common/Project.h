//-*- C++ -*-
#ifndef __C4_COMMON_PROJECT_H__
#define __C4_COMMON_PROJECT_H__

#include <memory>
#include <string>
#include <unordered_map>

#include "c4/common/CompilationRequest.h"
#include "c4/common/ProjectJava.h"
#include "c4/common/ProjectScala.h"
#include "c4/common/Util.h"

namespace c4 {

class Project;
typedef std::shared_ptr<Project> spProject;

class Project {

  std::string id;
  std::unordered_map<std::string, spCompilationRequest> compReqs;
  spProjectJava projJava;
  spProjectScala projScala;

public:

  Project(std::string id):
    id(id),
    projJava(std::shared_ptr<ProjectJava>(new ProjectJava)),
    projScala(std::shared_ptr<ProjectScala>(new ProjectScala))
    {}

  void compile(spCompilationRequest compReq);

};

} // namespace

#endif
