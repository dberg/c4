//-*- C++ -*-
#ifndef __PROJECT_H__
#define __PROJECT_H__

#include <memory>
#include <unordered_map>
#include "c4/common/CompilationUnit.h"

namespace c4 {

class Project;
typedef std::shared_ptr<Project> spProject;

class Project {

  std::string id;
  std::unordered_map<std::string, spCompilationUnit> units;

public:

  Project(std::string id): id(id) {}
  void compile(spCompilationUnit unit);

};

} // namespace

#endif
