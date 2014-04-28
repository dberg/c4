//-*- C++ -*-
#ifndef __PROJECT_HANDLER_H__
#define __PROJECT_HANDLER_H__

#include <memory>
#include <string>
#include <unordered_map>
#include "c4/common/Project.h"

namespace c4 {

class ProjectHandler;
typedef std::unique_ptr<ProjectHandler> spProjectHandler;

class ProjectHandler {

private:
  std::unordered_map<std::string, spProject> projects;

public:
  ProjectHandler(): projects() {}
};

} // namespace

#endif
