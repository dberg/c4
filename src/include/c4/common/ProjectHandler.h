//-*- C++ -*-
#ifndef __PROJECT_HANDLER_H__
#define __PROJECT_HANDLER_H__

#include <memory>
#include <string>
#include <unordered_map>
#include "c4/common/CompilationUnit.h"
#include "c4/common/Project.h"
#include "c4/server/Response.h"
#include "c4/server/Request.h"

namespace c4 {

class ProjectHandler;
typedef std::unique_ptr<ProjectHandler> spProjectHandler;

class ProjectHandler {

private:
  std::unordered_map<std::string, spProject> projects;
  spProject getProject(std::string projectId);

public:
  ProjectHandler(): projects() {}
  void process(spRequest &request, spResponse &response);
};

} // namespace

#endif
