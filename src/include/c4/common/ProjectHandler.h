//-*- C++ -*-
#ifndef __C4_COMMON_PROJECT_HANDLER_H__
#define __C4_COMMON_PROJECT_HANDLER_H__

#include <memory>
#include <string>
#include <unordered_map>

#include "c4/common/CompilationRequest.h"
#include "c4/common/Project.h"
#include "c4/server/Response.h"
#include "c4/server/Request.h"
#include "utf8/utf8.h"

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
