//-*- C++ -*-
#ifndef __C4_COMMON_PROJECT_HANDLER_H__
#define __C4_COMMON_PROJECT_HANDLER_H__

#include <memory>
#include <string>
#include <unordered_map>

#include "c4/common/Compilation.h"
#include "c4/common/Encode.h"
#include "c4/common/Project.h"
#include "c4/server/Response.h"
#include "c4/server/Request.h"

using std::string;
using std::u32string;
using std::unordered_map;
using std::make_shared;

namespace c4 {

class ProjectHandler;
typedef std::unique_ptr<ProjectHandler> spProjectHandler;

class ProjectHandler {

private:
  unordered_map<u32string, spProject> projects;
  spProject getProject(u32string projectId);

public:
  ProjectHandler(): projects() {}
  void process(spRequest &request, spResponse &response);
};

} // namespace

#endif
