#include "c4/server/ProjectHandler.h"

namespace c4 {

/**
 * Process a request coming from the client.
 */
Response ProjectHandler::process(Request request) {
  Response response;

  if (!request.IsInitialized()) {
    // TODO: error invalid request
    return response;
  }

  std::string projectId = request.projectid();
  spProject project = getProject(projectId);

  // get project information
  if (request.action() == Request::PROJECT) {
    // TODO:
    return response;
  }

  // compile list of compilation units
  if (request.action() == Request::COMPILE) {
    // TODO:
    return response;
  }

  // TODO: invalid action
  return response;
}

/**
 * Look for the project defined by the projectId. If no project exists we create
 * one.
 */
spProject ProjectHandler::getProject(std::string projectId) {
  auto it = projects.find(projectId);
  if (it == projects.end()) {
    spProject project = std::shared_ptr<Project>(new Project);
    projects[projectId] = project;
    return project;
  }

  return it->second;
}

}
