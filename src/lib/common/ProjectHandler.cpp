#include "c4/common/ProjectHandler.h"

namespace c4 {

/**
 * Process a request coming from the client.
 */
void ProjectHandler::process(spRequest &request, spResponse &response) {
  // Message error
  if (!request->IsInitialized()) {
    response->set_code(Response::ERROR);
    response->set_body("Invalid message");
    return;
  }

  std::string projectId = request->projectid();
  spProject project = getProject(projectId);

  // Project information request
  if (request->action() == Request::PROJECT) {
    response->set_code(Response::OK_PROJECT);
    // TODO: set body
    return;
  }

  // Compilation request
  if (request->action() == Request::COMPILE) {
    Request::CompilationUnit unit = request->unit();

    spCompilation comp = spCompilation(new Compilation(
      unit.filename(), unit.buffer()
    ));

    project->compile(comp);

    // TODO: get response
    response->set_code(Response::OK_COMPILE);
    response->set_body(comp->output);
    return;
  }

  // invalid action
  response->set_code(Response::ERROR);
  response->set_body("Invalid action");
  return;
}

/**
 * Look for the project defined by the projectId. If no project exists we create
 * one.
 */
spProject ProjectHandler::getProject(std::string projectId) {
  auto it = projects.find(projectId);
  if (it == projects.end()) {
    spProject project = std::shared_ptr<Project>(new Project(projectId));
    projects[projectId] = project;
    return project;
  }

  return it->second;
}

} // namespace
