#include "c4/common/ProjectHandler.h"

namespace c4 {

/**
 * Process a request coming from the client and update the response object with
 * the results of the request.
 */
void ProjectHandler::process(spRequest &request, spResponse &response) {
  // Message error
  if (!request->IsInitialized()) {
    response->set_code(Response::ERROR);
    response->set_body("Invalid message");
    return;
  }

  u32string projectId = utf8_to_u32(request->projectid());
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

    u32string filename = utf8_to_u32(unit.filename());
    u32string buffer = utf8_to_u32(unit.buffer());

    spCompilation comp = make_shared<Compilation>(filename, buffer);
    project->compile(comp);

    response->set_code(Response::OK_COMPILE);
    string output = u32_to_utf8(comp->output);
    response->set_body(output);
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
spProject ProjectHandler::getProject(u32string projectId) {
  auto it = projects.find(projectId);
  if (it == projects.end()) {
    spProject project = make_shared<Project>(projectId);
    projects[projectId] = project;
    return project;
  }

  return it->second;
}

} // namespace
