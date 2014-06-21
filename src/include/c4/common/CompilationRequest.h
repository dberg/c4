//-*- C++ -*-
#ifndef __C4_COMMON_COMPILATION_REQUEST_H__
#define __C4_COMMON_COMPILATION_REQUEST_H__

#include <memory>
#include <string>

namespace c4 {

class CompilationRequest;
typedef std::shared_ptr<CompilationRequest> spCompilationRequest;

class CompilationRequest {
public:

  std::string filename;
  std::string buffer;

  CompilationRequest(std::string filename, std::string buffer)
    : filename(filename), buffer(buffer) {}
};

} // namespace

#endif
