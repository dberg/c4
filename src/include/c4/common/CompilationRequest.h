//-*- C++ -*-
#ifndef __C4_COMMON_COMPILATION_REQUEST_H__
#define __C4_COMMON_COMPILATION_REQUEST_H__

#include <memory>
#include <string>
#include <vector>

#include "c4/common/TypeDefs.h"
#include "utf8/utf8.h"

namespace c4 {

class CompilationRequest;
typedef std::shared_ptr<CompilationRequest> spCompilationRequest;

class CompilationRequest {
public:

  std::string filename;
  // TODO: remove when the java parser uses Char
  std::string bufferStr;
  std::vector<Char> buffer;

  CompilationRequest(std::string filename, std::string bufferStr)
    : filename(filename), bufferStr(bufferStr) {

    utf8::utf8to16(bufferStr.begin(), bufferStr.end(),
      std::back_inserter(buffer));
  }
};

} // namespace

#endif
