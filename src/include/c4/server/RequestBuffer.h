//-*- C++ -*-
#ifndef __C4_SERVER_REQUEST_BUFFER_H__
#define __C4_SERVER_REQUEST_BUFFER_H__

#include <memory>
#include <string>
#include <vector>
#include "c4/common/Log.h"
#include "c4/common/Util.h"
#include "c4/server/Request.h"

namespace c4 {

class RequestBuffer;
typedef std::shared_ptr<RequestBuffer> spRequestBuffer;

class RequestBuffer {

  uint32_t size;
  std::vector<char> bytes;

  void calculateSize();

public:
  RequestBuffer(): size(0) {}
  int feed(char buffer[], int cbytes);
  spRequest buildAndRemoveRequest();
};

} // namespace

#endif
