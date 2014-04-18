//-*- C++ -*-
#ifndef __REQUEST_BUFFER_H__
#define __REQUEST_BUFFER_H__

#include <memory>
#include <vector>
#include "c4/Message.h"

namespace c4 {

class RequestBuffer;
typedef std::shared_ptr<RequestBuffer> spRequestBuffer;

class RequestBuffer {

  unsigned long size;
  std::vector<char> bytes;

public:
  RequestBuffer(): size(0) {}
  int feed(char buffer[], int cbytes);
  spMessage buildAndRemoveMessage();
};

} // namespace

#endif
