//-*- C++ -*-
#ifndef __MESSAGE_BUFFER_H__
#define __MESSAGE_BUFFER_H__

#include <memory>
#include <vector>
#include "c4/Message.h"

namespace c4 {

class MessageBuffer;
typedef std::shared_ptr<MessageBuffer> spMessageBuffer;

class MessageBuffer {

  unsigned long size;
  std::vector<char> bytes;

public:
  MessageBuffer(): size(0) {}
  int feed(char buffer[], int cbytes);
  spMessage buildAndRemoveMessage();
};

} // namespace

#endif
