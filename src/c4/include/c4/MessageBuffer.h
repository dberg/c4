//-*- C++ -*-
#ifndef __MESSAGE_BUFFER_H__
#define __MESSAGE_BUFFER_H__

#include <vector>

class MessageBuffer;
typedef std::shared_ptr<MessageBuffer> spMessageBuffer;

class MessageBuffer {
  MessageBuffer() {}
  int feed(std::vector<char> &bytes);
};

#endif
