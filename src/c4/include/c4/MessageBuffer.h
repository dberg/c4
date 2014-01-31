//-*- C++ -*-
#ifndef __MESSAGE_BUFFER_H__
#define __MESSAGE_BUFFER_H__

#include <vector>

namespace c4 {

class MessageBuffer;
typedef std::shared_ptr<MessageBuffer> spMessageBuffer;

class MessageBuffer {

public:
  MessageBuffer() {}
  int feed(char bytes[], int cbytes);
};

} // namespace

#endif
