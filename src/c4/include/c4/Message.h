//-*- C++ -*-
#ifndef __MESSAGE_H__
#define __MESSAGE_H__

#include <vector>

namespace c4 {

class Message;
typedef std::shared_ptr<Message> spMessage;

class Message {
  int feed(std::vector<char> &bytes);
};

} // namespace

#endif
