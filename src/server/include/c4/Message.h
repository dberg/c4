//-*- C++ -*-
#ifndef __MESSAGE_H__
#define __MESSAGE_H__

namespace c4 {

enum class MessageError {
  NONE,
  INCOMPLETE,
  MAL_FORMED,
  INVALID_CLIENT,
};

class Message;
typedef std::shared_ptr<Message> spMessage;

class Message {

  std::vector<char> body;

private:
  MessageError error;

public:
  Message(): error(MessageError::NONE) {}
  Message(MessageError error): error(error) {}
  Message(std::vector<char> body_): body(body_), error(MessageError::NONE) {}
};

} // namespace

#endif
