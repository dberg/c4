#include "c4/RequestBuffer.h"

#include <iostream>

namespace c4 {

/**
 * Append data to the RequestBuffer.
 * Return 1 if the Request is complete, 0 otherwise.
 */
int RequestBuffer::feed(char buffer[], int cbytes) {
  bytes.insert(bytes.end(), &buffer[0], &buffer[cbytes]);

  // if the message size is unknown set it if we have 4 bytes or more.
  if (size == 0 && bytes.size() >= 4) {
    size = (bytes[0] << 24)
      | (bytes[1] << 16)
      | (bytes[2] << 8)
      | bytes[3];
  }

  // Do we have a complete Request?
  if (bytes.size() >= size) { return 1; }
  return 0;
}

spMessage RequestBuffer::buildAndRemoveMessage() {
  if (size == 0 || size < bytes.size()) {
    return spMessage(new Message(MessageError::INCOMPLETE));
  }

  std::vector<char> msg(bytes.begin(), bytes.begin() + size);
  return spMessage(new Message(msg));
}

} // namespace
