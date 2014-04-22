#include "c4/RequestBuffer.h"

namespace c4 {

/**
 * Append data to the RequestBuffer.
 * Return 1 if the Request is complete, 0 otherwise.
 */
int RequestBuffer::feed(char buffer[], int cbytes) {
  bytes.insert(bytes.end(), &buffer[0], &buffer[cbytes]);

  // if the message size is unknown set it if we have 4 bytes or more.
  if (size == 0) {
    calculateSize();
  }

  // Do we have a complete Request?
  if (bytes.size() >= size) { return 1; }
  return 0;
}

void RequestBuffer::calculateSize() {
  size = 0;
  if (bytes.size() >= 4) {
    size = (bytes[0] << 24)
      | (bytes[1] << 16)
      | (bytes[2] << 8)
      | bytes[3];
  }
}

Request RequestBuffer::buildAndRemoveRequest() {
  Request request;
  if (size == 0 || size < bytes.size()) {
    return request;
  }

  std::string reqStr(bytes.begin(), bytes.begin() + size);
  bytes.erase(bytes.begin(), bytes.begin() + size);
  request.ParseFromString(reqStr);
  calculateSize();
  return request;
}

} // namespace
