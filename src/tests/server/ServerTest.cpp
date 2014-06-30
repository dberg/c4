#include <arpa/inet.h>  // inet_pton
#include <iostream>
#include <netinet/in.h> // sockaddr_in
#include <strings.h>    // bzero on linux
#include <thread>

#include "gtest/gtest.h"
#include "c4/server/Server.h"

// TODO: read consts from configuration file
const char* TEST_SERVER_HOST = "127.0.0.1";
const int TEST_SERVER_PORT = 8000;

void serverStart() {
  c4::Server server;
  server.start(TEST_SERVER_PORT);
}

int serverConnect(std::string &errMsg) {
  // create socket
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    errMsg = "Could not create client socket";
    return -1;
  }

  struct sockaddr_in servaddr;
  bzero(&servaddr, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons(TEST_SERVER_PORT);
  if (inet_pton(AF_INET, TEST_SERVER_HOST, &servaddr.sin_addr) <= 0) {
    errMsg = "Invalid test server hostname";
    return -1;
  }

  // connect to server
  if (connect(sockfd, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0) {
    errMsg = "Fail to connect to test server";
    return -1;
  }

  return sockfd;
}

// TODO: we're assuming we get the entire response in one read call
int readResponse(int socket, std::string &response) {
  const int payloadSize = sizeof(uint32_t);
  const int bufferLen = 1024;
  char buffer[bufferLen];
  int cbytes = read(socket, buffer, bufferLen);
  if (cbytes > payloadSize) {
    response.append(buffer + payloadSize, cbytes - payloadSize);
    return 1;
  }
  return -1;
}

TEST(Server, Connections) {
  std::string errMsg = "";
  int sockfd = serverConnect(errMsg);
  if (sockfd <= 0) {
    FAIL() << errMsg;
    return;
  }

  // create compilation unit request
  c4::Request request;
  request.set_action(c4::Request::COMPILE);
  request.set_projectid("project-001");
  auto unit = request.mutable_unit();
  unit->set_filename("A.java");
  std::string buffer =
    "public class A {\n"
    "  public static void main(String[] args) {\n"
    "  }\n"
    "}\n";
  unit->set_buffer(buffer);

  // write request payload
  uint32_t byteSize = htonl(request.ByteSize());
  char *payload = reinterpret_cast<char*>(&byteSize);
  int written = write(sockfd, payload, sizeof(uint32_t));

  // 08 01
  // (field #1, wire type 0) COMPILE
  //
  // 12 0b 70726f6a6563742d303031
  // (field #2, wire type 2) (len) project-001
  //
  // 1a 4c
  // (field #3, wire type 2) (len)
  //
  // 0a 06 412e6a617661
  // (field #1, wire type 2) (len) A.java
  //
  // 12 42 707562
  // 6c69 6320 636c 6173 7320 4120 7b0a 2020
  // 7075 626c 6963 2073 7461 7469 6320 766f
  // 6964 206d 6169 6e28 5374 7269 6e67 5b5d
  // 2061 7267 7329 207b 0a20 207d 0a7d 0a
  // (field #2, wire type 2) (len) public ...
  written = request.SerializeToFileDescriptor(sockfd);
  if (written <= 0) {
    FAIL() << "Fail to write bytes to test server";
  }

  std::string responseStr;
  int result = readResponse(sockfd, responseStr);
  if (result <= 0) {
    FAIL() << "Failed to read data from server.";
  }

  c4::Response response;
  response.ParseFromString(responseStr);
  ASSERT_TRUE(response.IsInitialized());
}

// TODO: Stop server before exiting.
//       Signal test that server is ready or an error occurred.
int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  std::thread serverThread(serverStart);
  serverThread.detach();
  return RUN_ALL_TESTS();
}
