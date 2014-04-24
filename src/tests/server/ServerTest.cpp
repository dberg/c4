#include <iostream>
#include <thread>
#include <arpa/inet.h>  // inet_pton
#include <strings.h>    // bzero on linux
#include <netinet/in.h> // sockaddr_in
#include "gtest/gtest.h"
#include "c4/server/Server.h"

// TODO: read consts from configuration file
const char* TEST_SERVER_HOST = "127.0.0.1";
const int TEST_SERVER_PORT = 8000;

void startServer() {
  c4::Server server;
  server.start(TEST_SERVER_PORT);
}

TEST(Server, Connections) {
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    FAIL() << "Could not create client socket";
  }

  struct sockaddr_in servaddr;
  bzero(&servaddr, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons(TEST_SERVER_PORT);
  if (inet_pton(AF_INET, TEST_SERVER_HOST, &servaddr.sin_addr) <= 0) {
    FAIL() << "Invalid test server hostname";
  }

  if (connect(sockfd, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0) {
    FAIL() << "Fail to connect to test server";
  }

  const char buff[] = "DUMMY VALUE";
  int written = write(sockfd, buff, strlen(buff));
  if (written <= 0) {
    FAIL() << "Fail to write bytes to test server";
  }
}

// TODO: stop server before exiting
int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  std::thread serverThread(startServer);
  serverThread.detach();
  return RUN_ALL_TESTS();
}
