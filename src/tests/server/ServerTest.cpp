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
  auto unit = request.add_compilationunits();
  unit->set_filename("A.java");
  std::string buffer =
    "public class A {\n"
    "  public static void main(String[] args) {\n"
    "  }\n"
    "}\n";
  unit->set_buffer(buffer);

  int written = request.SerializeToFileDescriptor(sockfd);
  if (written <= 0) {
    FAIL() << "Fail to write bytes to test server";
  }
}

// TODO: Stop server before exiting.
//       Signal test that server is ready or an error occurred.
int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  std::thread serverThread(serverStart);
  serverThread.detach();
  return RUN_ALL_TESTS();
}
