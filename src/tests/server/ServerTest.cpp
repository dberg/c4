#include <iostream>
#include <thread>
#include "gtest/gtest.h"
#include "c4/Server.h"

void startServer() {
  c4::Server server;
  // TODO: read port number from configuration file
  server.start(8000);
}

TEST(Server, Connections) {
  // TODO:
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  std::thread serverThread(startServer);
  serverThread.detach();
  return RUN_ALL_TESTS();
}
