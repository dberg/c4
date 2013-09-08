#include "JHelloWorldTest.cpp"
#include "SHelloWorldTest.cpp"

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
