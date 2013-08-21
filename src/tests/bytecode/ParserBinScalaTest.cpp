#include <string>
#include <vector>
#include "djp/File.h"
#include "djp/ParserBin.h"
#include "Util.h"
#include "gtest/gtest.h"
using namespace djp;

/**
 * object HelloWorld extends App { println("Hello world"); }
 */
TEST(ParserScalaBin, HelloWorld) {
  std::vector<unsigned char> buffer;
  File file;
  std::string filename =  current_dir
    + "/bytecode-classes/scala/HelloWorld/HelloWorld.class";
  ASSERT_EQ(file.read(filename, buffer), 0);
  ParserBin parser(filename, buffer);
  parser.parse();

  // TODO: HelloWorld$.class
  // TODO: HelloWorld$delayedInit$body.class
}
