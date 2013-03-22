#include "djp/File.h"
#include "djp/ParserBin.h"
#include "gtest/gtest.h"
#include <libgen.h>
#include <string>
#include <vector>
using namespace djp;

std::string current_dir = dirname(const_cast<char *>(__FILE__));

/// public class HelloWorld {
///     public static void main(String[] args) {
///         System.out.println("Oi mundo");
///     }
/// }
TEST(ParserBin, HelloWorld) {
  std::vector<unsigned char> buffer;
  File file;
  std::string filename =  current_dir + "/../classes/HelloWorld.class";
  ASSERT_EQ(file.read(filename, buffer), 0);
  ParserBin parser(filename, buffer);
  parser.parse();

  ASSERT_EQ(0xCAFEBABE, parser.classFile->magic);
  spCPItem cpItem = parser.classFile->constant_pool->items[0];
  ASSERT_EQ(CONSTANT_Methodref, cpItem->tag);
}
