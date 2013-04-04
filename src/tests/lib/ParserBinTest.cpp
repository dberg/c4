#include "djp/File.h"
#include "djp/ParserBin.h"
#include "gtest/gtest.h"
#include <libgen.h>
#include <string>
#include <vector>
using namespace djp;

std::string getCurrentDir() {
  char *filename = strdup(__FILE__);
  std::string dir = dirname(filename);
  free(filename);
  return dir;
}

std::string current_dir = getCurrentDir();

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
  ASSERT_EQ(29, parser.classFile->constant_pool_count);

  {
    // Item 0: CONSTANT_Methodref
    spCPItem item = parser.classFile->constant_pool->items[0];
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    // TODO: confirm indexes 6 and 15
    ASSERT_EQ(6, cMethodrefInfo->class_index);
    ASSERT_EQ(15, cMethodrefInfo->name_and_type_index);
  }

  {
    // Item 1: CONSTANT_Fieldref
    spCPItem item = parser.classFile->constant_pool->items[1];
    spCFieldrefInfo cFieldrefInfo = item->cFieldrefInfo;
    ASSERT_EQ(CONSTANT_Fieldref, item->tag);
    // TODO: confirm indexes 16 and 17
    ASSERT_EQ(16, cFieldrefInfo->class_index);
    ASSERT_EQ(17, cFieldrefInfo->name_and_type_index);
  }

  {
    // Item 2: CONSTANT_String
    spCPItem item = parser.classFile->constant_pool->items[2];
    spCStringInfo cStringInfo = item->cStringInfo;
    ASSERT_EQ(CONSTANT_String, item->tag);
    // TODO: confirm index 18
    ASSERT_EQ(18, cStringInfo->string_index);
  }
}
