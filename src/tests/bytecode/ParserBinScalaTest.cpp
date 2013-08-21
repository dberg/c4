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

  ASSERT_EQ(0xCAFEBABE, parser.classFile->magic);
  ASSERT_EQ(0, parser.classFile->minor_version);
  ASSERT_EQ(50, parser.classFile->major_version);
  ASSERT_EQ(45, parser.classFile->constant_pool_count);
  ASSERT_EQ(45, parser.classFile->constant_pool->items.size());

  {
    // Item 1: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[1];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(10, item->cUtf8Info->length);
    ASSERT_EQ(10, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("HelloWorld", str);
  }

  {
    // Item 2: CONSTANT_Class HelloWorld
    spCPItem item = parser.classFile->constant_pool->items[2];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    ASSERT_EQ(1, cClassInfo->name_index);
  }

  {
    // Item 3: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[3];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(16, item->cUtf8Info->length);
    ASSERT_EQ(16, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("java/lang/Object", str);
  }

  {
    // Item 4: CONSTANT_Class java/lang/Object
    spCPItem item = parser.classFile->constant_pool->items[4];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    ASSERT_EQ(3, cClassInfo->name_index);
  }

  // TODO: HelloWorld$.class
  // TODO: HelloWorld$delayedInit$body.class
}
