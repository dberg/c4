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
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    // TODO: confirm indexes 6 and 15
    ASSERT_EQ(6, cMethodrefInfo->class_index);
    ASSERT_EQ(15, cMethodrefInfo->name_and_type_index);
  }

  {
    // Item 1: CONSTANT_Fieldref
    spCPItem item = parser.classFile->constant_pool->items[1];
    ASSERT_EQ(CONSTANT_Fieldref, item->tag);
    spCFieldrefInfo cFieldrefInfo = item->cFieldrefInfo;
    // TODO: confirm indexes 16 and 17
    ASSERT_EQ(16, cFieldrefInfo->class_index);
    ASSERT_EQ(17, cFieldrefInfo->name_and_type_index);
  }

  {
    // Item 2: CONSTANT_String
    spCPItem item = parser.classFile->constant_pool->items[2];
    ASSERT_EQ(CONSTANT_String, item->tag);
    spCStringInfo cStringInfo = item->cStringInfo;
    // TODO: confirm index 18
    ASSERT_EQ(18, cStringInfo->string_index);
  }

  {
    // Item 3: CONSTANT_Methodref
    spCPItem item = parser.classFile->constant_pool->items[3];
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    // TODO: confirm indexes 19 and 20
    ASSERT_EQ(19, cMethodrefInfo->class_index);
    ASSERT_EQ(20, cMethodrefInfo->name_and_type_index);
  }

  {
    // Item 4: CONSTANT_Class
    spCPItem item = parser.classFile->constant_pool->items[4];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    // TODO: confirm index 21
    ASSERT_EQ(21, cClassInfo->name_index);
  }

  {
    // Item 5: CONSTANT_Class
    spCPItem item = parser.classFile->constant_pool->items[5];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    // TODO: confirm index 22
    ASSERT_EQ(22, cClassInfo->name_index);
  }

  {
    // Item 6: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[6];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(6, item->cUtf8Info->length);
    ASSERT_EQ(6, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("<init>", str);
  }

  {
    // Item 7: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[7];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(3, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("()V", str);
  }

  {
    // Item 8: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[8];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(4, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Code", str);
  }

  {
    // Item 9: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[9];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(15, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("LineNumberTable", str);
  }

  {
    // Item 10: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[10];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(4, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("main", str);
  }

  {
    // Item 11: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[11];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(22, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("([Ljava/lang/String;)V", str);
  }

  {
    // Item 12: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[12];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(10, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("SourceFile", str);
  }

  {
    // Item 13: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[13];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(15, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("HelloWorld.java", str);
  }

  {
    // Item 14: CONSTANT_Class
    spCPItem item = parser.classFile->constant_pool->items[14];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    // TODO: confirm index 8
    ASSERT_EQ(8, cClassInfo->name_index);
  }

  {
    // Item 15: CONSTANT_Class
    spCPItem item = parser.classFile->constant_pool->items[15];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    // TODO: confirm index 23
    ASSERT_EQ(23, cClassInfo->name_index);
  }

  {
    // Item 16: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[16];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(8, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Oi mundo", str);
  }

  {
    // Item 17: CONSTANT_Class
    spCPItem item = parser.classFile->constant_pool->items[17];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    // TODO: confirm index 26
    ASSERT_EQ(26, cClassInfo->name_index);
  }
}
