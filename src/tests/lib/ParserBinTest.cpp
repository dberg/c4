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
  ASSERT_EQ(28, parser.classFile->constant_pool->items.size());

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
    // Item 14: CONSTANT_NameAndType
    spCPItem item = parser.classFile->constant_pool->items[14];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    // TODO: confirm index 7 and 8
    ASSERT_EQ(7, cNameAndTypeInfo->name_index);
    ASSERT_EQ(8, cNameAndTypeInfo->descriptor_index);
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
    // Item 16: CONSTANT_NameAndType
    spCPItem item = parser.classFile->constant_pool->items[16];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    ASSERT_EQ(24, cNameAndTypeInfo->name_index);
    ASSERT_EQ(25, cNameAndTypeInfo->descriptor_index);
  }

  {
    // Item 17: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[17];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(8, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Oi mundo", str);
  }

  {
    // Item 18: CONSTANT_Class
    spCPItem item = parser.classFile->constant_pool->items[18];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    // TODO: confirm index 26
    ASSERT_EQ(26, cClassInfo->name_index);
  }

  {
    // Item 19: CONSTANT_NameAndType
    spCPItem item = parser.classFile->constant_pool->items[19];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    // TODO: confirm index 27 and 28
    ASSERT_EQ(27, cNameAndTypeInfo->name_index);
    ASSERT_EQ(28, cNameAndTypeInfo->descriptor_index);
  }

  {
    // Item 20: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[20];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(10, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("HelloWorld", str);
  }

  {
    // Item 21: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[21];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(16, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("java/lang/Object", str);
  }

  {
    // Item 22: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[22];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(16, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("java/lang/System", str);
  }

  {
    // Item 23: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[23];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(3, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("out", str);
  }

  {
    // Item 24: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[24];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(21, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Ljava/io/PrintStream;", str);
  }

  {
    // Item 25: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[25];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(19, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("java/io/PrintStream", str);
  }

  {
    // Item 26: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[26];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(7, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("println", str);
  }

  {
    // Item 27: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[27];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(21, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("(Ljava/lang/String;)V", str);
  }

  ASSERT_EQ(ACC_PUBLIC | ACC_SUPER, parser.classFile->access_flags);
}
