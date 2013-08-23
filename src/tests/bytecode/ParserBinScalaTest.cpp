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

  {
    // Item 5: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[5];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(16, item->cUtf8Info->length);
    ASSERT_EQ(16, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("HelloWorld.scala", str);
  }

  {
    // Item 6: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[6];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(30, item->cUtf8Info->length);
    ASSERT_EQ(30, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Lscala/reflect/ScalaSignature;", str);
  }

  {
    // Item 7: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[7];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(5, item->cUtf8Info->length);
    ASSERT_EQ(5, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("bytes", str);
  }

  {
    // Item 8: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[8];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(146, item->cUtf8Info->length);
    ASSERT_EQ(146, item->cUtf8Info->bytes.size());
    // TODO: check content
  }

  {
    // Item 9: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[9];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(4, item->cUtf8Info->length);
    ASSERT_EQ(4, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("main", str);
  }

  {
    // Item 10: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[10];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(22, item->cUtf8Info->length);
    ASSERT_EQ(22, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("([Ljava/lang/String;)V", str);
  }

  {
    // Item 11: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[11];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(11, item->cUtf8Info->length);
    ASSERT_EQ(11, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("HelloWorld$", str);
  }

  {
    // Item 12: CONSTANT_Class
    spCPItem item = parser.classFile->constant_pool->items[12];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    ASSERT_EQ(11, cClassInfo->name_index);
  }

  {
    // Item 13: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[13];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(7, item->cUtf8Info->length);
    ASSERT_EQ(7, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("MODULE$", str);
  }

  {
    // Item 14: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[14];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(13, item->cUtf8Info->length);
    ASSERT_EQ(13, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("LHelloWorld$;", str);
  }

  {
    // Item 15: CONSTANT_NameAndType.
    // Field $MODULE$ of type LHelloWorld$
    spCPItem item = parser.classFile->constant_pool->items[15];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    ASSERT_EQ(13, cNameAndTypeInfo->name_index); // $MODULE$
    ASSERT_EQ(14, cNameAndTypeInfo->descriptor_index); // LHelloWorld$;
  }

  {
    // Item 16: CONSTANT_Fieldref
    spCPItem item = parser.classFile->constant_pool->items[16];
    ASSERT_EQ(CONSTANT_Fieldref, item->tag);
    spCFieldrefInfo cFieldrefInfo = item->cFieldrefInfo;
    ASSERT_EQ(12, cFieldrefInfo->class_index);
    ASSERT_EQ(15, cFieldrefInfo->name_and_type_index);

    // Assert that index 12 is a class (HelloWorld$) and that
    // index 15 is a NameAndType constant ($MODULE$).
    ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[12]->tag);
    ASSERT_EQ(CONSTANT_NameAndType,
      parser.classFile->constant_pool->items[15]->tag);
  }

  {
    // Item 17: CONSTANT_NameAndType.
    spCPItem item = parser.classFile->constant_pool->items[17];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    ASSERT_EQ(9, cNameAndTypeInfo->name_index); // main
    ASSERT_EQ(10, cNameAndTypeInfo->descriptor_index); // ([Ljava/lang/String;)V
  }

  {
    // Item 18: CONSTANT_Methodref
    // Method main from class HelloWorld$
    spCPItem item = parser.classFile->constant_pool->items[18];
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    ASSERT_EQ(12, cMethodrefInfo->class_index);
    ASSERT_EQ(17, cMethodrefInfo->name_and_type_index);

    // Assert that index 12 is a class and 17 is a name and type info
    ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[12]->tag);
    ASSERT_EQ(CONSTANT_NameAndType,
      parser.classFile->constant_pool->items[17]->tag);
  }

  {
    // Item 19: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[19];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(11, item->cUtf8Info->length);
    ASSERT_EQ(11, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("delayedInit", str);
  }

  {
    // Item 20: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[20];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(20, item->cUtf8Info->length);
    ASSERT_EQ(20, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("(Lscala/Function0;)V", str);
  }

  {
    // Item 21: CONSTANT_NameAndType.
    spCPItem item = parser.classFile->constant_pool->items[21];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    ASSERT_EQ(19, cNameAndTypeInfo->name_index); // delayedInit
    ASSERT_EQ(20, cNameAndTypeInfo->descriptor_index); // ([Ljava/Function0;)V
  }

  {
    // Item 22: CONSTANT_Methodref
    // Method delayedInit from class HelloWorld$
    spCPItem item = parser.classFile->constant_pool->items[22];
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    ASSERT_EQ(12, cMethodrefInfo->class_index);
    ASSERT_EQ(21, cMethodrefInfo->name_and_type_index);

    // Assert that index 12 is a class and 21 is a name and type info
    ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[12]->tag);
    ASSERT_EQ(CONSTANT_NameAndType,
      parser.classFile->constant_pool->items[21]->tag);
  }

  {
    // Item 23: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[23];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(4, item->cUtf8Info->length);
    ASSERT_EQ(4, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("args", str);
  }

  {
    // Item 24: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[24];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(21, item->cUtf8Info->length);
    ASSERT_EQ(21, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("()[Ljava/lang/String;", str);
  }

  {
    // Item 25: CONSTANT_NameAndType.
    spCPItem item = parser.classFile->constant_pool->items[25];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    ASSERT_EQ(23, cNameAndTypeInfo->name_index); // args
    ASSERT_EQ(24, cNameAndTypeInfo->descriptor_index); // ()[Ljava/lang/String;
  }

  {
    // Item 26: CONSTANT_Methodref
    // Method args from class HelloWorld$
    spCPItem item = parser.classFile->constant_pool->items[26];
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    ASSERT_EQ(12, cMethodrefInfo->class_index);
    ASSERT_EQ(25, cMethodrefInfo->name_and_type_index);

    // Assert that index 12 is a class and 25 is a name and type info
    ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[12]->tag);
    ASSERT_EQ(CONSTANT_NameAndType,
      parser.classFile->constant_pool->items[25]->tag);
  }

  {
    // Item 27: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[27];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(37, item->cUtf8Info->length);
    ASSERT_EQ(37, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("scala$App$_setter_$executionStart_$eq", str);
  }

  {
    // Item 28: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[28];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(4, item->cUtf8Info->length);
    ASSERT_EQ(4, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("(J)V", str);
  }

  {
    // Item 29: CONSTANT_NameAndType.
    spCPItem item = parser.classFile->constant_pool->items[29];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    // scala$App$_setter_$executionStart_$eq
    ASSERT_EQ(27, cNameAndTypeInfo->name_index);
    ASSERT_EQ(28, cNameAndTypeInfo->descriptor_index); // (J)V
  }

  {
    // Item 30: CONSTANT_Methodref
    // Method scala$App$_setter_$executionStart_$eq from class HelloWorld$
    spCPItem item = parser.classFile->constant_pool->items[30];
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    ASSERT_EQ(12, cMethodrefInfo->class_index);
    ASSERT_EQ(29, cMethodrefInfo->name_and_type_index);

    // Assert that index 12 is a class and 25 is a name and type info
    ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[12]->tag);
    ASSERT_EQ(CONSTANT_NameAndType,
      parser.classFile->constant_pool->items[25]->tag);
  }

  {
    // Item 31: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[31];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(14, item->cUtf8Info->length);
    ASSERT_EQ(14, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("executionStart", str);
  }

  {
    // Item 32: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[32];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(3, item->cUtf8Info->length);
    ASSERT_EQ(3, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("()J", str);
  }

  {
    // Item 33: CONSTANT_NameAndType.
    spCPItem item = parser.classFile->constant_pool->items[33];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    ASSERT_EQ(31, cNameAndTypeInfo->name_index); // executionStart
    ASSERT_EQ(32, cNameAndTypeInfo->descriptor_index); // ()J
  }

  {
    // Item 34: CONSTANT_Methodref
    // Method executionStart from class HelloWorld$
    spCPItem item = parser.classFile->constant_pool->items[34];
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    ASSERT_EQ(12, cMethodrefInfo->class_index);
    ASSERT_EQ(33, cMethodrefInfo->name_and_type_index);

    // Assert that index 12 is a class and 33 is a name and type info
    ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[12]->tag);
    ASSERT_EQ(CONSTANT_NameAndType,
      parser.classFile->constant_pool->items[33]->tag);
  }

  {
    // Item 35: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[35];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(27, item->cUtf8Info->length);
    ASSERT_EQ(27, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("HelloWorld$delayedInit$body", str);
  }

  {
    // Item 36: CONSTANT_Class HelloWorld
    spCPItem item = parser.classFile->constant_pool->items[36];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    ASSERT_EQ(35, cClassInfo->name_index);
  }

  {
    // Item 37: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[37];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(16, item->cUtf8Info->length);
    ASSERT_EQ(16, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("delayedInit$body", str);
  }

  {
    // Item 38: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[38];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(4, item->cUtf8Info->length);
    ASSERT_EQ(4, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Code", str);
  }

  {
    // Item 39: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[39];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(9, item->cUtf8Info->length);
    ASSERT_EQ(9, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Signature", str);
  }

  {
    // Item 40: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[40];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(47, item->cUtf8Info->length);
    ASSERT_EQ(47, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("(Lscala/Function0<Lscala/runtime/BoxedUnit;>;)V", str);
  }

  {
    // Item 41: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[41];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(10, item->cUtf8Info->length);
    ASSERT_EQ(10, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("SourceFile", str);
  }

  {
    // Item 42: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[42];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(12, item->cUtf8Info->length);
    ASSERT_EQ(12, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("InnerClasses", str);
  }

  {
    // Item 43: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[43];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(25, item->cUtf8Info->length);
    ASSERT_EQ(25, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("RuntimeVisibleAnnotations", str);
  }

  {
    // Item 44: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[44];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(8, item->cUtf8Info->length);
    ASSERT_EQ(8, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("ScalaSig", str);
  }

  // TODO: HelloWorld$.class
  // TODO: HelloWorld$delayedInit$body.class
}
