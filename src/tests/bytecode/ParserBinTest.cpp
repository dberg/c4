#include <string>
#include <vector>
#include "djp/File.h"
#include "djp/ParserBin.h"
#include "Util.h"
#include "gtest/gtest.h"
using namespace djp;

/**
 * public class HelloWorld {
 *     public static void main(String[] args) {
 *         System.out.println("Oi mundo");
 *     }
 * }
 */
TEST(ParserBin, HelloWorld) {
  std::vector<unsigned char> buffer;
  File file;
  std::string filename =  current_dir + "/bytecode-classes/HelloWorld.class";
  ASSERT_EQ(file.read(filename, buffer), 0);
  ParserBin parser(filename, buffer);
  parser.parse();

  ASSERT_EQ(0xCAFEBABE, parser.classFile->magic);
  ASSERT_EQ(29, parser.classFile->constant_pool_count);
  ASSERT_EQ(29, parser.classFile->constant_pool->items.size());

  {
    // Item 1: CONSTANT_Methodref
    // Method <init> from class java.lang.Object
    spCPItem item = parser.classFile->constant_pool->items[1];
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    ASSERT_EQ(6, cMethodrefInfo->class_index);
    ASSERT_EQ(15, cMethodrefInfo->name_and_type_index);

    // Assert that index 6 is a class and 15 is a name and type info
    ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[6]->tag);
    ASSERT_EQ(CONSTANT_NameAndType,
      parser.classFile->constant_pool->items[15]->tag);
  }

  {
    // Item 2: CONSTANT_Fieldref
    spCPItem item = parser.classFile->constant_pool->items[2];
    ASSERT_EQ(CONSTANT_Fieldref, item->tag);
    spCFieldrefInfo cFieldrefInfo = item->cFieldrefInfo;
    ASSERT_EQ(16, cFieldrefInfo->class_index);
    ASSERT_EQ(17, cFieldrefInfo->name_and_type_index);

    // Assert that index 16 is a class (java.lang.System) and that
    // index 17 is a NameAndType constant ("out").
    ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[16]->tag);
    ASSERT_EQ(CONSTANT_NameAndType,
      parser.classFile->constant_pool->items[17]->tag);
  }

  {
    // Item 3: CONSTANT_String
    spCPItem item = parser.classFile->constant_pool->items[3];
    ASSERT_EQ(CONSTANT_String, item->tag);
    spCStringInfo cStringInfo = item->cStringInfo;
    ASSERT_EQ(18, cStringInfo->string_index);
    // Assert that index 18 is a CONSTANT_Utf8 ("Oi mundo")
    ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[18]->tag);
  }

  {
    // Item 4: CONSTANT_Methodref
    // Method println from class java.io.PrintStream
    spCPItem item = parser.classFile->constant_pool->items[4];
    ASSERT_EQ(CONSTANT_Methodref, item->tag);
    spCMethodrefInfo cMethodrefInfo = item->cMethodrefInfo;
    ASSERT_EQ(19, cMethodrefInfo->class_index);
    ASSERT_EQ(20, cMethodrefInfo->name_and_type_index);

    // Assert that index 19 is a CONSTANT_Class and index 20 is 
    // CONSTANT_NameAndType "(Ljava/lang/String;)V
    ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[19]->tag);
    ASSERT_EQ(CONSTANT_NameAndType,
      parser.classFile->constant_pool->items[20]->tag);
  }

  {
    // Item 5: CONSTANT_Class HelloWorld
    spCPItem item = parser.classFile->constant_pool->items[5];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    ASSERT_EQ(21, cClassInfo->name_index);
    ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[21]->tag);
  }

  {
    // Item 6: CONSTANT_Class java.lang.Object
    spCPItem item = parser.classFile->constant_pool->items[6];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    ASSERT_EQ(22, cClassInfo->name_index);
    ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[22]->tag);
  }

  {
    // Item 7: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[7];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(6, item->cUtf8Info->length);
    ASSERT_EQ(6, item->cUtf8Info->bytes.size());
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("<init>", str);
  }

  {
    // Item 8: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[8];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(3, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("()V", str);
  }

  {
    // Item 9: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[9];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(4, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Code", str);
  }

  {
    // Item 10: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[10];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(15, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("LineNumberTable", str);
  }

  {
    // Item 11: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[11];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(4, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("main", str);
  }

  {
    // Item 12: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[12];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(22, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("([Ljava/lang/String;)V", str);
  }

  {
    // Item 13: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[13];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(10, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("SourceFile", str);
  }

  {
    // Item 14: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[14];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(15, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("HelloWorld.java", str);
  }

  {
    // Item 15: CONSTANT_NameAndType.
    // Method <init> and descriptor ()V
    spCPItem item = parser.classFile->constant_pool->items[15];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    ASSERT_EQ(7, cNameAndTypeInfo->name_index); // "<init>"
    ASSERT_EQ(8, cNameAndTypeInfo->descriptor_index); // "()V"
  }

  {
    // Item 16: CONSTANT_Class java.lang.System
    spCPItem item = parser.classFile->constant_pool->items[16];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    ASSERT_EQ(23, cClassInfo->name_index);
  }

  {
    // Item 17: CONSTANT_NameAndType
    spCPItem item = parser.classFile->constant_pool->items[17];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    // "out"
    ASSERT_EQ(24, cNameAndTypeInfo->name_index);
    // "Ljava/io/PrintStream;"
    ASSERT_EQ(25, cNameAndTypeInfo->descriptor_index);
  }

  {
    // Item 18: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[18];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(8, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Oi mundo", str);
  }

  {
    // Item 19: CONSTANT_Class "java/io/PrintStream"
    spCPItem item = parser.classFile->constant_pool->items[19];
    ASSERT_EQ(CONSTANT_Class, item->tag);
    spCClassInfo cClassInfo = item->cClassInfo;
    ASSERT_EQ(26, cClassInfo->name_index);
  }

  {
    // Item 20: CONSTANT_NameAndType
    spCPItem item = parser.classFile->constant_pool->items[20];
    ASSERT_EQ(CONSTANT_NameAndType, item->tag);
    spCNameAndTypeInfo cNameAndTypeInfo = item->cNameAndTypeInfo;
    // "println"
    ASSERT_EQ(27, cNameAndTypeInfo->name_index);
    // "(Ljava/lang/String;)V"
    ASSERT_EQ(28, cNameAndTypeInfo->descriptor_index);
  }

  {
    // Item 21: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[21];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(10, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("HelloWorld", str);
  }

  {
    // Item 22: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[22];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(16, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("java/lang/Object", str);
  }

  {
    // Item 23: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[23];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(16, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("java/lang/System", str);
  }

  {
    // Item 24: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[24];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(3, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("out", str);
  }

  {
    // Item 25: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[25];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(21, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("Ljava/io/PrintStream;", str);
  }

  {
    // Item 26: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[26];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(19, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("java/io/PrintStream", str);
  }

  {
    // Item 27: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[27];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(7, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("println", str);
  }

  {
    // Item 28: CONSTANT_Utf8
    spCPItem item = parser.classFile->constant_pool->items[28];
    ASSERT_EQ(CONSTANT_Utf8, item->tag);
    spCUtf8Info cUtf8Info = item->cUtf8Info;
    ASSERT_EQ(21, item->cUtf8Info->length);
    std::string str(item->cUtf8Info->bytes.begin(),
      item->cUtf8Info->bytes.end());
    ASSERT_EQ("(Ljava/lang/String;)V", str);
  }

  ASSERT_EQ(CLASS_ACC_PUBLIC | CLASS_ACC_SUPER, parser.classFile->access_flags);

  ASSERT_EQ(5, parser.classFile->this_class);
  ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[5]->tag);

  // Our class inherits from java.lang.Object
  ASSERT_EQ(6, parser.classFile->super_class);
  ASSERT_EQ(CONSTANT_Class, parser.classFile->constant_pool->items[6]->tag);

  ASSERT_EQ(0, parser.classFile->interfaces_count);

  ASSERT_EQ(0, parser.classFile->fields_count);

  // 2 methods: <init> and main
  ASSERT_EQ(2, parser.classFile->methods_count);
  ASSERT_EQ(2, parser.classFile->methods.size());

  {
    // Method 1
    spMethodInfo method = parser.classFile->methods[0];
    ASSERT_EQ(METHOD_ACC_PUBLIC, method->access_flags);

    // name - <init>
    ASSERT_EQ(7, method->name_index);
    ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[7]->tag);

    // descriptor - ()V
    ASSERT_EQ(8, method->descriptor_index);
    ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[7]->tag);

    // attributes - Code
    ASSERT_EQ(1, method->attributes_count);
    ASSERT_EQ(1, method->attributes.size());
    spAttributeInfo info = method->attributes[0];
    ASSERT_EQ(ATTRIBUTE_TYPE_CODE, info->type);
    ASSERT_EQ(9, info->attribute_name_index); // Code
    ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[9]->tag);
    ASSERT_EQ(29, info->attribute_length);

    // attribute info
    spCodeAttribute code = info->code;
    ASSERT_EQ(1, code->max_stack);
    ASSERT_EQ(1, code->max_locals);
    ASSERT_EQ(5, code->code_length);
    ASSERT_EQ(5, code->code.size());
    ASSERT_EQ(0, code->exception_table_length);
    ASSERT_EQ(0, code->exceptions.size());

    // code
    ASSERT_EQ(0x2A, code->code[0]); // aload_0
    ASSERT_EQ(0xB7, code->code[1]); // invoke_special
    // index in constant pool
    // CONSTANT_Methodref <init>
    ASSERT_EQ(1, (code->code[2] << 8 | code->code[3]));
    ASSERT_EQ(0xB1, code->code[4]); // return

    ASSERT_EQ(1, code->attributes_count);
    ASSERT_EQ(1, code->attributes.size());
    spAttributeInfo codeInfo = code->attributes[0];
    ASSERT_EQ(ATTRIBUTE_TYPE_LINE_NUMBER_TABLE, codeInfo->type);
    ASSERT_EQ(10, codeInfo->attribute_name_index); // LineNumberTable
    ASSERT_EQ(6, codeInfo->attribute_length);
    ASSERT_EQ(1, codeInfo->table->line_number_table_length);
    ASSERT_EQ(1, codeInfo->table->table.size());
    ASSERT_EQ(0, codeInfo->table->table[0]->start_pc);
    ASSERT_EQ(1, codeInfo->table->table[0]->line_number);
  }

  {
    // Method 2
    spMethodInfo method = parser.classFile->methods[1];
    ASSERT_EQ(METHOD_ACC_PUBLIC | METHOD_ACC_STATIC, method->access_flags);

    // name - main
    ASSERT_EQ(11, method->name_index);
    ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[11]->tag);

    // descriptor - ([Ljava/lang/String;)V
    ASSERT_EQ(12, method->descriptor_index);
    ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[12]->tag);

    // attributes - Code
    ASSERT_EQ(1, method->attributes_count);
    ASSERT_EQ(1, method->attributes.size());
    spAttributeInfo info = method->attributes[0];
    ASSERT_EQ(9, info->attribute_name_index);
    ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[9]->tag);
    ASSERT_EQ(37, info->attribute_length);

    // attribute info
    spCodeAttribute code = info->code;
    ASSERT_EQ(2, code->max_stack);
    ASSERT_EQ(1, code->max_locals);
    ASSERT_EQ(9, code->code_length);
    ASSERT_EQ(9, code->code.size());
    ASSERT_EQ(0, code->exception_table_length);
    ASSERT_EQ(0, code->exceptions.size());

    // code
    ASSERT_EQ(0xB2, code->code[0]); // getstatic
    // index in constant pool
    // CONSTANT_Fieldref "out"
    ASSERT_EQ(2, (code->code[1] << 8 | code->code[2]));
    ASSERT_EQ(0x12, code->code[3]); // ldc
    // index, ref to a string literal "Oi mundo"
    ASSERT_EQ(3, code->code[4]);
    ASSERT_EQ(0xB6, code->code[5]); // invokevirtual
    // index, CONSTANT_Methodref println
    ASSERT_EQ(4, (code->code[6] << 8 | code->code[7]));
    ASSERT_EQ(0xB1, code->code[8]); // return

    ASSERT_EQ(1, code->attributes_count);
    ASSERT_EQ(1, code->attributes.size());
    spAttributeInfo codeInfo = code->attributes[0];
    ASSERT_EQ(ATTRIBUTE_TYPE_LINE_NUMBER_TABLE, codeInfo->type);
    ASSERT_EQ(ATTRIBUTE_TYPE_LINE_NUMBER_TABLE, codeInfo->type);
    ASSERT_EQ(10, codeInfo->attribute_name_index); // LineNumberTable
    ASSERT_EQ(10, codeInfo->attribute_length);
    ASSERT_EQ(2, codeInfo->table->line_number_table_length);
    ASSERT_EQ(2, codeInfo->table->table.size());
    ASSERT_EQ(0, codeInfo->table->table[0]->start_pc);
    ASSERT_EQ(3, codeInfo->table->table[0]->line_number);
    ASSERT_EQ(8, codeInfo->table->table[1]->start_pc);
    ASSERT_EQ(4, codeInfo->table->table[1]->line_number);
  }

  // attributes - SourceFile
  ASSERT_EQ(1, parser.classFile->attributes_count);
  ASSERT_EQ(1, parser.classFile->attributes.size());
  spAttributeInfo info = parser.classFile->attributes[0];
  ASSERT_EQ(ATTRIBUTE_TYPE_SOURCE_FILE, info->type);
  ASSERT_EQ(13, info->attribute_name_index);
  ASSERT_EQ(CONSTANT_Utf8, parser.classFile->constant_pool->items[13]->tag);
  ASSERT_EQ(2, info->attribute_length);

  // CONSTANT_Utf8 "HelloWorld.java"
  ASSERT_EQ(14, info->sourcefile_index);
}
