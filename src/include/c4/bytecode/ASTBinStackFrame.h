//-*- C++ -*-
#ifndef __AST_BIN_STACK_FRAME_H__
#define __AST_BIN_STACK_FRAME_H__

namespace c4 {

typedef std::shared_ptr<struct StackMapTable> spStackMapTable;
typedef std::shared_ptr<struct StackMapFrame> spStackMapFrame;
typedef std::shared_ptr<struct SameFrame> spSameFrame;
typedef std::shared_ptr<struct SameLocals1StackItemFrame>
  spSameLocals1StackItemFrame;
typedef std::shared_ptr<struct SameLocals1StackItemFrameExtended>
  spSameLocals1StackItemFrameExtended;
typedef std::shared_ptr<struct ChopFrame> spChopFrame;
typedef std::shared_ptr<struct SameFrameExtended> spSameFrameExtended;
typedef std::shared_ptr<struct AppendFrame> spAppendFrame;
typedef std::shared_ptr<struct FullFrame> spFullFrame;
typedef std::shared_ptr<struct VerificationTypeInfo> spVerificationTypeInfo;

enum class StackMapFrameOpt {
  SAME_FRAME,
  SAME_LOCALS_1_STACK_ITEM_FRAME,
  SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED,
  CHOP_FRAME,
  SAME_FRAME_EXTENDED,
  APPEND_FRAME,
  FULL_FRAME,
};

/**
 * union verification_type_info {
 *     Top_variable_info;
 *     Integer_variable_info;
 *     Float_variable_info;
 *     Long_variable_info;
 *     Double_variable_info;
 *     Null_variable_info;
 *     UninitializedThis_variable_info;
 *     Object_variable_info;
 *     Uninitialized_variable_info;
 * }
*/
enum class VerificationTypeInfoOpt {
  TOP_VARIABLE_INFO = 0,
  INTEGER_VARIABLE_INFO = 1,
  FLOAT_VARIABLE_INFO = 2,
  DOUBLE_VARIABLE_INFO = 3,
  LONG_VARIABLE_INFO = 4,
  NULL_VARIABLE_INFO = 5,
  UNINITIALIZEDTHIS_VARIABLE_INFO = 6,
  OBJECT_VARIABLE_INFO = 7,
  UNINITIALIZED_VARIABLE_INFO = 8,
};

/**
 * StackMapTable_attribute {
 *      u2              attribute_name_index;
 *      u4              attribute_length;
 *      u2              number_of_entries;
 *      stack_map_frame entries[number_of_entries];
 * }
 */
struct StackMapTable {
  // u2 attribute_name_index;
  // u4 attribute_length;
  u2 number_of_entries;
  std::vector<spStackMapFrame> entries;
};

/**
 * union stack_map_frame {
 *     same_frame;
 *     same_locals_1_stack_item_frame;
 *     same_locals_1_stack_item_frame_extended;
 *     chop_frame;
 *     same_frame_extended;
 *     append_frame;
 *     full_frame;
 * }
 */
struct StackMapFrame {
  StackMapFrameOpt type;

  spSameFrame sameFrame;
  spSameLocals1StackItemFrame sameLocals1;
  spSameLocals1StackItemFrameExtended sameLocals1Ext;
  spChopFrame chopFrame;
  spSameFrameExtended sameExt;
  spAppendFrame appendFrame;
  spFullFrame fullFrame;
};

struct SameFrame {
  u1 frame_type; // 0-63
};

struct SameLocals1StackItemFrame {
  u1 frame_type; // 64-127
  std::vector<spVerificationTypeInfo> stack; // [1]
};

struct SameLocals1StackItemFrameExtended {
  u1 frame_type; // 128-247
  u2 offset_delta;
  std::vector<spVerificationTypeInfo> stack; // [1]
};

struct ChopFrame {
  u1 frame_type; // 248-250
  u2 offset_delta;
};

struct SameFrameExtended {
  u1 frame_type; // 251
  u2 offset_delta;
};

struct AppendFrame {
  u1 frame_type; // 252-254
  u2 offset_delta;
  std::vector<spVerificationTypeInfo> locals; // [frame_type - 251]
};

struct FullFrame {
  u1 frame_type; // 255
  u1 offset_delta;
  u2 number_of_locals;
  std::vector<spVerificationTypeInfo> locals; // [number_of_locals]
  u2 number_of_stack_items;
  std::vector<spVerificationTypeInfo> stack; // [number_of_stack_items]
};

struct VerificationTypeInfo {
  //u1 tag; We use an enun to hold the tag value.
  VerificationTypeInfoOpt tag;

  // cpool_index if tag == VERIFICATION_TYPE_OBJECT_VARIABLE_INFO
  // offset if tag == VERIFICATION_TYPE_UNINITIALIZED_VARIABLE_INFO
  u2 cpoolIndexOrOffset;
};
} // namespace
#endif
