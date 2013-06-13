//-*- C++ -*-
#ifndef __AST_BIN_STACK_FRAME_H__
#define __AST_BIN_STACK_FRAME_H__

namespace djp {

typedef std::shared_ptr<struct StackMapTable> spStackMapTable;
typedef std::shared_ptr<struct StackMapFrame> spStackMapFrame;

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

enum StackMapFrameType {
  STACK_MAP_FRAME_SAME_FRAME,
  STACK_MAP_FRAME_SAME_LOCALS_1_STACK_ITEM_FRAME,
  STACK_MAP_FRAME_SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED,
  STACK_MAP_FRAME_CHOP_FRAME,
  STACK_MAP_FRAME_SAME_FRAME_EXTENDED,
  STACK_MAP_FRAME_APPEND_FRAME,
  STACK_MAP_FRAME_FULL_FRAME,
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
  StackMapFrameType type;
};

} // namespace
#endif