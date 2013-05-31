#include "djp/BinOutput.h"

namespace djp {

u4 BinOutputCode::getCodeU4() {
  return ((code[++idx] << 24)
    | (code[++idx] << 16)
    | (code[++idx] << 8)
    | code[++idx]);
}

u2 BinOutputCode::getCodeU2() {
  return (code[++idx] << 8) | code[++idx];
}

std::string BinOutputCode::codeIdxInfo(size_t maxLineWidth) {
  std::stringstream idxColon;
  idxColon << "  " << idx << ":";

  std::stringstream idxWithFillIn;
  idxWithFillIn << std::left << std::setw(maxLineWidth + 4)
    << std::setfill(' ') << idxColon.str();

  return idxWithFillIn.str();
}

void BinOutputCode::build() {
  std::stringstream ss; ss << code.size();
  size_t maxLineWidth = ss.str().size();

  for (idx = 0; idx < code.size(); idx++) {
    u1 opcode = code[idx];
    PairOpNameAndType pair = opcodes.info[opcode];
    std::string label = pair.first;
    OperandType type = pair.second;

    out << codeIdxInfo(maxLineWidth) << label << " ";

    switch (type) {
    case OPERAND_NONE:
      out << std::endl;
      break;
    case OPERAND_1BYTE:
      out << (unsigned) code[++idx] << std::endl;
      break;
    case OPERAND_2BYTES:
      out << getCodeU2() << std::endl;
      break;
    case OPERAND_IINC:
      out << (unsigned) code[++idx] << " "
        << (unsigned) code[++idx] << std::endl;
      break;
    case OPERAND_MULTIANEWARRAY:
      out << getCodeU2() << " "
          << (unsigned) code[++idx] << std::endl;
      break;
    case OPERAND_4BYTES:
      out << getCodeU4() << std::endl;
      break;
    case OPERAND_INVOKE_DYNAMIC:
      out <<  getCodeU2() << " "
          << (unsigned) code[++idx] << " "
          << (unsigned) code[++idx] << std::endl;
      break;
    case OPERAND_INVOKE_INTERFACE:
      out << getCodeU2() << " "
          << (unsigned) code[++idx]
          << (unsigned) code[++idx] << std::endl;
      break;

    case OPERAND_WIDE: {
      u1 wideOpcode = code[++idx];
      if (isWideFormat1(wideOpcode)) {
        // format 1
        PairOpNameAndType widePair = opcodes.info[wideOpcode];
        out << widePair.first << getCodeU2() << std::endl;
      } else if (wideOpcode == 0x84 /*iinc*/) {
        // format 2
        out << "iinc " << ((code[++idx] << 8) | code[++idx]) << " "
          << ((code[++idx] << 8) | code[++idx]) << std::endl;
      } else {
        // error
        out << "ERROR!" << std::endl;
        return;
      }
      break;
    }

    case OPERAND_LOOKUPSWITCH: {
      u4 switchIdxLocal = idx;
      unsigned long switchIdxGlobal = codeIdxGlobal + switchIdxLocal;
      int padding = (switchIdxGlobal % 4) ? (4 - (switchIdxGlobal % 4)) : 0;
      out << "padding " << padding;
      for (int p = 1; p <= padding; p++) { ++idx; }

      unsigned long defaultGoto = getCodeU4() + switchIdxLocal;

      u4 npairs = getCodeU4();
      out << " npairs " << npairs << std::endl;
      for (u4 j = 0; j < npairs; j++) {
        unsigned long caseLabel = getCodeU4();
        unsigned long caseGoto = getCodeU4() + switchIdxLocal;
        out << std::setw(maxLineWidth + 8) << std::setfill(' ') << " "
            << caseLabel << ": " << caseGoto << std::endl;
      }

      out << std::setw(maxLineWidth + 8) << std::setfill(' ') << " "
        << "default: " << defaultGoto << std::endl;

      break;
    }

    case OPERAND_TABLESWITCH:
      out << "TODO!" << std::endl;
      break;
    default:
      out << "ERROR" << std::endl;
    }
  }
}

// Helper methods
bool isWideFormat1(u1 opcode) {
  if (opcode == 0x15 // iload
    || opcode == 0x17 // fload
    || opcode == 0x19 // aload
    || opcode == 0x16 // lload
    || opcode == 0x18 // dload
    || opcode == 0x36 // istore
    || opcode == 0x38 // fstore
    || opcode == 0x3a // astore
    || opcode == 0x37 // lstore
    || opcode == 0x39 // dstore
    || opcode == 0xa9 // ret
    ) {
    return true;
  }
  return false;
}

} // namespace
