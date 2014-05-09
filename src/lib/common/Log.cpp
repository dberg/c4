#include "c4/common/Log.h"

namespace c4 {

void log(int level, std::string message) {

#if LOG_LEVEL == LOG_INFO
  if (level == LOG_INFO) {
    std::cout << message << std::endl;
  }
#endif

  if (level == LOG_ERROR) {
    std::cerr << message << std::endl;
  }
}

}
