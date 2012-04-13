#include "File.h"

namespace djp {

int File::read(std::string filename, std::string &buffer) {
  std::ifstream t(filename.c_str());
  if (t.good() == false) {
    return -1;
  }

  t.seekg(0, std::ios::end);   
  buffer.reserve(t.tellg());
  t.seekg(0, std::ios::beg);

  buffer.assign((std::istreambuf_iterator<char>(t)),
    std::istreambuf_iterator<char>());

  return 0;
}

}
