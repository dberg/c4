//-*- C++ -*-
#ifndef __FILE_H__
#define __FILE_H__
#include <string>
#include <fstream>
#include <streambuf>

namespace c4 {

class File {

public:
  template<class T>
  int read(std::string filename, T &buffer) {
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
};

}

#endif
