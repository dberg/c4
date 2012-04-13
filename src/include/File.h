//-*- C++ -*-
#ifndef __FILE_H
#define __FILE_H
#include <string>
#include <fstream>
#include <streambuf>

namespace djp {
class File {
public:
  int read(std::string filename, std::string &buffer);
};

}

#endif
