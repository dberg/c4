//-*- C++ -*-
#ifndef __C4_COMMON_PROJECT_H__
#define __C4_COMMON_PROJECT_H__

#include <memory>
#include <string>
#include <unordered_map>

#include "c4/common/Compilation.h"
#include "c4/common/ProjectJava.h"
#include "c4/common/ProjectScala.h"
#include "c4/common/Util.h"

using std::string;
using std::unordered_map;
using std::unique_ptr;

namespace c4 {

class Project;
typedef std::shared_ptr<Project> spProject;

class Project {

  u32string id;
  unordered_map<u32string, spCompilation> comps;
  spProjectJava projJava;
  spProjectScala projScala;

public:
  Project(u32string id);
  void compile(spCompilation comp);
};

} // namespace

#endif
