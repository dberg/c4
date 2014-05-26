//-*- C++ -*-
#ifndef __C4S_GLOBAL_H__
#define __C4S_GLOBAL_H__

#include <memory>

namespace c4s {

class Global;
typedef shared_ptr<Global> spGlobal;

class Run;
typedef shared_ptr<Run> spRun;

class Global {

};

/**
 * Single execution of the compiler on a set of units.
 */
class Run {

};

} // namespace

#endif
