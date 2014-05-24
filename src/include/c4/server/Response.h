//-*- C++ -*-
#ifndef __RESPONSE_H__
#define __RESPONSE_H__

#include <memory>
#include "c4/server/Response.pb.h"

namespace c4 {

typedef std::shared_ptr<Response> spResponse;

} // namespace

#endif
