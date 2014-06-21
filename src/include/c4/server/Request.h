//-*- C++ -*-
#ifndef __C4_SERVER_REQUEST_H__
#define __C4_SERVER_REQUEST_H__

#include <memory>
#include "c4/server/Request.pb.h"

namespace c4 {

typedef std::shared_ptr<Request> spRequest;

} // namespace

#endif
