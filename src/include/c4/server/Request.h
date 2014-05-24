//-*- C++ -*-
#ifndef __REQUEST_H__
#define __REQUEST_H__

#include <memory>
#include "Request.pb.h"

namespace c4 {

typedef std::shared_ptr<Request> spRequest;

} // namespace

#endif
