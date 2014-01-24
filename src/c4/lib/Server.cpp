#include "c4/Server.h"

/**
 * The Server implementation is platform specific.
 * If we have epoll we assume linux and if we have kqueue we assume bsd.
 */
#ifdef HAVE_EPOLL
#include "ServerLinux.cpp"
#else
  #ifdef HAVE_KQUEUE
  #include "ServerBSD.cpp"
  #endif
#endif
