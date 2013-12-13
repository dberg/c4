#include "c4/Daemon.h"

/**
 * The Daemon implementation is platform specific.
 * If we have epoll we assume linux and if we have kqueue we assume bsd.
 */
#ifdef HAVE_EPOLL
#include "DaemonLinux.cpp"
#else
  #ifdef HAVE_KQUEUE
  #include "DaemonBSD.cpp"
  #endif
#endif
