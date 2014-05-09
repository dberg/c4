//-*- C++ -*-
#ifndef __CONFIG_H__
#define __CONFIG_H__

#ifdef __linux__
#define HAVE_EPOLL 1
#endif

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined (__NetBSD__)
#define HAVE_KQUEUE 1
#endif

#define LOG_INFO 1
#define LOG_ERROR 2
#define LOG_LEVEL LOG_INFO

#endif
