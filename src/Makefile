#------------------------------------------------------------------------------
# C4 Build
#------------------------------------------------------------------------------
# To build run:
# make
#
# To build in Debug mode run:
# make BUILDTYPE=Debug
#
# A C++14 compiler is required.
#------------------------------------------------------------------------------
MAKEFLAGS=-r

BUILDTYPE ?= Release

#-----------------------------------------------------------------------------
# Compiler flags
#------------------------------------------------------------------------------
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  #CXX = clang++
  CXX14FLAGS = -std=c++14 -stdlib=libc++
  ifeq ($(wildcard /opt/local/include),)
    # homebrew
    INC_EXTRA ?= -I/usr/local/include
    LIB_EXTRA ?= -L/usr/local/lib
  else
    # macports
    INC_EXTRA ?= -I/opt/local/include
    LIB_EXTRA ?= -L/opt/local/lib
  endif
endif

CXX14FLAGS ?= -std=c++14
INC_EXTRA ?=
LIB_EXTRA ?=

#------------------------------------------------------------------------------
# Release type
#------------------------------------------------------------------------------
ifeq ($(BUILDTYPE),Release)
    CFLAGS = -Wall -Wextra -Os $(CXX14FLAGS)
else
    CFLAGS = -Wall -Wextra -g3 $(CXX14FLAGS)
endif

#------------------------------------------------------------------------------
# Output directory
#------------------------------------------------------------------------------
builddir_name = out
builddir = $(builddir_name)/$(BUILDTYPE)

#------------------------------------------------------------------------------
# Binaries
#------------------------------------------------------------------------------
obj_main = $(builddir)/obj/main

bin = $(builddir)/bin

#------------------------------------------------------------------------------
# Headers
#------------------------------------------------------------------------------
incdir_main := include/c4/main

INC = $(INC_EXTRA) -Iinclude -Ithird_party

#------------------------------------------------------------------------------
# Implementation files
#------------------------------------------------------------------------------
libdir_main := lib/main

#------------------------------------------------------------------------------
# Targets
#------------------------------------------------------------------------------
.PHONY: all
all: directories $(bin)/c4

.PHONY: clean
	rm -r $(builddir_name)

# List of directories to be created or deleted.
c4_directories := \
  $(obj_main) $(bin)

.PHONY: directories
directories:
	mkdir -p $(c4_directories)

# main c4
$(bin)/c4: \
  $(obj_main)/c4.o
	$(CXX) \
  $(CFLAGS) \
  $(INC) \
  $(obj_main)/c4.o -o $@

# objects main
$(obj_main)/%.o: \
  $(libdir_main)/%.cpp \
  $(incdir_main)/%.h
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

$(obj_main)/c4.o: $(libdir_main)/c4.cpp
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@
