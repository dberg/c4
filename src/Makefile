###############################################################################
#
# OPTIONS:
#         BUILDTYPE: Debug (default) or Release. The build is generated under
#        	     /out/BUILDTYPE/.
#                    Ex.: make BUILDTYPE=Release
#
#         clean: remove directory out.
#                    Ex.: make clean
#
# LIBRARIES: boost
# OSX: We rely in the Boost installation from MacPorts for OSX:
#      /opt/local/include
###############################################################################
MAKEFLAGS=-r

CXX ?= g++
BUILDTYPE ?= Debug

# Release type
ifeq ($(BUILDTYPE),Release)
    CFLAGS = -Wall -O3
else
    CFLAGS = -Wall -g
endif

builddir_name = out
builddir = $(builddir_name)/$(BUILDTYPE)
obj = $(builddir)/obj
bin = $(builddir)/bin
incdir := include
libdir := lib
INC = -I$(incdir)

# Darwin
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
INC = -I$(incdir) -I/opt/local/include
endif

.PHONY: all
all: directories $(bin)/djp

.PHONY: clean
clean:
	$(RM) -r $(builddir_name)

.PHONY: directories
directories:
	mkdir -p $(obj) $(bin)

# main djp
$(bin)/djp: $(obj)/CmdInput.o \
            $(obj)/File.o \
            $(obj)/Parser.o \
            $(obj)/Output.o \
            $(obj)/djp.o
	$(CXX) $(obj)/CmdInput.o \
               $(obj)/File.o \
               $(obj)/Parser.o \
               $(obj)/Output.o \
               $(CFLAGS) $(INC) $(obj)/djp.o -o $@

# modules
$(obj)/CmdInput.o: $(libdir)/CmdInput.cpp $(incdir)/CmdInput.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/File.o: $(libdir)/File.cpp $(incdir)/File.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/Parser.o: $(libdir)/Parser.cpp $(incdir)/Parser.h $(incdir)/AST.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/Output.o: $(libdir)/Output.cpp $(incdir)/Output.h $(incdir)/AST.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/djp.o: $(libdir)/djp.cpp
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@
