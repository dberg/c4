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
# The default compiler on linux is g++. To use clang override the CXX variable
# make CXX=clang++
# On osx the default compiler is clang.
#
###############################################################################
MAKEFLAGS=-r

CXX ?= g++
BUILDTYPE ?= Debug

# If we're running OSX we try to understand if we're running homebrew or
# macports.
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  CXX = clang++
  CXX11FLAGS = -std=c++11 -stdlib=libc++
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

CXX11FLAGS ?= -std=c++11
INC_EXTRA ?=
LIB_EXTRA ?=

# Release type
ifeq ($(BUILDTYPE),Release)
    CFLAGS = -Wall -Wextra -Os $(CXX11FLAGS)
else
    CFLAGS = -Wall -Wextra -g3 $(CXX11FLAGS)
endif

builddir_name = out
builddir = $(builddir_name)/$(BUILDTYPE)

obj_common = $(builddir)/common
obj_bytecode = $(builddir)/bytecode/obj
obj_java = $(builddir)/obj/java
obj_scala = $(builddir)/obj/scala
obj_server = $(builddir)/obj/server
obj_main = $(builddir)/obj/main

bin = $(builddir)/bin

# third party projects
gtest_dir = third_party/gtest
gtest_obj = $(builddir)/third_party/gtest
protobuf_compiler = protoc

# includes
incdir_common := include/c4/common
incdir_bytecode := include/c4/bytecode
incdir_java := include/c4/java
incdir_scala := include/c4/scala
incdir_server := include/c4/server
incdir_main := include/c4/main

INC = $(INC_EXTRA) -Iinclude -Ithird_party

libdir_common := lib/common
libdir_java := lib/java
libdir_scala := lib/scala
libdir_bytecode := lib/bytecode
libdir_server := lib/server
libdir_main := lib/main

# Tests
# Google Test r682
# http://code.google.com/p/googletest/
gtest_inc = $(gtest_dir)/include
tests_bytecode = tests/bytecode
tests_java = tests/java
tests_scala = tests/scala
tests_server = tests/server
CXXTESTFLAGS = -g3 $(CXX11FLAGS)

.PHONY: all
all: directories $(bin)/c4 $(bin)/test-all

.PHONY: c4
c4: directories $(bin)/c4

# Only delete c4 data and skip removing third party compiled code.
.PHONY: clean
clean:
	$(RM) -r $(c4_directories)

# Nukes everything.
.PHONY: clean-all
clean-all:
	$(RM) -r $(builddir_name)

# List of directories to be created or deleted.
c4_directories := \
  $(obj_common) $(obj_java) $(obj_scala) \
  $(obj_bytecode) $(obj_server) $(obj_main) $(bin)

third_party_directories := $(gtest_obj)

.PHONY: directories
directories:
	mkdir -p $(c4_directories) $(third_party_directories)

# main c4
$(bin)/c4: \
  $(obj_bytecode)/BinOutput.o \
  $(obj_bytecode)/BinOutputCode.o \
  $(obj_bytecode)/ParserBin.o \
  $(obj_main)/CmdInput.o \
  $(obj_java)/libjava.a \
  $(obj_scala)/libscala.a \
  $(obj_server)/libserver.a \
  $(obj_common)/libcommon.a \
  $(obj_main)/c4.o
	$(CXX) \
  $(obj_main)/CmdInput.o \
  $(obj_bytecode)/BinOutput.o \
  $(obj_bytecode)/BinOutputCode.o \
  $(obj_bytecode)/ParserBin.o \
  $(CFLAGS) \
  $(INC) \
  $(obj_main)/c4.o -o $@ \
  $(obj_java)/libjava.a \
  $(obj_scala)/libscala.a \
  $(obj_server)/libserver.a \
  $(obj_common)/libcommon.a \
  $(LIB_EXTRA) -lprotobuf

# objects main
$(obj_main)/%.o: \
  $(libdir_main)/%.cpp \
  $(incdir_main)/%.h
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

$(obj_main)/c4.o: $(libdir_main)/c4.cpp
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

# objects common - libcommon
$(obj_common)/libcommon.a: \
  $(obj_common)/Log.o \
  $(obj_common)/Project.o \
  $(obj_common)/ProjectHandler.o \
  $(obj_common)/ProjectJava.o \
  $(obj_common)/ProjectScala.o \
  $(obj_common)/Util.o
	ar -rv $@ $+

$(obj_common)/%.o: \
  $(libdir_common)/%.cpp \
  $(incdir_common)/%.h
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

# objects bytecode
$(obj_bytecode)/%.o: \
  $(libdir_bytecode)/%.cpp \
  $(incdir_bytecode)/%.h
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

# objects java - libjava
$(obj_java)/libjava.a: \
  $(obj_java)/Diagnosis.o \
  $(obj_java)/Indentation.o \
  $(obj_java)/Lexer.o \
  $(obj_java)/LiteralSupport.o \
  $(obj_java)/Parser.o \
  $(obj_java)/SourceCodeStream.o \
  $(obj_java)/SymbolTable.o \
  $(obj_java)/EmacsOutput.o
	ar -rv $@ $+

$(obj_java)/%.o: \
  $(libdir_java)/%.cpp \
  $(incdir_java)/%.h
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

# objects scala - libscala
$(obj_scala)/libscala.a: \
  $(obj_scala)/CompilationUnits.o \
  $(obj_scala)/Global.o \
  $(obj_scala)/Phase.o \
  $(obj_scala)/SourceFile.o \
  $(obj_scala)/SubComponent.o \
  $(obj_scala)/SyntaxAnalyzer.o
	ar -rv $@ $+

$(obj_scala)/%.o: \
  $(libdir_scala)/%.cpp \
  $(incdir_scala)/%.h
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

# objects server - libserver
$(obj_server)/libserver.a: \
  $(obj_server)/RequestBuffer.o \
  $(obj_server)/Request.o \
  $(obj_server)/Response.o \
  $(obj_server)/Server.o
	ar -rv $@ $+

$(obj_server)/RequestBuffer.o: \
  $(libdir_server)/RequestBuffer.cpp \
  $(incdir_server)/RequestBuffer.h \
  $(obj_server)/Request.o
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

$(obj_server)/Server.o: \
  $(libdir_server)/Server.cpp \
  $(incdir_server)/Server.h \
  $(libdir_server)/ServerBSD.cpp \
  $(libdir_server)/ServerLinux.cpp \
  $(obj_server)/Request.o \
  $(obj_server)/Response.o
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

$(obj_server)/Request.o: \
  $(incdir_server)/Request.pb.h \
  $(incdir_server)/Request.pb.cc
	$(CXX) $(CFLAGS) $(INC) \
  -c $(incdir_server)/Request.pb.cc -o $@

$(obj_server)/Response.o: \
  $(incdir_server)/Response.pb.h \
  $(incdir_server)/Response.pb.cc
	$(CXX) $(CFLAGS) $(INC) \
  -c $(incdir_server)/Response.pb.cc -o $@

$(obj_server)/%.o: \
  $(libdir_server)/%.cpp \
  $(incdir_server)/%.h
	$(CXX) $(CFLAGS) $(INC) \
  -c $< -o $@

# tests
.PHONY: bytecode-classes
bytecode-classes:
	mkdir -p $(bin)/bytecode-classes
	cp -r $(tests_bytecode)/bytecode-classes/* $(bin)/bytecode-classes/

.PHONY: run-all
run-all:
	cp tests/run-all.sh.tmpl $(bin)/run-all.sh
	chmod +x $(bin)/run-all.sh

.PHONY: $(bin)/test-all
$(bin)/test-all: run-all $(bin)/test-java \
                         $(bin)/test-scala \
                         $(bin)/test-bytecode \
                         $(bin)/test-server

$(bin)/test-java: \
  $(tests_java)/JavaTests.cpp \
  $(tests_java)/EmacsOutputTest.cpp \
  $(tests_java)/IndentationTest.cpp \
  $(tests_java)/ParserTest.cpp \
  $(tests_java)/SymbolTableTest.cpp \
  $(obj_common)/libcommon.a \
  $(obj_java)/libjava.a \
  $(gtest_obj)/libgtest.a

	$(CXX) $(CXXTESTFLAGS) $(INC) \
    -I$(gtest_inc) \
  $(tests_java)/JavaTests.cpp \
  -o $@ \
  $(obj_java)/libjava.a \
  $(obj_common)/libcommon.a \
  $(gtest_obj)/libgtest.a \
  -lpthread

$(bin)/test-scala: \
  $(tests_scala)/ScalaTests.cpp \
  $(tests_scala)/ScalaCompilerTest.cpp \
  $(obj_common)/libcommon.a \
  $(obj_scala)/libscala.a \
  $(gtest_obj)/libgtest.a

	$(CXX) $(CXXTESTFLAGS) $(INC) \
    -I$(gtest_inc) \
  $(tests_scala)/ScalaTests.cpp \
  -o $@ \
  $(obj_scala)/libscala.a \
  $(obj_common)/libcommon.a \
  $(gtest_obj)/libgtest.a \
  -lpthread

# We reference libcommon twice because it's a circular dependency and we want to
# make ld happy. Joy.
$(bin)/test-server: \
  $(tests_server)/ServerTest.cpp \
  $(obj_common)/libcommon.a \
  $(obj_java)/libjava.a \
  $(obj_scala)/libscala.a \
  $(obj_server)/libserver.a \
  $(gtest_obj)/libgtest.a

	$(CXX) $(CXXTESTFLAGS) $(INC) \
    -I$(gtest_inc) \
  $(tests_server)/ServerTest.cpp \
  -o $@ \
  $(gtest_obj)/libgtest.a \
  $(obj_server)/libserver.a \
  $(obj_common)/libcommon.a \
  $(obj_java)/libjava.a \
  $(obj_scala)/libscala.a \
  $(obj_common)/libcommon.a \
  $(LIB_EXTRA) -lprotobuf \
  -lpthread

$(bin)/test-bytecode: \
  bytecode-classes \
  $(tests_bytecode)/BytecodeTests.cpp \
  $(tests_bytecode)/JHelloWorldTest.cpp \
  $(tests_bytecode)/SHelloWorldTest.cpp \
  $(gtest_obj)/libgtest.a

	$(CXX) $(CXXTESTFLAGS) $(INC) \
    -I$(gtest_inc) \
  $(tests_bytecode)/BytecodeTests.cpp \
  $(obj_bytecode)/ParserBin.o \
  -o $@ \
  $(gtest_obj)/libgtest.a \
  -lpthread -ldl

# third party
$(gtest_obj)/libgtest.a: $(gtest_obj)/gtest-all.o
	ar -rv $@ $<

$(gtest_obj)/gtest-all.o: $(gtest_dir)/src/gtest-all.cc
	$(CXX) $(CXXTESTFLAGS) \
    -I$(gtest_dir)/include \
    -I$(gtest_dir) \
  -c $< -o $@

# Auto-generated files for the server
server_resources := $(abspath lib/resources)
$(incdir_server)/Request.pb.h: $(server_resources)/Request.proto
	$(protobuf_compiler) -I$(server_resources) \
  --cpp_out=$(abspath $(incdir_server)) $(server_resources)/Request.proto

$(incdir_server)/Response.pb.h: $(server_resources)/Response.proto
	$(protobuf_compiler) -I$(server_resources) \
  --cpp_out=$(abspath $(incdir_server)) $(server_resources)/Response.proto
