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

# Darwin:
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  CXX = clang++
  CXX11FLAGS = -std=c++11 -stdlib=libc++
endif

CXX11FLAGS ?= -std=c++11

# Release type
ifeq ($(BUILDTYPE),Release)
    CFLAGS = -Wall -Wextra -Os $(CXX11FLAGS)
else
    CFLAGS = -Wall -Wextra -g3 $(CXX11FLAGS)
endif

builddir_name = out
builddir = $(builddir_name)/$(BUILDTYPE)

obj_common = $(builddir)/common/obj
obj_java = $(builddir)/java/obj
obj_scala = $(builddir)/scala/obj
obj_bytecode = $(builddir)/bytecode/obj
obj_server = $(builddir)/server/obj
obj_c4 = $(builddir)/c4/obj

bin = $(builddir)/bin

# third party projects
gtest_dir = third_party/gtest
gtest_obj = $(builddir)/third_party/gtest
protobuf_dir = third_party/protobuf
protobuf_out = $(builddir)/third_party/protobuf
protobuf_compiler = $(protobuf_out)/src/protoc

# includes
incdir_common := common/include/c4
incdir_java := java/include/c4
incdir_scala := scala/include/c4
incdir_bytecode := bytecode/include/c4
incdir_server := server/include/c4
incdir_c4 := c4/include/c4
incdir_protobuf := $(protobuf_dir)/src

INC = \
  -I$(dir $(incdir_common)) \
  -I$(dir $(incdir_java)) \
  -I$(dir $(incdir_scala)) \
  -I$(dir $(incdir_bytecode)) \
  -I$(dir $(incdir_server)) \
  -I$(incdir_protobuf) \
  -I$(dir $(incdir_c4))

libdir_common := common/lib
libdir_java := java/lib
libdir_scala := scala/lib
libdir_bytecode := bytecode/lib
libdir_server := server/lib
libdir_c4 := c4/lib

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
  $(obj_bytecode) $(obj_server) $(obj_c4) $(bin)

third_party_directories := $(gtest_obj) $(protobuf_out)

.PHONY: directories
directories:
	mkdir -p  $(c4_directories) $(third_party_directories)

# main c4
$(bin)/c4: \
  $(obj_bytecode)/BinOutput.o \
  $(obj_bytecode)/BinOutputCode.o \
  $(obj_bytecode)/ParserBin.o \
  $(obj_c4)/CmdInput.o \
  $(obj_java)/libjava.a \
  $(obj_scala)/libscala.a \
  $(obj_server)/libserver.a \
  $(obj_common)/libcommon.a \
  $(obj_c4)/c4.o
	$(CXX) \
  $(obj_c4)/CmdInput.o \
  $(obj_bytecode)/BinOutput.o \
  $(obj_bytecode)/BinOutputCode.o \
  $(obj_bytecode)/ParserBin.o \
  $(CFLAGS) \
  $(INC) \
  $(obj_c4)/c4.o -o $@ \
  $(obj_java)/libjava.a \
  $(obj_scala)/libscala.a \
  $(obj_server)/libserver.a \
  $(obj_common)/libcommon.a

# objects c4
$(obj_c4)/%.o: \
  $(libdir_c4)/%.cpp \
  $(incdir_c4)/%.h
	$(CXX) $(CFLAGS) \
  $(INC) \
  -c $< -o $@

$(obj_c4)/c4.o: $(libdir_c4)/c4.cpp
	$(CXX) $(CFLAGS) \
    $(INC) \
  -c $< -o $@

# objects common - libcommon
$(obj_common)/libcommon.a: \
  $(obj_common)/Diagnosis.o \
  $(obj_common)/LiteralSupport.o \
  $(obj_common)/SourceCodeStream.o \
  $(obj_common)/Util.o
	ar -rv $@ $+

$(obj_common)/%.o: \
  $(libdir_common)/%.cpp \
  $(incdir_common)/%.h
	$(CXX) $(CFLAGS) \
  $(INC) \
  -c $< -o $@

# objects bytecode
$(obj_bytecode)/%.o: \
  $(libdir_bytecode)/%.cpp \
  $(incdir_bytecode)/%.h
	$(CXX) $(CFLAGS) \
  $(INC) \
  -c $< -o $@

# objects java - libjava
$(obj_java)/libjava.a: \
  $(obj_java)/Indentation.o \
  $(obj_java)/Lexer.o \
  $(obj_java)/Parser.o \
  $(obj_java)/SymbolTable.o \
  $(obj_java)/EmacsOutput.o
	ar -rv $@ $+

$(obj_java)/%.o: \
  $(libdir_java)/%.cpp \
  $(incdir_java)/%.h
	$(CXX) $(CFLAGS) \
  $(INC) \
  -c $< -o $@

# objects scala - libscala
$(obj_scala)/libscala.a: \
  $(obj_scala)/ScalaIndentation.o \
  $(obj_scala)/ScalaParser.o \
  $(obj_scala)/ScalaLexer.o \
  $(obj_scala)/ScalaEmacsOutput.o \
  $(obj_scala)/ScalaSyntaxHighlighting.o
	ar -rv $@ $+

$(obj_scala)/%.o: \
  $(libdir_scala)/%.cpp \
  $(incdir_scala)/%.h
	$(CXX) $(CFLAGS) \
  $(INC) \
  -c $< -o $@

# objects server - libserver
$(obj_server)/libserver.a: \
  $(obj_server)/RequestBuffer.o \
  $(obj_server)/Server.o
	ar -rv $@ $+

$(obj_server)/RequestBuffer.o: \
  $(libdir_server)/RequestBuffer.cpp \
  $(incdir_server)/RequestBuffer.h \
  $(incdir_server)/Request.pb.h
	$(CXX) $(CFLAGS) \
  $(INC) \
  -c $< -o $@

$(obj_server)/Server.o: \
  $(libdir_server)/Server.cpp \
  $(incdir_server)/Server.h \
  $(libdir_server)/ServerBSD.cpp \
  $(libdir_server)/ServerLinux.cpp \
  $(incdir_server)/Request.pb.h \
  $(incdir_server)/Response.pb.h
	$(CXX) $(CFLAGS) \
  $(INC) \
  -c $< -o $@

$(obj_server)/%.o: \
  $(libdir_server)/%.cpp \
  $(incdir_server)/%.h
	$(CXX) $(CFLAGS) \
  $(INC) \
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

	$(CXX) $(CXXTESTFLAGS) \
    $(INC) \
    -I$(gtest_inc) \
  $(tests_java)/JavaTests.cpp \
  -o $@ \
  $(obj_java)/libjava.a \
  $(obj_common)/libcommon.a \
  $(gtest_obj)/libgtest.a \
  -lpthread

$(bin)/test-scala: \
  $(tests_scala)/ScalaTests.cpp \
  $(tests_scala)/ScalaParserTest.cpp \
  $(tests_scala)/ScalaIndentationTest.cpp \
  $(tests_scala)/ScalaSyntaxHighlightingTest.cpp \
  $(obj_common)/libcommon.a \
  $(obj_scala)/libscala.a \
  $(gtest_obj)/libgtest.a

	$(CXX) $(CXXTESTFLAGS) \
    $(INC) \
    -I$(gtest_inc) \
  $(tests_scala)/ScalaTests.cpp \
  -o $@ \
  $(obj_scala)/libscala.a \
  $(obj_common)/libcommon.a \
  $(gtest_obj)/libgtest.a \
  -lpthread

$(bin)/test-server: \
  $(tests_server)/ServerTest.cpp \
  $(obj_common)/libcommon.a \
  $(obj_server)/libserver.a \
  $(gtest_obj)/libgtest.a

	$(CXX) $(CXXTESTFLAGS) \
    $(INC) \
    -I$(gtest_inc) \
  $(tests_server)/ServerTest.cpp \
  -o $@ \
  $(gtest_obj)/libgtest.a \
  $(obj_server)/libserver.a \
  $(obj_common)/libcommon.a \
  -lpthread

$(bin)/test-bytecode: \
  bytecode-classes \
  $(tests_bytecode)/BytecodeTests.cpp \
  $(tests_bytecode)/JHelloWorldTest.cpp \
  $(tests_bytecode)/SHelloWorldTest.cpp \
  $(gtest_obj)/libgtest.a

	$(CXX) $(CXXTESTFLAGS) \
    $(INC) \
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

.PHONY: protoc
protoc: $(protobuf_compiler)

# run configure and make from the protobuf output directory.
$(protobuf_compiler):
	cd $(protobuf_out); \
  $(CURDIR)/$(protobuf_dir)/configure --prefix=$(abspath $(protobuf_out)); \
  $(MAKE)

# Auto-generated files for the server
server_resources := $(abspath server/resources)
$(incdir_server)/Request.pb.h: $(protobuf_compiler) $(server_resources)/Request.proto
	$(protobuf_compiler) -I$(server_resources) \
  --cpp_out=$(abspath $(incdir_server)) $(server_resources)/Request.proto

$(incdir_server)/Response.pb.h: $(server_resources)/Response.proto
	$(protobuf_compiler) -I$(server_resources) \
  --cpp_out=$(abspath $(incdir_server)) $(server_resources)/Response.proto
