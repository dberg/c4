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
builddir_common = $(builddir)/common
builddir_java = $(builddir)/java
builddir_scala = $(builddir)/scala
builddir_bytecode = $(builddir)/bytecode
builddir_djp = $(builddir)/djp

obj_common = $(builddir)/common/obj
obj_java = $(builddir)/java/obj
obj_scala = $(builddir)/scala/obj
obj_bytecode = $(builddir)/bytecode/obj
obj_djp = $(builddir)/djp/obj

bin = $(builddir)/bin

incdir_common := common/include/djp
incdir_java := java/include/djp
incdir_scala := scala/include/djp
incdir_bytecode := bytecode/include/djp
incdir_djp := djp/include/djp
INC = -I$(dir $(incdir_common)) -I$(dir $(incdir_java)) \
  -I$(dir $(incdir_scala)) -I$(dir $(incdir_bytecode)) \
  -I$(dir $(incdir_djp))

libdir_common := common/lib
libdir_java := java/lib
libdir_scala := scala/lib
libdir_bytecode := bytecode/lib
libdir_djp := djp/lib

# Tests
# Google Test r652
# http://code.google.com/p/googletest/
gtest_dir = third_party/gtest
gtest_obj = $(builddir)/third_party/gtest
gtest_inc = $(gtest_dir)/include
test_libdir = tests/lib
CXXTESTFLAGS = -g3 $(CXX11FLAGS)

.PHONY: all
all: directories $(bin)/djp $(bin)/tests

.PHONY: djp
djp: directories $(bin)/djp

.PHONY: clean
clean:
	$(RM) -r $(builddir_name)

.PHONY: directories
directories:
	mkdir -p $(obj_common) $(obj_java) $(obj_scala) \
          $(obj_bytecode) $(obj_djp) $(bin) $(gtest_obj)

# main djp
$(bin)/djp: $(obj_bytecode)/BinOutput.o \
            $(obj_bytecode)/BinOutputCode.o \
            $(obj_bytecode)/ParserBin.o \
            $(obj_common)/libcommon.a \
            $(obj_java)/libjava.a \
            $(obj_scala)/libscala.a \
            $(obj_djp)/CmdInput.o \
            $(obj_djp)/djp.o
	$(CXX) $(obj_djp)/CmdInput.o \
               $(obj_bytecode)/BinOutput.o \
               $(obj_bytecode)/BinOutputCode.o \
               $(obj_bytecode)/ParserBin.o \
               $(CFLAGS) \
               $(INC) \
               $(obj_djp)/djp.o -o $@ \
               $(obj_java)/libjava.a \
               $(obj_scala)/libscala.a \
               $(obj_common)/libcommon.a

# objects djp
$(obj_djp)/CmdInput.o: $(libdir_djp)/CmdInput.cpp $(incdir_djp)/CmdInput.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_djp)/djp.o: $(libdir_djp)/djp.cpp
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

# objects common - libcommon
$(obj_common)/libcommon.a: $(obj_common)/SourceCodeStream.o
	ar -rv $@ $+

$(obj_common)/SourceCodeStream.o: $(libdir_common)/SourceCodeStream.cpp \
                                  $(incdir_common)/SourceCodeStream.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

# objects bytecode
$(obj_bytecode)/ParserBin.o: $(libdir_bytecode)/ParserBin.cpp \
                             $(incdir_bytecode)/ParserBin.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_bytecode)/BinOutput.o: $(libdir_bytecode)/BinOutput.cpp \
                             $(incdir_bytecode)/BinOutput.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_bytecode)/BinOutputCode.o: $(libdir_bytecode)/BinOutputCode.cpp \
                                 $(incdir_bytecode)/BinOutputCode.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

# objects java - libjava
$(obj_java)/libjava.a: $(obj_java)/Util.o \
                       $(obj_java)/Diagnosis.o \
                       $(obj_java)/Indentation.o \
                       $(obj_java)/Lexer.o \
                       $(obj_java)/LiteralSupport.o \
                       $(obj_java)/Parser.o \
                       $(obj_java)/SymbolTable.o \
                       $(obj_java)/EmacsOutput.o
	ar -rv $@ $+

$(obj_java)/Util.o: $(libdir_java)/Util.cpp $(incdir_java)/Util.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_java)/Diagnosis.o: $(libdir_java)/Diagnosis.cpp $(incdir_java)/Diagnosis.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_java)/Indentation.o: $(libdir_java)/Indentation.cpp \
                           $(incdir_java)/Indentation.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_java)/Lexer.o: $(libdir_java)/Lexer.cpp $(incdir_java)/Lexer.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_java)/LiteralSupport.o: $(libdir_java)/LiteralSupport.cpp \
                              $(incdir_java)/LiteralSupport.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_java)/Parser.o: $(libdir_java)/Parser.cpp $(incdir_java)/Parser.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_java)/EmacsOutput.o: $(libdir_java)/EmacsOutput.cpp \
                           $(incdir_java)/EmacsOutput.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_java)/SymbolTable.o: $(libdir_java)/SymbolTable.cpp \
                           $(incdir_java)/SymbolTable.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

# objects scala - libscala
$(obj_scala)/libscala.a: $(obj_scala)/ScalaParser.o $(obj_scala)/ScalaLexer.o
	ar -rv $@ $+

$(obj_scala)/ScalaParser.o: $(libdir_scala)/ScalaParser.cpp \
                            $(incdir_scala)/ScalaParser.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

$(obj_scala)/ScalaLexer.o: $(libdir_scala)/ScalaLexer.cpp \
                           $(incdir_scala)/ScalaParser.h
	$(CXX) $(CFLAGS) $(INC) \
            -c $< -o $@

# tests
.PHONY: test-classes
test-classes:
	mkdir -p $(bin)/test-classes
	cp tests/test-classes/*.class $(bin)/test-classes

$(bin)/tests: test-classes \
              $(test_libdir)/AllTests.cpp \
              $(test_libdir)/EmacsOutputTest.cpp \
              $(test_libdir)/IndentationTest.cpp \
              $(test_libdir)/ParserTest.cpp \
              $(test_libdir)/ParserBinTest.cpp \
              $(test_libdir)/SymbolTableTest.cpp \
              $(obj_common)/libcommon.a \
              $(obj_java)/libjava.a \
              $(obj_scala)/libscala.a \
              $(gtest_obj)/libgtest.a

	$(CXX) $(CXXTESTFLAGS) $(INC) -I$(gtest_inc) \
            $(obj_bytecode)/ParserBin.o \
            $(test_libdir)/AllTests.cpp \
            -o $@ \
            $(obj_java)/libjava.a \
            $(gtest_obj)/libgtest.a \
            $(obj_common)/libcommon.a \
            -lpthread -ldl

$(gtest_obj)/libgtest.a: $(gtest_obj)/gtest-all.o
	ar -rv $@ $<

$(gtest_obj)/gtest-all.o: $(gtest_dir)/src/gtest-all.cc
	$(CXX) $(CXXTESTFLAGS) -I$(gtest_dir)/include -I$(gtest_dir) \
            -c $< -o $@
