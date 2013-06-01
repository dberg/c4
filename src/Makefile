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
obj = $(builddir)/obj
bin = $(builddir)/bin
incdir := include
libdir := lib
INC = -I$(incdir)

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

.PHONY: clean
clean:
	$(RM) -r $(builddir_name)

.PHONY: directories
directories:
	mkdir -p $(obj) $(bin) $(gtest_obj) $(test_obj)

# main djp
$(bin)/djp: $(obj)/CmdInput.o \
            $(obj)/BinOutput.o \
            $(obj)/BinOutputCode.o \
            $(obj)/EmacsOutput.o \
            $(obj)/SourceCodeStream.o \
            $(obj)/SymbolTable.o \
            $(obj)/djp.o \
            $(obj)/libdjp.a
	$(CXX) $(obj)/CmdInput.o \
               $(obj)/BinOutput.o \
               $(obj)/BinOutputCode.o \
               $(obj)/EmacsOutput.o \
               $(CFLAGS) $(INC) $(obj)/djp.o -o $@ $(obj)/libdjp.a

# objects
$(obj)/CmdInput.o: $(libdir)/CmdInput.cpp $(incdir)/djp/CmdInput.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/BinOutput.o: $(libdir)/BinOutput.cpp \
                    $(incdir)/djp/BinOutput.h \
                    $(incdir)/djp/ParserBin.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/BinOutputCode.o: $(libdir)/BinOutputCode.cpp \
                        $(incdir)/djp/BinOutputCode.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/EmacsOutput.o: $(libdir)/EmacsOutput.cpp \
                      $(incdir)/djp/EmacsOutput.h \
                      $(incdir)/djp/AST.h \
                      $(incdir)/djp/ErrorCodes.h \
                      $(incdir)/djp/Token.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/djp.o: $(libdir)/djp.cpp
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

# objects - libdjp
$(obj)/libdjp.a: $(obj)/Util.o \
                 $(obj)/Diagnosis.o \
                 $(obj)/Indentation.o \
                 $(obj)/Lexer.o \
                 $(obj)/LiteralSupport.o \
                 $(obj)/Parser.o \
                 $(obj)/SourceCodeStream.o \
                 $(obj)/SymbolTable.o \
                 $(obj)/ParserBin.o
	ar -rv $@ $+

$(obj)/Util.o: $(libdir)/Util.cpp $(incdir)/djp/Util.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/Diagnosis.o: $(libdir)/Diagnosis.cpp $(incdir)/djp/Diagnosis.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/Indentation.o: $(libdir)/Indentation.cpp $(incdir)/djp/Indentation.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/Lexer.o: $(libdir)/Lexer.cpp \
                $(incdir)/djp/Lexer.h \
                $(incdir)/djp/AST.h \
                $(incdir)/djp/Diagnosis.h \
                $(incdir)/djp/LiteralSupport.h \
                $(incdir)/djp/ParserState.h \
                $(incdir)/djp/SourceCodeStream.h \
                $(incdir)/djp/Token.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/LiteralSupport.o: $(libdir)/LiteralSupport.cpp \
                         $(incdir)/djp/LiteralSupport.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/Parser.o: $(libdir)/Parser.cpp \
                 $(incdir)/djp/Parser.h \
                 $(incdir)/djp/AST.h \
                 $(incdir)/djp/ErrorCodes.h \
                 $(incdir)/djp/ParserState.h \
                 $(incdir)/djp/Token.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/SourceCodeStream.o: $(libdir)/SourceCodeStream.cpp \
	                   $(incdir)/djp/SourceCodeStream.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/SymbolTable.o: $(libdir)/SymbolTable.cpp \
                 $(incdir)/djp/SymbolTable.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

# objects - binary parsing
$(obj)/ParserBin.o: $(libdir)/ParserBin.cpp $(incdir)/djp/ParserBin.h
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@

# tests
.PHONY: test-classes
test-classes:
	mkdir -p $(bin)/test-classes
	cp tests/test-classes/*.class $(bin)/test-classes

$(bin)/tests: test-classes \
              $(test_libdir)/EmacsOutputTest.cpp \
              $(test_libdir)/IndentationTest.cpp \
              $(test_libdir)/ParserTest.cpp \
              $(test_libdir)/ParserBinTest.cpp \
              $(test_libdir)/SymbolTableTest.cpp \
              $(obj)/EmacsOutput.o \
              $(obj)/Diagnosis.o \
              $(obj)/libdjp.a \
              $(gtest_obj)/libgtest.a

	$(CXX) $(CXXTESTFLAGS) $(INC) -I$(gtest_inc) $(obj)/EmacsOutput.o \
                                     $(test_libdir)/EmacsOutputTest.cpp \
                                     $(test_libdir)/IndentationTest.cpp \
                                     $(test_libdir)/ParserTest.cpp \
                                     $(test_libdir)/ParserBinTest.cpp \
                                     $(test_libdir)/SymbolTableTest.cpp \
                                     -o $@ \
                                     $(obj)/libdjp.a \
                                     $(gtest_obj)/libgtest.a \
                                     -lpthread

$(gtest_obj)/libgtest.a: $(gtest_obj)/gtest-all.o
	ar -rv $@ $<

$(gtest_obj)/gtest-all.o: $(gtest_dir)/src/gtest-all.cc
	$(CXX) $(CXXTESTFLAGS) -I$(gtest_dir)/include -I$(gtest_dir) \
               -c $< -o $@
