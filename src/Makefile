MAKEFLAGS=-r

CC ?= gcc
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

.PHONY: all
all: directories $(bin)/djp

.PHONY: clean
clean:
	$(RM) -r $(builddir_name)

.PHONY: directories
directories:
	mkdir -p $(obj) $(bin)

# main djp
$(bin)/djp: $(obj)/cmd_input.o \
            $(obj)/parser.o \
            $(obj)/djp.o
	$(CC) $(CFLAGS) $(INC) $(obj)/djp.o -o $@

# modules
$(obj)/cmd_input.o: $(libdir)/cmd_input.c $(incdir)/cmd_input.h
	$(CC) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/parser.o: $(libdir)/parser.c $(incdir)/parser.h
	$(CC) $(CFLAGS) $(INC) -c $< -o $@

$(obj)/djp.o: $(libdir)/djp.c
	$(CC) $(CFLAGS) $(INC) -c $< -o $@
