BUILDTYPE ?= Debug

# Headers and static lib from djp
djp_include := ../include
djp_lib := ../out/$(BUILDTYPE)/obj/libdjp.a

# Release type
ifeq ($(BUILDTYPE),Release)
    CFLAGS = -Wall -Wextra -O3
else
    CFLAGS = -Wall -Wextra -g3
endif

builddir_name := out
builddir := $(builddir_name)/$(BUILDTYPE)
obj := $(builddir)/obj
bin := $(builddir)/bin
INC := -I$(djp_include)

# Darwin
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
INC := $(INC) -I/opt/local/include
endif

.PHONY: all
all: directories $(bin)/ListClassMethods

.PHONY: clean
clean:
	$(RM) -r $(builddir_name)

.PHONY: directories
directories:
	mkdir -p $(obj) $(bin)

$(bin)/ListClassMethods: $(obj)/ListClassMethods.o
	$(CXX) $(CFLAGS) $(INC) $< -o $@ $(djp_lib)

$(obj)/ListClassMethods.o: ListClassMethods.cpp
	$(CXX) $(CFLAGS) $(INC) -c $< -o $@
