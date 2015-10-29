## C4

C4 is an emacs mode for Java 8.


### A few notes on variables used in this README

In the examples below I'll mention two variables: `SRC` and `BIN`.

The variable `SRC` is the location of the project's `src` directory.

The variable `BIN` is the location of the `bin` directory which is created by the build process.

The path to `BIN` is affected by the variable `BUILDTYPE`, which has two possible values: `Debug` or `Release`. Check the Build section for more information.


### Build

Prerequisite:

Install Protocol Buffers before compiling c4. See https://developers.google.com/protocol-buffers/ for more information.

```bash
# Fedora
dnf -y install protobuf-devel

# OSX running macports
brew install protobuf-c
```

You can build c4 in `Debug` mode, or in `Release` mode which is the default.

```bash
git clone git://github.com/dberg/c4.git
cd c4/src
make BUILDTYPE=Release
```

The default compiler is clang if you're running OSX and gcc if you're running GNU/Linux.

You can set the flag `CXX` to use your compiler of choice. For example:

```bash
make CXX=clang++
# or
make CXX=g++
```


### Emacs mode (c4-mode) installation

1. Open the file $SRC/c4-mode/c4-mode.el and set the variable

```elisp
;; Replace BIN with the path to the c4 executable
(defvar c4-executable "BIN/c4")
```

2. Add the following to your .emacs or init.el file:

```elisp
;; Replace SRC with the path to c4-mode
(add-to-list 'load-path "SRC/c4-mode")
(autoload 'c4j-mode "c4j-mode" "C4 Major mode." t)
(add-to-list 'auto-mode-alist '("\\.java$" . c4-mode))
```


### Running c4 from the command line

```bash
# Print usage information
BIN/c4 --help

# Print the emacs output of a java source file
BIN/c4 -i java -f MyClass.java
```


### Unit Tests

To run all tests execute

```bash
BIN/run-all
```


### Docker

Check the Dockerfile for instructions on how to build c4 with Fedora.
