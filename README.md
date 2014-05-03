## C4

C4 is a collection of emacs modes and parsers for Java 7, Scala 2.10 and the JVM bytecode.

The java emacs mode provides:

- Syntax highlighting
- Indentation

Scala support is under development.


### A few notes on variables used in this README

In the examples below I'll mention two variables: $SRC and $BIN.

The variable $SRC is the location of the 'src' directory in the project.

If you checkout the project like this:

```bash
cd ~
git clone git@github.com:dberg/c4.git
SRC=~/c4/src
```

$SRC is the path to /home/your_user/c4/src.

The variable $BIN is the location of the 'bin' directory in the project.

The $BIN directory contains the binaries produced by the build process.

The path to bin is affected by the variable BUILDTYPE, which has two possible values: Debug or Release. Check the Build section for more information.

Example. Assuming your username is 'ichigo' and you run the build in Release mode:

```bash
SRC=/home/ichigo/c4/src
BIN=/home/ichigo/c4/src/out/Release/bin
```


### Build

Prerequisite:

Install Protocol Buffers before compiling c4. See https://developers.google.com/protocol-buffers/ for more information.

```bash
# Fedora
sudo yum install protobuf-compiler protobuf-devel
# OSX running macports
sudo port install protobuf-cpp
```

To build c4 in Release mode:

```bash
git clone git://github.com/dberg/c4.git
cd c4/src
make BUILDTYPE=Release
```

The default value for BUILDTYPE is Debug and will produce binaries with debugging symbols. The Release mode will produce optimized binaries.

The default compiler is clang if you're running OSX and gcc if you're running GNU/Linux.

You can set the flag CXX to use your compiler of choice. For example:

```bash
make CXX=clang++
# or
make CXX=g++
```


### Java emacs mode (c4j-mode) installation

1. Open the file $SRC/emacs-modes/c4j-mode/c4j-mode.el and set the variable

```elisp
(defvar c4j-executable "$BIN/c4")
```

2. Add the following to your .emacs or init.el file:

```elisp
(add-to-list 'load-path "$SRC/emacs-modes/c4j-mode")
(autoload 'c4j-mode "c4j-mode" "Major mode for Java." t)
(add-to-list 'auto-mode-alist '("\\.java$" . c4j-mode))
```


### Running c4 from the command line

```bash
# Print usage information
$BIN/c4 --help

# Print the emacs output of a java source file
$BIN/c4 -i java -f MyClass.java
```


### Unit Tests

To run all tests execute

```bash
$BIN/run-all
```
