Djp
================================================================================

Djp is a collection of emacs modes and parsers for Java 7, Scala 2.10 and the JVM bytecode.

The java emacs mode provides:

- Syntax highlighting
- Indentation

Scala support is under development.


A few notes on variables used in this README
--------------------------------------------------------------------------------

In the examples below I'll mention two variables: $SRC and $BIN.

The variable $SRC is the location of the 'src' directory in the project.

If you checkout the project like this:

```bash
cd ~
git clone git@github.com:dberg/djp.git
SRC=~/djp/src
```

$SRC is the path to /home/your_user/djp/src.

The variable $BIN is the location of the 'bin' directory in the project.

The $BIN directory contains the binaries produced by the build process.

The path to bin is affected by the BUILDTYPE, which has two possible values: Debug or Release. Check the Build section for more information.

Example. Assuming your username is 'ichigo' and you run the build in Release mode:

```bash
SRC=/home/ichigo/djp/src
BIN=/home/ichigo/djp/src/out/Release/bin
```


Build
--------------------------------------------------------------------------------

To build djp in Release mode:

```bash
git clone git://github.com/dberg/djp.git
cd djp/src
make BUILDTYPE=Release
```

The default BUILDTYPE is Debug and will produce binaries with debugging symbols. The Release mode will produce optmized binaries.

The default compiler is clang if you're running OSX and gcc if you're running GNU/Linux.

You can set the flag CXX to indicate your compiler of choice. For example:

```bash
make CXX=clang
# or
make CXX=g++
```


Java emacs mode (djp-mode) installation
-------------------------------------------------------------------------------

1. Open the file $SRC/emacs-modes/djp-mode/djp-mode.el and set the variable

```elisp
(defvar djp-executable "$BIN/djp")
```

2. Add the following to your .emacs or init.el file:

```elisp
(add-to-list 'load-path "$SRC/emacs-modes/djp-mode")
(autoload 'djp-mode "djp-mode" "Major mode for Java." t)
(add-to-list 'auto-mode-alist '("\\.java$" . djp-mode))
```


Running from the command line
-------------------------------------------------------------------------------

```bash
# Print usage information
$BIN/djp --help

# Print the emacs output of a java source file
$BIN/djp -i java -f MyClass.java
```


Unit Tests
-------------------------------------------------------------------------------

To run all tests execute

```bash
$BIN/run-all
```
