# C4

C4 is an emacs mode for Java 8.

## Requirements

1. C++14 compiler
1. CMake >= 3.3.2
1. Doxygen (required only for Documentation)

## Build

```bash
cd src
cmake .
make
```

### Emacs mode (c4-mode) installation

1. Open the file $C4/c4-mode/c4-mode.el and set the variable

```elisp
(defvar c4-executable "$C4/src/c4")
```

2. Add the following to your .emacs or init.el file:

```elisp
(add-to-list 'load-path "$C4/src/c4-mode")
(autoload 'c4-mode "c4-mode" "C4 Major mode." t)
(add-to-list 'auto-mode-alist '("\\.java$" . c4-mode))
```

### Running c4 from the command line

```bash
# Print usage information
$C4/src/c4 --help

# Print the emacs output of a java source file
$C4/c4 -i java -f MyClass.java
```

### Unit Tests

To run all tests execute

```bash
# TODO: add gtest
```

### Docker

Check the Dockerfile for instructions on how to build c4 with Fedora.

TODO: Check doxygen and cmake dependencies.
