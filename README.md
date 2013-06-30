Djp is a parser and an emacs mode for Java version 7.

The emacs mode provides only two features:

- syntax highlighting
- indentation


Emacs mode (djp-mode) installation
-------------------------------------------------------------------------------

1) Checkout the code.
    git clone git://github.com/dberg/djp.git

2) Build it
    cd djp/src
    make BUILDTYPE=Release

3) Open the file /src/djp-mode/djp-mode.el and set the variable
    'djp-executable' to 'PATH/TO/src/out/Release/bin/djp'.

4) Add the following to your .emacs file:
    (add-to-list 'load-path "~/PATH/TO/src/djp-mode")
    (autoload 'djp-mode "djp-mode" "Major mode for Java." t)
    (add-to-list 'auto-mode-alist '("\\.java$" . djp-mode))
