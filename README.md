Djp
================================================================================

Djp is a parser and an emacs mode for Java version 7.

The emacs mode provides only two features:

- syntax highlighting
- indentation


Build
--------------------------------------------------------------------------------

```bash
git clone git://github.com/dberg/djp.git
cd djp/src
make BUILDTYPE=Release
```

Emacs mode (djp-mode) installation
-------------------------------------------------------------------------------

1. Open the file /src/djp/djp-mode/djp-mode.el and set the variable

```elisp
(defvar djp-executable "PATH/TO/src/out/Release/bin/djp")
```

2. Add the following to your .emacs file:

```elisp
(add-to-list 'load-path "~/PATH/TO/src/djp-mode")
(autoload 'djp-mode "djp-mode" "Major mode for Java." t)
(add-to-list 'auto-mode-alist '("\\.java$" . djp-mode))
```
