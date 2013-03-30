0.0.3 - Not yet released
------------------------

  * `cryptol-mode` now works on Emacs 23 and below (issue #2.)
  * Fix (some) libedit stupidity on Linux which broke `M-x
    cryptol-version` and `M-x cryptol-backends` (issue #13.)

0.0.2 - Released March 3rd, 2013
--------------------------------

  * Fix release date information.

0.0.1 - Released March 3rd, 2013
--------------------------------

  * Initial, super primitive version.
    - REPL is supported via `C-c C-l` in Cryptol mode or `M-x cryptol-repl`.
	- Basic syntax highlighting.
    - Version and backend information via `M-x cryptol-version` and
      `M-x cryptol-backends`.
    - You can customize where the `cryptol` executable is with `M-x
      customize-group RET cryptol-mode RET`.
