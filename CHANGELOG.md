0.1.0 - Released April 26th, 2014
------------------------

  * Preliminary Cryptol 2 support (issue #15). Cryptol 1 is still
    supported.
  * Preliminary Support for inserting the inferred type signature of
    top-level declarations. By default, this is bound to `M-x t`, and
    only works for top-level definitions.

    To use, put your cursor over the name of a top-level declaration,
    and hit `M-x t` to insert the inferred type signature.

    **Note**: Currently this does not yet work with Cryptol 2.
  * `cryptol-mode` now works on Emacs 23 and below (issue #2.)
  * Fix (some) libedit stupidity on Linux which broke `M-x
    cryptol-version` and `M-x cryptol-backends` (issue #13.)
  * Improved operator and constructor highlighting.
  * `imenu` support now identifies Cryptol 1 theorems and Cryptol 2 properties.
  * `imenu` now auto-rescans buffers for properties/theorems.
  * Bug fixes to `imenu` indexing

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
