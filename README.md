# Cryptol Major Mode for Emacs

This package provides an Emacs major mode for [Cryptol][] - a DSL for
writing cryptographic code.

[Cryptol]: http://corp.galois.com/cryptol/

# Installation

There are two ways to get this package: via marmalade and manual
installation.

## Installation via Marmalade

If you're using [marmalade][] (and if you're not, *why not?*) then
installation is as easy as doing `M-x package-install cryptol-mode`.

[marmalade]: http://marmalade-repo.org/

## Manual installation

Put `cryptol-mode.el` somewhere on your `load-path` and then just add:

```lisp
(require 'cryptol-mode)
```

to your `.emacs`.

# Usage

Currently supported functionality is:

  * Load the current file into the cryptol REPL: `C-c C-l`

This will expect the `cryptol` executable to be in your `$PATH`. You
can customize this (and various other aspects of the mode) by
executing `M-x customize-group RET cryptol-mode RET`.

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/cryptol-mode.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/cryptol-mode.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/cryptol-mode/master/AUTHORS.txt).

# License

MIT. See
[LICENSE.txt](https://raw.github.com/thoughtpolice/cryptol-mode/master/LICENSE.txt)
for terms of copyright and redistribution.

[contribute]: https://github.com/thoughtpolice/cryptol-mode/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/cryptol-mode/issues
[gh]: http://github.com/thoughtpolice/cryptol-mode
[bb]: http://bitbucket.org/thoughtpolice/cryptol-mode
