# Cryptol Major Mode for Emacs

This package provides an Emacs major mode for [Cryptol][] - a DSL for
writing cryptographic code. From the homepage:

> Cryptol is a domain-specific language (DSL) and tool suite that
> simplifies the specification of a cryptographic algorithm and then
> compiles the specification into VHDL... Cryptol is fully executable,
> allowing designers to experiment with their programs incrementally
> as their designs evolve. Cryptol compilers can generate C, C++, and
> Haskell software implementations, and VHDL or Verilog HDL hardware
> implementations... In addition to generating implementations of
> cryptographic algorithms, Cryptol tools can verify the faithfulness
> of an implementation to a reference specification, at each stage of
> the toolchain.


[Cryptol]: http://corp.galois.com/cryptol/

# Installation

There are two ways to get this package: via [marmalade][] if you're
using `package.el`, or via manual installation.

If you're using Emacs 24 or above, then installation via `package.el`
is recommended. If you're not using Emacs 24, `package.el` is still
recommended because it's awesome and I say so. But you can still do
things the old fashioned way too.

[Melpa](https://github.com/milkypostman/melpa) is not supported (yet?)

## Installation via Marmalade

If you're using [marmalade][] (and if you're not, *ask yourself why!*)
then installation is as easy as doing `M-x package-install
cryptol-mode`.

## Manual install

Even if you don't use `package.el`, installation is pretty
simple. Just put `cryptol-mode.el` somewhere on your `load-path` and
then just add:

```lisp
(require 'cryptol-mode)
```

to your `.emacs`, and you're done.

# Usage

Open up any `.cry` file to get dropped into `cryptol-mode`
automatically. Cryptol batch files (`.scr` extension) are also
supported. Literate files are not yet supported.

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

GPLv3 or later. See
[LICENSE.txt](https://raw.github.com/thoughtpolice/cryptol-mode/master/LICENSE.txt)
for terms of copyright and redistribution.

[marmalade]: http://marmalade-repo.org/
[contribute]: https://github.com/thoughtpolice/cryptol-mode/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/cryptol-mode/issues
[gh]: http://github.com/thoughtpolice/cryptol-mode
[bb]: http://bitbucket.org/thoughtpolice/cryptol-mode
