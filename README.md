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

There are three ways of installing this package:

  * [marmalade][] via `package.el`.
  * [MELPA][] via `package.el` (if you want to track the git
    repository.)
  * Manual installation.

Emacs v24 has `package.el` preinstalled. You will need to configure
the mentioned repositories yourself (refer to the homepages.) If
you're not using Emacs v24 yet, then you can still install manually.

## Installation via Marmalade/MELPA

If you've configured your repositories, you need to decide what you
want:

  * If you want the latest stable version, **you should use
    marmalade**.
  * If you want to track the git repository, **you should use MELPA**.

If you've configure marmalade, then installation is as easy as `M-x
package-install RET cryptol-mode`. The same is true for MELPA.

If you're using marmalde and MELPA, then MELPA's build will override
the stable version. You can exclude MELPA's `cryptol-mode` using
[melpa.el](https://github.com/milkypostman/melpa#melpa-package), by
setting your `package-archive-exclude-alist` to include `("melpa"
. cryptol-mode)`.

## Manual installation

Even if you don't use `package.el`, installation is pretty
simple. Just put `cryptol-mode.el` somewhere on your `load-path` and
then add:

```lisp
(require 'cryptol-mode)
```

to your `.emacs`. You're done.

# Supported versions

`cryptol-mode` supports Cryptol 1 (tested with 1.8.27-1) and Cryptol 2
(tested with 2.0.0). The academic and evaluation versions of Cryptol 1
(1.8.23, 1.8.25) should work but may have other bugs.

# Usage

Open up any `.cry` file to get dropped into `cryptol-mode`
automatically. Cryptol batch files (`.scr` extension) are also
supported. Literate files are not yet supported.

Currently supported functionality is:

  * Load the current file into the cryptol REPL: `C-c C-l`
  * Insert type signature of top-level declaration under point: `M-x
    t` (**Note**: This is only supported with Cryptol 1 at the
    moment.)

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
[MELPA]: http://melpa.milkbox.net/
[contribute]: https://github.com/thoughtpolice/cryptol-mode/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/cryptol-mode/issues
[gh]: http://github.com/thoughtpolice/cryptol-mode
[bb]: http://bitbucket.org/thoughtpolice/cryptol-mode
