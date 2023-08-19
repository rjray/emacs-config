# Emacs Config

This is a split off of my [dotfiles](https://github.com/rjray/dotfiles) repo,
for just the emacs configuration.

This allows me to install the emacs config in places where I might not need
the full set of dot-files, or install the dot-files on systems that don't
have emacs.

### Packages

I have recently switch from managing my external dependencies via
[package.el](http://wikemacs.org/wiki/Package.el), to
[use-package](https://github.com/jwiegley/use-package#modes-and-interpreters).
Those parts of the config are still somewhat volatile, as I keep finding bits
I did either inefficiently or outright incorrectly.

### The <kbd>init.el</kbd> File

As of 29.1, I've rewritten my configuration to keep pretty much everything in
this file. I have per-host directories that have snippets that are only for the
particular host (font settings and frame size/placement, generally). But the
parts that matter are all in here. Lightly documented in places, but of course
not enough.
