# Emacs Config

This is a split off of my [dotfiles](https://github.com/rjray/dotfiles) repo,
for just the emacs configuration.

This allows me to install the emacs config in places where I might not need
the full set of dot-files, or install the dot-files on systems that don't
have emacs.

### Packages

I have recently switch from managing my external dependencies as git
submodules, to using [package.el](http://wikemacs.org/wiki/Package.el) and the
[ELPA](https://www.emacswiki.org/emacs/ELPA),
[MELPA](https://www.emacswiki.org/emacs/MELPA)
and [Marmalade](https://www.emacswiki.org/emacs/MarmaladeRepo) repositories.

See the defvar of `rjray/packages` in the <kbd>.emacs.d/init.el</kbd> file for
the list of packages I am currently using.

### Other People's Code

The directory <kbd>.emacs.d/other-peoples-code</kbd> contains files that I've
copied in whole from other places. See the comments in those files for their
source.

### My Code

The directory <kbd>.emacs.d/my-code</kbd> contains code that I've written
and/or cribbed from other sources.

### The <kbd>init.el</kbd> File

I've moved the <kbd>.emacs</kbd> file to <kbd>.emacs.d/init.el</kbd>. I am
trying to keep this file relatively clean, but there is still a lot of clean-up
to do. I'm also debating moving most of the <kbd>.emacs.d/my-code</kbd> files
back into the main file; it would mean fewer `flycheck` warnings and fewer
duplicated `(require ...)` statements. It would also mean a single file of over
800 lines...
