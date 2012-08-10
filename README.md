# Emacs Config

This is a split off of my [dotfiles](https://github.com/rjray/dotfiles) repo,
for just the emacs configuration.

This allows me to install the emacs config in places where I might not need
the full set of dot-files, or install the dot-files on systems that don't
have emacs.

### Submodules

A large part of what is here is in git submodules, see the file
<kbd>.gitmodules</kbd>. Each sub-directory under <kbd>.emacs.d/submodules</kbd>
is a git submodule.

### Other People's Code

The directory <kbd>.emacs.d/other-peoples-code</kbd> contains files that I've
copied in whole from other places. See the comments in those files for their
source. (In some cases, I couldn't find the original source.)

### My Code

The directory <kbd>.emacs.d/my-code</kbd> contains code that I've written
and/or cribbed from other sources.

### The `.emacs` File

Lastly, the file `.emacs` file is here at the top level. I try to keep this
file relatively clean. Most of it is setting up the search paths and loading
code. There is also the customization stuff that emacs itself writes when you
use the customize menus. Right now, there's some other stuff too, that I
haven't decided any better location for.
