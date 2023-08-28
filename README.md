# Emacs Config

This is a split off of my [dotfiles](https://github.com/rjray/dotfiles) repo,
for just the emacs configuration.

This allows me to install the emacs config in places where I might not need
the full set of dot-files, or install the dot-files on systems that don't
have emacs.

## Packages

I have recently switch from managing my external dependencies via
[package.el](http://wikemacs.org/wiki/Package.el), to
[use-package](https://github.com/jwiegley/use-package).
Those parts of the config are still somewhat volatile, as I keep finding bits
I did either inefficiently or outright incorrectly.

## The <kbd>init.el</kbd> File

As of 29.1, I've rewritten my configuration to keep pretty much everything in
this file. I have per-host directories that have snippets that are only for the
particular host (font settings and frame size/placement, generally). But the
parts that matter are all in here. Lightly documented in places, but of course
not enough.

## Additional Software Needed

This is my attempt at a comprehensive list of what needs to be installed for
this configuration to work.

* Basic:
    * Emacs 29.1 or later, with tree-sitter support
    * Node.js/npm
    * Homebrew (https://brew.sh/)
    * Perl 5.16.0 or newer, with `cpan`
    * Python 3.8 or newer, with `pip3`
    * Rust 1.70 or newer, with [rustup](https://rustup.rs/)
* Language Server/Eglot Support (Source):
    * Bash: [bash-language-server](https://github.com/mads-hartmann/bash-language-server) (npm)
    * C/C++: [clangd](https://clangd.llvm.org/) (Homebrew)
    * Clojure: [clojure-lsp](https://clojure-lsp.io/) (Homebrew)
    * CSS: [css-languageserver](https://github.com/hrsh7th/vscode-langservers-extracted) (npm)
    * Dockerfile: [docker-langserver](https://github.com/rcjsuen/dockerfile-language-server-nodejs) (npm)
    * Go: [gopls](https://github.com/golang/tools/tree/master/gopls) (Homebrew)
    * HTML: [html-languageserver](https://github.com/hrsh7th/vscode-langservers-extracted) (npm)
    * Java: [Eclipse JDT Language Server](https://github.com/eclipse/eclipse.jdt.ls) (Homebrew)
    * JavaScript/TypeScript: [TS & JS Language Server](https://github.com/theia-ide/typescript-language-server) (npm)
    * JSON: [vscode-json-languageserver](https://github.com/hrsh7th/vscode-langservers-extracted) (npm)
    * LaTeX: [texlab](https://github.com/latex-lsp/texlab) (Homebrew)
    * Markdown: [marksman](https://github.com/artempyanykh/marksman) (Homebrew)
    * Perl: [Perl::LanguageServer](https://github.com/richterger/Perl-LanguageServer) (cpan)
    * Python: Requires the following module installations:
        * `pip3 install "python-lsp-server[all]"`
        * `pip3 install pylsp-mypy pylsp-rope python-lsp-ruff`
        * `pip3 install python-lsp-black`
    * Rust: [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer) (rustup)
    * YAML: [yaml-language-server](https://github.com/redhat-developer/yaml-language-server) (Homebrew)
* Other External Programs (Source):
    * Ripgrep: [rg](https://github.com/BurntSushi/ripgrep) (Homebrew)
    * FD: [fd](https://github.com/sharkdp/fd) (Homebrew)

The script `setup_deps.sh` in this directory will check for and install these
dependencies (except for the basic items). It will check for the installer
programs (Homebrew, npm, etc.) and only install those elements that can be
installed. It does not install Emacs, Homebrew, Node.js, Perl, Python, or Rust.
It will install the latest OpenJDK and Go from Homebrew along with their
Eglot-supporting tools.

Note that while `texlab` is for LaTeX, no TeX/LaTeX system is installed
automatically by the setup script. This is because `texlab` does not require
such an installation in order to operate. It is left to the user to choose a
preferred TeX system.

Emacs 29.1 with tree-sitter support can be installed via Homebrew if the
distribution doesn't offer it.

## Build/Installation Notes

Notes on any issues found with getting the above to work.

### `pdf-tools` installation

The configuration now includes use of the `pdf-tools` package for viewing PDF
files. On Ubuntu with Homebrew installed, this failed to build the server
piece of the package (`epdfinfo`). The solution was to build the server (from
within the installation directory) with Homebrew paths moved to later in the
sequence within `$PATH`. See
[this reddit thread](https://www.reddit.com/r/emacs/comments/162xd2f/installing_pdftools_on_ubuntu_when_one_has/)
for the details.

## License

This is licensed under the MIT License. See the file `LICENSE` for details.
