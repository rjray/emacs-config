# Emacs Config

This is a split off of my [dotfiles](https://github.com/rjray/dotfiles) repo,
for just the emacs configuration.

This allows me to install the emacs config in places where I might not need
the full set of dot-files, or install the dot-files on systems that don't
have emacs.

### Packages

I have recently switch from managing my external dependencies via
[package.el](http://wikemacs.org/wiki/Package.el), to
[use-package](https://github.com/jwiegley/use-package).
Those parts of the config are still somewhat volatile, as I keep finding bits
I did either inefficiently or outright incorrectly.

### The <kbd>init.el</kbd> File

As of 29.1, I've rewritten my configuration to keep pretty much everything in
this file. I have per-host directories that have snippets that are only for the
particular host (font settings and frame size/placement, generally). But the
parts that matter are all in here. Lightly documented in places, but of course
not enough.

### Additional Software Needed

This is my attempt at a comprehensive list of what needs to be installed for
this configuration to work.

* Basic:
    * Emacs 29.1 or later, with tree-sitter support
* Language Server/Eglot Support:
    * Bash: [bash-language-server](https://github.com/mads-hartmann/bash-language-server)
    * C/C++: [clangd](https://clangd.llvm.org/)
    * Clojure: [clojure-lsp](https://clojure-lsp.io/)
    * CSS: [css-languageserver](https://github.com/hrsh7th/vscode-langservers-extracted)
    * Dockerfile: [docker-langserver](https://github.com/rcjsuen/dockerfile-language-server-nodejs)
    * Go: [gopls](https://github.com/golang/tools/tree/master/gopls)
    * HTML: [html-languageserver](https://github.com/hrsh7th/vscode-langservers-extracted)
    * Java: [Eclipse JDT Language Server](https://github.com/eclipse/eclipse.jdt.ls)
    * JavaScript/TypeScript: [TS & JS Language Server](https://github.com/theia-ide/typescript-language-server)
    * JSON: [vscode-json-languageserver](https://github.com/hrsh7th/vscode-langservers-extracted)
    * LaTeX: [texlab](https://github.com/latex-lsp/texlab)
    * Markdown: [marksman](https://github.com/artempyanykh/marksman)
    * Perl: [Perl::LanguageServer](https://github.com/richterger/Perl-LanguageServer)
    * Rust: [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer)
    * YAML: [yaml-language-server](https://github.com/redhat-developer/yaml-language-server)
