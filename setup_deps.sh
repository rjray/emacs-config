#!/bin/bash

# Install various packages, programs, etc. needed to support the Emacs
# configuration.

echo -n "Checking for npm... "
if which npm 2> /dev/null 1> /dev/null; then
    echo "found."
    for pkg in bash-language-server vscode-langservers-extracted \
                                    dockerfile-language-server-nodejs \
                                    typescript-language-server typescript; do
        echo -n "  Checking for $pkg... "
        if npm -g ls $pkg 2> /dev/null 1> /dev/null; then
            echo "found."
        else
            echo "not found. Installing $pkg."
            npm i -g $pkg
        fi
    done
else
    echo "not found."
fi

echo -n "Checking for Homebrew... "
if which brew 2> /dev/null 1> /dev/null; then
    echo "found."
    for pkg in fd rg clojure clojure-lsp openjdk jdtls marksman texlab \
                  yaml-language-server go gopls llvm; do
        echo -n "  Checking for $pkg... "
        if brew list $pkg 2> /dev/null 1> /dev/null; then
            echo "found."
        else
            echo "not found. Installing $pkg."
            brew install $pkg
        fi
    done
else
    echo "not found."
fi

echo -n "Checking for Python3's pip3... "
if which pip3 2> /dev/null 1> /dev/null; then
    echo "found."
    echo -n "  Checking for python-lsp-server... "
    if pip3 show python-lsp-server 2> /dev/null 1> /dev/null; then
        echo "found."
    else
        echo "not found. Installing python-lsp-server."
        pip3 install "python-lsp-server[all]"
    fi
    for pkg in pylsp-mypy pylsp-rope python-lsp-ruff python-lsp-black; do
        echo -n "  Checking for $pkg... "
        if pip3 show $pkg 2> /dev/null 1> /dev/null; then
            echo "found."
        else
            echo "not found. Installing $pkg."
            pip3 install $pkg
        fi
    done
else
    echo "not found."
fi

echo -n "Checking for Perl::LanguageServer... "
if perl -MPerl::LanguageServer -e 0 2> /dev/null 1> /dev/null; then
    echo "found."
else
    echo "not found. Checking for cpan..."
    if which cpan 2> /dev/null 1> /dev/null; then
        echo "found. Installing Perl::LanguageServer..."
        cpan Perl::LanguageServer
    else
        echo "not found."
    fi
fi

echo -n "Checking for rust-analyzer... "
if which rust-analyzer 2> /dev/null 1> /dev/null; then
    echo "found."
else
    echo -n "not found. Checking for rustup... "
    if which rustup 2> /dev/null 1> /dev/null; then
        echo "found. Installing."
        rustup component add rust-analyzer
    else
        echo "not found."
    fi
fi
