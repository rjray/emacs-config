;;; .emacs

(require 'cl)

;; The "base" part of system-name, without the domain.
(defconst *system-name*
  (if (string-match "^\\([a-zA-Z0-9_-]+\\)\\." system-name)
      (match-string 1 system-name)
    system-name)
  "Host name without the domain")

;; True if this system is MacOS. Used in a few places for paths, etc.
(defconst *is-mac* (eq system-type 'darwin) "Is this a Mac system?")

;; Turn the two emacs-*-version values into a single comparable int
(defconst *emacs-version*
  (+ (* emacs-major-version 1000) emacs-minor-version)
  "Emacs version as a comparable integer")

;; These constants are used to manage all the various sub-dirs that need to
;; be in the load-path:
(defconst *homedir* (if (or
                         (eq system-type 'cygwin)
                         (eq system-type 'gnu/linux)
                         (eq system-type 'linux)
                         (eq system-type 'darwin))
                        (getenv "HOME")
                      (getenv "USERPROFILE"))
  "My home dir, regardless of host.")
(defconst *emacsdir* (concat *homedir* "/.emacs.d/") "Root of emacs lisp code")

;; Additions to the load-path:
(add-to-list 'load-path (concat *emacsdir* "my-code"))

(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar rjray/packages '(ac-c-headers
                         ac-cider
                         ac-emmet
                         aggressive-indent
                         auto-complete
                         cider
                         clojure-mode
                         cuda-mode
                         diminish
                         dired-details
                         emmet-mode
                         exec-path-from-shell
                         expand-region
                         flycheck
                         flycheck-clojure
                         fringe-helper
                         gist
                         git
                         git-gutter
                         git-gutter-fringe
                         highlight-parentheses
                         highlight-symbol
                         js2-mode
                         json-mode
                         json-navigator
                         json-reformat
                         less-css-mode
                         magit
                         markdown-mode
                         multiple-cursors
                         paredit
                         pkg-info
                         popup
                         rainbow-delimiters
                         rainbow-mode
                         sass-mode
                         scss-mode
                         seoul256-theme
                         solarized-theme
                         web-mode
                         wrap-region
                         yaml-mode)
  "Default packages")

(defun rjray/packages-installed-p ()
  (loop for pkg in rjray/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (rjray/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg rjray/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Libs I want visible at all levels:
(require 'linum)
(require 'paredit)
(require 'recentf)
(require 'expand-region)
(require 'multiple-cursors)
(require 'whitespace)
(require 'diminish)
(require 'wrap-region)
(require 'uniquify)
(require 'saveplace)
(require 'highlight-symbol)
;; These have their own set-up code, but should also be pre-loaded:
(require 'setup-cider)
(require 'setup-dired)
(require 'setup-magit)

;; Load my personal code
(load "key-bindings")
(load "key-functions")
(load "hooks")
(load "utils")
(load "misc")

;; Diminish some of the minor-mode clutter:
(diminish 'global-whitespace-mode)
(diminish 'wrap-region-mode)
(diminish 'paredit-mode)

;; Things to do when running in a windowing system (X, MacOS, etc.)
(when (display-graphic-p)
  (progn
    (require 'git-gutter-fringe)
    (global-git-gutter-mode +1)
    (diminish 'git-gutter-mode)
    (setq-default indicate-buffer-boundaries 'left)
    ;; Number ALL the lines!
    (global-linum-mode)
    ;; Start a server if one isn't already running
    (unless (server-running-p)
      (server-start))
    (setq solarized-use-variable-pitch nil)
    (load-theme 'solarized-light t)
    (if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
    (if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
    (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))))

;; Turn on show-paren if available
(when (fboundp 'show-paren-mode)
  (show-paren-mode t))

;; Turn on font-lock if available
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; Turn OFF CUA mode. Dunno who's brilliant idea enabling that by default was.
(when (fboundp 'cua-mode)
  (cua-mode -1))

;; default to better frame titles
(setq frame-title-format
      (concat "%b - emacs@" *system-name*))

;; Alias some stuff to get preferred behavior
;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
;; Don't care for typing out "yes" and "no" all the time...
(defalias 'yes-or-no-p 'y-or-n-p)

;;enable narrowing
(put 'narrow-to-region 'disabled nil)

;; Set the location for customization settings saves, and load it (before
;; any per-host or Mac-specific settings).
(setq custom-file (expand-file-name "custom.el" *emacsdir*))
(load custom-file)

;; If this is a Mac, load some Mac-specific code
(when *is-mac* (require 'mac))

;; If there is a directory under ~/.emacs.d named for this host, load all *.el
;; files within it:
(let ((hostdir (concat *emacsdir* *system-name*)))
  (when (file-directory-p hostdir)
    (dolist (host-el-file (directory-files hostdir t "\\.el$"))
      (load-file host-el-file))))
