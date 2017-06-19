;;; .emacs

;; The "base" part of system-name, without the domain.
(defconst *system-name*
  (if (string-match "^\\([a-zA-Z0-9_-]+\\)\\." system-name)
      (match-string 1 system-name)
    system-name)
  "Host name without the domain")

;; True if this system is MacOS. Used in a few places for paths, etc.
(defconst *is-mac* (eq system-type 'darwin))

;; Turn the two emacs-*-version values into a single comparable int
(defconst *emacs-version* (+ (* emacs-major-version 1000) emacs-minor-version))

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
(defconst *emacsmodules* (concat *emacsdir* "submodules") "Git submodules")

;; Additions to the load-path:
(add-to-list 'load-path (concat *emacsdir* "my-code"))
(add-to-list 'load-path (concat *emacsdir* "other-peoples-code"))
(dolist (submodule (directory-files *emacsmodules* t "\\w+"))
  (when (file-directory-p submodule)
    (add-to-list 'load-path submodule)))

;; Set the location for customization settings saves, and load it (before
;; any per-host or Mac-specific settings).
(setq custom-file (expand-file-name "custom.el" *emacsdir*))
(load custom-file)

;; Libs which have their own set-up code, but are loaded as-needed:
(eval-after-load 'dired '(require 'setup-dired))

;; Libs I want visible at all levels:
(require 'imenu)
(require 'linum)
(require 'compile) ; Needed for perlcritic.el
(require 'perlcritic)
(require 'browse-kill-ring)
(require 'rainbow-delimiters)
(require 'paredit)
(require 'recentf)
(require 'expand-region)
(require 'multiple-cursors)
(require 'whitespace)
(require 'diminish)
(require 'wrap-region)
(require 'uniquify)
(require 'saveplace)

;; These have their own set-up code, but should also be pre-loaded:
(require 'setup-magit)
;; CIDER requires some stuff only in 24.3 and later:
(when (>= *emacs-version* 24003)
  (require 'setup-cider))

;; Load my personal code
(load "key-bindings")
(load "key-functions")
(load "hooks")
(load "utils")
(load "misc")

;; Diminish some of the minor-mode clutter:
(diminish 'global-whitespace-mode)
(diminish 'wrap-region-mode)

;; Things to do when running in a windowing system (X, MacOS, etc.)
(when window-system
  (progn
    ;; Number ALL the lines!
    (global-linum-mode)
    ;; Start a server if one isn't already running
    (unless (server-running-p)
      (server-start))
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

;; If this is a Mac, load some Mac-specific code
(when *is-mac* (require 'mac))

;; If there is a directory under ~/.emacs.d named for this host, load all *.el
;; files within it:
(let ((hostdir (concat *emacsdir* *system-name*)))
  (when (file-directory-p hostdir)
    (dolist (host-el-file (directory-files hostdir t "\\.el$"))
      (load-file host-el-file))))
