;;; .emacs

;; These constants are used to manage all the various sub-dirs that need to
;; be in the load-path:
(defconst homedir (if (or (eq system-type 'cygwin)
                          (eq system-type 'gnu/linux)
                          (eq system-type 'linux)
                          (eq system-type 'darwin))
                      (getenv "HOME")
                    (getenv "USERPROFILE"))
  "My home dir, regardless of host.")
(defconst emacsdir (concat homedir "/.emacs.d") "Location of emacs lisp code")
(defconst emacsmodules (concat emacsdir "/submodules") "External submodules")

;; Additions to the load-path:
(add-to-list 'load-path (concat emacsdir "/my-code"))
(add-to-list 'load-path (concat emacsdir "/other-peoples-code"))
(dolist (submodule (directory-files emacsmodules t "\\w+"))
  (when (file-directory-p submodule)
    (add-to-list 'load-path submodule)))
; Missed this one in the above:
(add-to-list 'load-path (concat emacsmodules "/slime/contrib"))

;; Libs I want visible at all levels:
(require 'imenu)
(require 'iswitch-buffer)
(require 'linum)
(require 'compile) ; Needed for perlcritic.el
(require 'perlcritic)
(require 'browse-kill-ring)
(require 'gist)
(require 'highlight-parentheses)
(require 'paredit)
(require 'magit)
(require 'recentf)
(require 'slime)

;; Load my personal code
(load "key-bindings")
(load "key-functions")
(load "autoloads")
(load "mode-list")
(load "hooks")
(load "utils")
(load "misc")

;; Number ALL the lines!
(when window-system
  (progn
    (global-linum-mode)
    (server-start 1)))

;; Turn on font-lock if available
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; Turn OFF CUA mode. Dunno who's brilliant idea enabling that by default was.
(when (fboundp 'cua-mode)
  (cua-mode -1))

;; default to better frame titles
(setq frame-title-format
      (concat "%b - emacs@" system-name))

;; Alias some stuff to get preferred behavior
; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
; Don't care for typing out "yes" and "no" all the time...
(defalias 'yes-or-no-p 'y-or-n-p)

;;enable narrowing
(put 'narrow-to-region 'disabled nil)

;;; Added/updated by emacs

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(fill-column 79)
 '(menu-bar-mode t)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "dejavu-dejavu sans mono"))))
 '(develock-whitespace-3 ((t nil)))
 '(trailing-whitespace ((t (:underline t))))
 '(whitespace-highlight ((((class color) (background light)) (:background "green1" :underline t)))))
