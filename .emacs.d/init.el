;;; init.el --- Master Emacs configuration file.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; The "base" part of system-name, without the domain.
(defconst *system-name*
  (if (string-match "^\\([a-zA-Z0-9_-]+\\)\\." (system-name))
      (match-string 1 (system-name))
    (system-name))
  "Host name without the domain.")

;; True if this system is MacOS. Used in a few places for paths, etc.
(defconst *is-mac* (eq system-type 'darwin) "Is this a Mac system?")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; This directory holds files that are taken whole from other sources
(add-to-list 'load-path (concat user-emacs-directory "external"))

;; Packages
(use-package aggressive-indent
  ;; Aggressive indenting for some modes
  :ensure t
  :delight
  :hook ((clojure-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)))

(use-package cider
  ;; CIDER for Clojure editing
  :ensure t
  :hook (clojure-mode . cider-mode)
  :config
  (setq
   nrepl-hide-special-buffers t
   cider-auto-select-error-buffer t
   nrepl-buffer-name-show-port t
   cider-repl-display-in-current-window t
   cider-repl-history-size 1000))

(use-package clojure-mode
  ;; Clojure
  :ensure t
  :mode "\\.clj")

(use-package cperl-mode
  ;; Preferred Perl mode
  :ensure t
  :defer t
  :custom
  (setq cperl-indent-parens-as-block t)
  (setq cperl-close-paren-offset -4)
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-continued-brace-offset 0)
  (setq cperl-brace-offset -4)
  (setq cperl-brace-imaginary-offset 0)
  (setq cperl-label-offset -2)
  :config
  (c-set-offset 'inline-open 0))

(use-package delight
  ;; Minor-mode visibility control
  :ensure t)

(use-package desktop
  ;; General desktop stuff (history, etc.)
  :ensure t
  :custom
  (desktop-save t)
  (desktop-restore-eager 5)
  (desktop-globals-to-save (append '((extended-command-history . 30)
                                     (file-name-history        . 100)
                                     (grep-history             . 30)
                                     (compile-history          . 30)
                                     (minibuffer-history       . 50)
                                     (query-replace-history    . 60)
                                     (read-expression-history  . 60)
                                     (regexp-history           . 60)
                                     (regexp-search-ring       . 20)
                                     (search-ring              . 20)
                                     (shell-command-history    . 50)
                                     tags-file-name
                                     register-alist)))
  :config
  (desktop-save-mode 1))

(use-package dired
  ;; Dired mode
  :defer 1
  :config
  (defadvice dired-create-directory (after revert-buffer-after-create activate)
    "Revert the buffer after a new directory is created."
    (revert-buffer))
  (defadvice wdired-abort-changes (after revert-buffer-after-abort activate)
    "Revert the buffer after aborting wdired change."
    (revert-buffer)))

(use-package display-line-numbers
  ;; Number ALL the lines
  :if window-system
  :ensure t
  :config
  ;; No, seriously... all the lines.
  (global-display-line-numbers-mode t)
  (setq display-line-numbers-grow-only t))

(use-package dumb-jump
  ;; A go-to-def package that uses ripgrep
  :ensure t
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

(use-package ef-themes
  ;; Theming
  :if window-system
  :ensure t
  :config
  ;; Clear out anything from custom
  (mapc #'disable-theme custom-enabled-themes)
  ;; Enable the theme
  (load-theme 'ef-elea-dark t))

(use-package exec-path-from-shell
  ;; Set up the exec-path by reading $PATH from a shell
  :ensure t
  :commands (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  ;; Flycheck
  :ensure t
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-clojure
  ;; Clojure support for Flycheck
  :ensure t
  :mode "\\.clj")

(use-package git-gutter
  ;; Git-related decorations in the gutter
  :if window-system
  :ensure t
  :commands (global-git-gutter-mode)
  :delight
  :config
  (global-git-gutter-mode +1))

(use-package highlight-parentheses
  ;; Parens highlighting
  :ensure t
  :delight
  :hook ((clojure-mode . highlight-parentheses-mode)
         (emacs-lisp-mode . highlight-parentheses-mode)
         (lisp-mode . highlight-parentheses-mode)))

(use-package ido
  ;; IDO mode
  :ensure t
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  :config
  (ido-mode 1))

(use-package magit
  ;; Supercharged git interface
  :ensure t
  :custom
  (magit-diff-highlight-trailing t)
  (magit-diff-paint-whitespace t)
  :config
  (global-set-key (kbd "C-c m") 'magit-status)
  (define-key magit-status-mode-map (kbd "W")
              (lambda ()
                (interactive)
                (if magit-diff-paint-whitespace
                    (setq magit-diff-paint-whitespace nil)
                  (setq magit-diff-paint-whitespace t)))))

(use-package org
  ;; Org Mode
  :ensure t
  :hook ((org-mode . auto-revert-mode)
         (org-mode . (lambda ()
                       (progn
                         (local-set-key (kbd "C-c C-j") 'org-journal-new-entry)
                         (local-set-key (kbd "C-c j") 'org-goto)))))
  :custom
  (org-default-notes-file "~/Dropbox/org/organizer.org")
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  :config
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c o")
                  (lambda () (interactive)
                    (find-file "~/Dropbox/org/organizer.org")))
  (global-set-key (kbd "C-c C-o")
                  (lambda () (interactive)
                    (find-file "~/Dropbox/org"))))

(use-package org-journal
  ;; Org journaling mode
  :ensure t
  :bind (("C-c C-j" . org-journal-new-entry))
  :hook ((org-journal-mode . auto-fill-mode))
  :custom
  (org-journal-dir "~/Dropbox/org/journal"))

(use-package paredit
  ;; Parens-editing supercharging
  :ensure t
  :delight
  :hook ((clojure-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)))

(use-package recentf
  ;; Recent-file tracking and opening
  :ensure t
  :init
  (add-hook 'after-init-hook 'recentf-mode)
  (setq recentf-auto-cleanup 'never
        recentf-max-menu-items 40
        recentf-max-saved-items 100
        recentf-exclude '("\\.ido\\.last" "/recentf$" ".emacs.d/elpa/")))

(use-package server
  ;; Emacs in server mode
  :commands (server-running-p)
  :init
  (add-hook 'server-done-hook 'delete-frame)
  (add-hook 'server-done-hook
            (lambda ()
              (kill-buffer nil)))
  :config
  (unless (server-running-p)
    (server-start)))

(use-package wdired
  ;; Writable-dired package
  :defer 1
  :ensure t
  :config
  (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer)
              'dired-back-to-top)
  (define-key wdired-mode-map (vector 'remap 'end-of-buffer)
              'dired-jump-to-bottom))

(use-package whitespace
  ;; Excess whitespace display
  :ensure t
  :delight global-whitespace-mode
  :init
  (add-hook 'after-init-hook 'global-whitespace-mode)
  :config
  (setq whitespace-style '(face tabs lines-tail)))

;; Set some defaults
(setq-default
 default-case-fold-search nil
 x-select-enable-clipboard 1
 tramp-default-method "ssh"
 fill-column 79
 transient-mark-mode t

 ;; Startup stuff supression
 inhibit-splash-screen t
 inhibit-startup-echo-area-message t
 inhibit-startup-screen t

 ;; Backup stuff
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 auto-save-list-file-name nil
 delete-auto-save-files t

 ;; Inhibit "magic" mode selection
 magic-mode-alist nil

 uniquify-buffer-name-style 'forward

 ;; No double-spaces when I fill a paragraph
 sentence-end-double-space nil

 ;; Always number lines and columns in the status line
 line-number-mode t
 column-number-mode t

 ;; Show indication of the buffer size (right before the line/col)
 size-indication-mode t

 ;; Tabs and lines
 tab-width 2
 indent-tabs-mode nil

 ;; Dialogs
 use-dialog-box nil
 use-file-dialog nil)

;; Some window-system-necessary settings
(when (window-system)
  (progn
    (blink-cursor-mode -1)
    (menu-bar-mode 1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
;; Don't care for typing out "yes" and "no" all the time...
(defalias 'yes-or-no-p 'y-or-n-p)

;;enable narrowing
(put 'narrow-to-region 'disabled nil)

;; Visual Bell (flash the mode-line instead of an audio bell)
;; Cribbed from Jason Filsinger, https://github.com/filsinger/emacs-config
(setq visible-bell nil
      ring-bell-function `(lambda ()
                            (let ((mode-line-bell-orig-bg
                                   (face-background 'mode-line))
                                  (mode-line-bell-orig-fg
                                   (face-foreground 'mode-line)))
                              (set-face-background 'mode-line "#ED3B3B")
                              (set-face-foreground 'mode-line "#7F2020")
                              (sit-for 0.1)
                              (set-face-background 'mode-line
                                                   mode-line-bell-orig-bg)
                              (set-face-foreground 'mode-line
                                                   mode-line-bell-orig-fg))))

;; default to better frame titles
(setq frame-title-format
      (concat "%b - emacs@" *system-name*))

(when *is-mac* (require 'mac))

;; If there is a directory under ~/.emacs.d named for this host, load all *.el
;; files within it:
(let ((hostdir (concat user-emacs-directory *system-name*)))
  (when (file-directory-p hostdir)
    (dolist (host-el-file (directory-files hostdir t "\\.el$"))
      (load-file host-el-file))))

(provide 'init)
;;; init.el ends here
