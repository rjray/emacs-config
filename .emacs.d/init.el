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
(add-to-list 'default-frame-alist
             '(font . "Cascadia Code-10"))

;; Packages

(use-package emacs
  ;; Set emacs customizations that aren't related to packages below.
  :bind (("C-+" . text-scale-increase)
	       ("C--" . text-scale-decrease)
	       ("C-=" . (lambda () (interactive) (text-scale-set 0))))
  :custom
  (default-case-fold-search nil)
  (x-select-enable-clipboard 1)
  (tramp-default-method "ssh")
  (fill-column 79)
  (transient-mark-mode t)
  (byte-compile-warnings '(not obsolete))
  (warning-suppress-log-types '((comp) (bytecomp)))
  (warning-minimum-level :error)

  ;; Startup stuff supression
  (inhibit-splash-screen t)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t)

  ;; Backup stuff
  (backup-inhibited t)
  (make-backup-files nil)
  (auto-save-default nil)
  (auto-save-list-file-name nil)
  (delete-auto-save-files t)

  ;; Inhibit "magic" mode selection
  (magic-mode-alist nil)

  (uniquify-buffer-name-style 'forward)

  ;; No double-spaces when I fill a paragraph
  (sentence-end-double-space nil)

  ;; Always number lines and columns in the status line
  (line-number-mode t)
  (column-number-mode t)

  ;; Show indication of the buffer size (right before the line/col)
  (size-indication-mode t)

  ;; Tabs and lines
  (tab-width 2)
  (indent-tabs-mode nil)

  ;; Fill column indicators
  (display-fill-column-indicator-character 124)
  (display-fill-column-indicator-column 80)

  ;; Dialogs
  (use-dialog-box nil)
  (use-file-dialog nil)
  :config
  (display-fill-column-indicator-mode)
  (subword-mode)
  (pixel-scroll-precision-mode)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (add-to-list 'completion-ignored-extensions ".#"))

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
   cider-prompt-for-symbol t
   cider-repl-display-in-current-window t
   cider-repl-history-size 1000))

(use-package clojure-mode
  ;; Clojure
  :ensure t
  :mode "\\.clj")

(use-package counsel
  ;; Provide versions of common commands customized to use Ivy
  :ensure t
  :config
  ;; Use this instead of hitting M-x all the time:
  (global-set-key "\C-x\C-m" 'counsel-M-x)
  (global-set-key "\C-c\C-m" 'counsel-M-x)
  ;; Rest taken from https://oremacs.com/swiper/#global-key-bindings
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  (global-set-key (kbd "C-c c") 'counsel-compile)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c L") 'counsel-git-log)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c m") 'counsel-linux-app)
  (global-set-key (kbd "C-c n") 'counsel-fzf)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c J") 'counsel-file-jump)
  (global-set-key (kbd "C-c w") 'counsel-wmctrl)
  (global-set-key (kbd "C-c b") 'counsel-bookmark)
  (global-set-key (kbd "C-c d") 'counsel-descbinds)
  (global-set-key (kbd "C-c o") 'counsel-outline)
  (global-set-key (kbd "C-c t") 'counsel-load-theme)
  (global-set-key (kbd "C-c F") 'counsel-org-file))

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

(use-package ivy
  ;; Ivy interactive interface completion
  :ensure t
  :delight
  :commands (ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package magit
  ;; Supercharged git interface
  :ensure t
  :custom
  (magit-diff-highlight-trailing t)
  (magit-diff-paint-whitespace t)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (global-set-key (kbd "C-c m") 'magit-status)
  (define-key magit-status-mode-map (kbd "W")
              (lambda ()
                (interactive)
                (if magit-diff-paint-whitespace
                    (setq magit-diff-paint-whitespace nil)
                  (setq magit-diff-paint-whitespace t)))))

(use-package markdown-mode
  ;; Markdown editing
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "multimarkdown"))

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
        recentf-exclude '("\\.ido\\.last" "/recentf$" ".emacs.d/elpa/"))
  :config
  ;; Find files based on the recent-files list:
  (global-set-key (kbd "C-x C-r") 'recentf-open-files-compl))

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

(use-package swiper
  ;; Isearch alternative that uses Ivy
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch))

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

;; Some code for custom key commands:
;; Taken from crisp.el, written by Gary D. Foster
(defvar last-last-command nil
  "Internal variable.")

(defun home ()
  "Home - begin of line, once more - screen, once more - buffer."
  (interactive nil)
  (cond
   ((and (eq last-command 'home) (eq last-last-command 'home))
    (goto-char (point-min)))
   ((eq last-command 'home)
    (move-to-window-line 0))
   (t (beginning-of-line)))
  (setq last-last-command last-command))

(defun end ()
  "End - end of line, once more - screen, once more - buffer."
  (interactive nil)
  (cond
   ((and (eq last-command 'end) (eq last-last-command 'end))
    (goto-char (point-max)))
   ((eq last-command 'end)
    (move-to-window-line -1)
    (end-of-line))
   (t (end-of-line)))
  (setq last-last-command last-command))

;; I forget where I got this... this is a modified version, anyway.
(defun count-region ()
  "Count lines, words and characters in region."
  (interactive)
  (let* ((start (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (l (count-lines start end))
         (w (count-words start end))
         (c (- end start)))
    (message "%s has %d line%s, %d word%s and %d character%s."
             (if (region-active-p) "Region" "Buffer")
             l (if (= 1 l) "" "s")
             w (if (= 1 w) "" "s")
             c (if (= 1 c) "" "s"))))

;; From https://www.emacswiki.org/emacs/RecentFiles
(defun recentf-open-files-compl ()
  "Open a file from the recent-files list, with completion."
  (interactive)
  (let* ((tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
                        recentf-list))
         (fname (completing-read "File name: " tocpl nil nil)))
    (when fname
      (find-file (cdr (assoc-string fname tocpl))))))

(defun fill-paragraph-or-region ()
  "If the region is active, call `fill-region'. Otherwise, `fill-paragraph'."
  (interactive)
  (cond ((region-active-p) (fill-region (region-beginning) (region-end)))
        (t (fill-paragraph nil))))

;; Bind some keys:

;; Browse the kill-ring with C-c k:
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Bind count-region to C-c =:
(global-set-key (kbd "C-c =") 'count-region)

;; This one is neat-- make C-w kill a region when the region is active, or
;; otherwise do a backward-kill-word like C-w behaves in things like bash.
(global-set-key "\C-w"
                (lambda (arg)
                  (interactive "p")
                  (cond ((region-active-p)
                         (kill-region (region-beginning) (region-end)))
                        (t (backward-kill-word arg)))))


;; Function-key bindings. Don't go above f8, though, because MacOS grabs f9
;; through f12. And f1-f4 are already in use.
(global-set-key [(f5)]         'call-last-kbd-macro)
(global-set-key [(control f5)] 'edit-last-kbd-macro)

(global-set-key [(f6)]         'search-forward-regexp)
(global-set-key [(control f6)] 'search-backward-regexp)

(global-set-key [(f7)]         'fill-paragraph-or-region)

(global-set-key [(f8)]         'cider-jack-in)

;; Meta-key combinations
(global-set-key [(meta q)] 'quote)

;; I miss these keys on my Macbook... but at least I have them on full
;; keyboards...
(global-set-key [(home)] 'home)
(global-set-key [(end)] 'end)

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

(when *is-mac*
  (progn
    (setq mac-option-modifier '(:function alt :mouse alt)
          mac-right-command-modifier 'super
          mac-right-option-modifier 'hyper
          ns-alternate-modifier 'super
          ns-command-modifier 'meta
          ns-pop-up-frames nil)
    ;; Do something with command+arrow keys
    (global-set-key [(super up)] 'home)
    (global-set-key [(super down)] 'end)
    (global-set-key [(super left)] 'previous-buffer)
    (global-set-key [(super right)] 'next-buffer)))

;; If there is a directory under ~/.emacs.d named for this host, load all *.el
;; files within it:
(let ((hostdir (concat user-emacs-directory *system-name*)))
  (when (file-directory-p hostdir)
    (dolist (host-el-file (directory-files hostdir t "\\.el$"))
      (load-file host-el-file))))

(provide 'init)
;;; init.el ends here
