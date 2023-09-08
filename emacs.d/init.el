;;; init.el --- Master Emacs configuration file.  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Latest iteration of my Emacs configuration. This is a complete rewrite done
;; in light of the 29.1 release. All the code that was kept in separate files
;; has been rolled in to this one, with the exception of host-specific code
;; kept in the sub-directories named for each host.

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
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)

;; This directory holds files that are taken whole from other sources
(add-to-list 'load-path (concat user-emacs-directory "external"))

;; Packages

;; Set emacs customizations that aren't related to packages below. This should
;; be mostly global keybindings and top-level setq's. This appears before all
;; other `use-package' invocations.
(use-package emacs
  :init
  ;; Some code for custom key commands:
  ;; Taken from crisp.el, written by Gary D. Foster
  (defvar last-last-command nil
    "Internal variable.")

  (defun my/home ()
    "Home - begin of line, once more - screen, once more - buffer."
    (interactive nil)
    (cond
     ((and (eq last-command 'my/home) (eq last-last-command 'my/home))
      (goto-char (point-min)))
     ((eq last-command 'my/home)
      (move-to-window-line 0))
     (t (beginning-of-line)))
    (setq last-last-command last-command))

  (defun my/end ()
    "End - end of line, once more - screen, once more - buffer."
    (interactive nil)
    (cond
     ((and (eq last-command 'my/end) (eq last-last-command 'my/end))
      (goto-char (point-max)))
     ((eq last-command 'my/end)
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

  (defun fill-paragraph-or-region ()
    "If the region is active, call `fill-region'. Otherwise, `fill-paragraph'."
    (interactive)
    (cond ((region-active-p) (fill-region (region-beginning) (region-end)))
          (t (fill-paragraph nil))))

  (defun untabify-buffer-or-region ()
    "Untabify the entire buffer. If region is active, only untabify the region."
    (interactive)
    (cond ((region-active-p) (untabify (region-beginning) (region-end)))
          (t (untabify (point-min) (point-max)))))

  (defun my/text-scale-reset ()
    (interactive)
    (text-scale-set 0))

  ;; This one is neat-- make C-w kill a region when the region is active, or
  ;; otherwise do a backward-kill-word like C-w behaves in things like bash.
  (defun my/kill-region-or-word (arg)
    (interactive "p")
    (cond ((region-active-p)
           (kill-region (region-beginning) (region-end)))
          (t (backward-kill-word arg))))

  :bind (:map global-map
              ("C-h h" . nil)
              ("C-w" . my/kill-region-or-word)
              ("C-z" . undo)
              ;; Function-key bindings. Don't go above f8, though, because MacOS
              ;; grabs f9 through f12. And f1-f4 are already in use.
              ("<f5>" . call-last-kbd-macro)
              ("C-<f5>" . edit-last-kbd-macro)
              ("<f6>" . search-forward-regexp)
              ("C-<f6>" . search-backward-regexp)
              ("<f7>" . fill-paragraph-or-region)
              ("C-<f7>" . untabify-buffer-or-region)
              ("<f8>" . cider-jack-in)
              ("C-!" . count-region)
              ("C-+" . text-scale-increase)
              ("C--" . text-scale-decrease)
              ("C-_" . my/text-scale-reset)
              ("<home>" . my/home)
              ("<end>" . my/end))

  :hook ((text-mode . display-fill-column-indicator-mode)
         (text-mode . hl-line-mode)
         (prog-mode . display-fill-column-indicator-mode)
         (prog-mode . hl-line-mode)
         (prog-mode . flyspell-prog-mode))

  :custom
  (locale-coding-system 'utf-8)
  (default-case-fold-search nil)
  (x-select-enable-clipboard 1)
  (tramp-default-method "ssh")
  (fill-column 79)
  (transient-mark-mode t)
  (byte-compile-warnings '(not obsolete))
  (warning-suppress-log-types '((comp) (bytecomp)))
  (warning-minimum-level :error)

  ;; default to better frame titles
  (frame-title-format (concat "%b - emacs@" *system-name*))

  ;; Startup stuff suppression
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

  ;; Using ripgrep in place of grep
  (grep-command "rg -nS --no-heading ")
  (grep-use-null-device nil)

  :config
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
  ;; Some window-system-necessary settings
  (if (window-system)
      (progn
        (blink-cursor-mode -1)
        (menu-bar-mode 1)
        (tool-bar-mode -1)
        (scroll-bar-mode -1))
    (menu-bar-mode -1))
  ;; cperl-mode is preferred to perl-mode
  (defalias 'perl-mode 'cperl-mode)
  ;; Don't care for typing out "yes" and "no" all the time...
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;;enable narrowing
  (put 'narrow-to-region 'disabled nil)
  (subword-mode)
  (prefer-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (add-to-list 'completion-ignored-extensions ".#"))

;;;===========================================================================
;;; Start-up and general interface packages.
;;;===========================================================================

(use-package delight
  ;; Minor-mode visibility control. Load this early so that later use-package
  ;; invocations can use it.
  :ensure t)

(use-package desktop
  ;; General desktop stuff (history, etc.)
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

(use-package display-line-numbers
  ;; Number ALL the lines
  :if window-system
  :config
  ;; No, seriously... all the lines.
  (global-display-line-numbers-mode t)
  (setq display-line-numbers-grow-only t))

(use-package ef-themes
  ;; Theming: https://protesilaos.com/emacs/ef-themes
  :ensure t
  :custom
  (ef-themes-to-toggle '(ef-elea-dark ef-elea-light))
  :config
  ;; For some reason, doing this in :bind broke things:
  (keymap-global-set "M-t" 'ef-themes-toggle)
  ;; Clear out anything from custom
  (mapc #'disable-theme custom-enabled-themes)
  ;; Enable the theme
  (load-theme 'ef-elea-dark t))

(use-package exec-path-from-shell
  ;; Set up the exec-path by reading $PATH from a shell
  :hook (after-init-hook . exec-path-from-shell-initialize))

(use-package recentf
  ;; Recent-file tracking and opening
  :bind
  (("C-x C-r" .
    ;; From https://www.emacswiki.org/emacs/RecentFiles
    (lambda ()
      "Open a file from the recent-files list, with completion."
      (interactive)
      (let* ((tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
                            recentf-list))
             (fname (completing-read "File name: " tocpl nil nil)))
        (when fname
          (find-file (cdr (assoc-string fname tocpl))))))))
  :init
  (add-hook 'after-init-hook 'recentf-mode)
  (setq recentf-auto-cleanup 'never
        recentf-max-menu-items 40
        recentf-max-saved-items 100
        recentf-exclude '("\\.ido\\.last" "/recentf$" ".emacs.d/elpa/")))

(use-package server
  ;; Emacs in server mode
  :commands (server-running-p)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package whitespace
  ;; Excess whitespace display
  :delight global-whitespace-mode
  :init
  (add-hook 'after-init-hook 'global-whitespace-mode)
  :config
  (setq whitespace-style '(face tabs lines-tail)))

;;;===========================================================================
;;; Packages related to command-selection, completion, etc.
;;;===========================================================================

(use-package counsel
  ;; Provide versions of common commands customized to use Ivy
  :ensure t
  :bind
  (;; Use this instead of hitting M-x all the time:
   ("C-x C-m" . counsel-M-x)
   ("C-c C-m" . counsel-M-x)
   ;; Rest taken from https://oremacs.com/swiper/#global-key-bindings
   ("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("<f2> j" . counsel-set-variable)
   ("C-c b" . counsel-bookmark)
   ("C-c c" . counsel-compile)
   ("C-c d" . counsel-descbinds)
   ("C-c F" . counsel-org-file)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c J" . counsel-file-jump)
   ("C-c k" . counsel-rg)
   ("C-x l" . counsel-locate)
   ("C-c L" . counsel-git-log)
   ("C-c m" . counsel-linux-app)
   ("C-c n" . counsel-fzf)
   ("C-c t" . counsel-load-theme)))

(use-package ivy
  ;; Ivy interactive interface completion
  :ensure t
  :delight
  :commands (ivy-mode)
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-c v" . ivy-push-view)
   ("C-c V" . ivy-pop-view)
   ("C-c C-r" . ivy-resume))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

(use-package swiper
  ;; Isearch alternative that uses Ivy
  :ensure t
  :bind (("C-s" . swiper-isearch)))

(use-package projectile
  ;; Projectile for project-level management
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name)))
  :commands (projectile-mode)
  ;; Enable Projectile globally
  :init (projectile-mode +1)
  :config
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package dumb-jump
  ;; A go-to-def package that uses ripgrep
  :ensure t
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-rg-search-args "--hidden --pcre2")
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

(use-package company
  ;; Company mode for completion
  :ensure t
  :delight
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package which-key
  :ensure t
  :delight which-key-mode
  :commands (which-key-mode which-key-setup-minibuffer)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-prefix-prefix "◉ ")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-max-display-columns nil)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package prescient
  :ensure t
  :commands (prescient-persist-mode)
  :config
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000) ;; More prescient history
  (prescient-persist-mode +1))

;; Use `prescient' for Ivy menus.
(use-package ivy-prescient
  :ensure t
  :after ivy
  :commands (ivy-prescient-mode)
  :config
  ;; don't prescient sort these commands
  (dolist (command '(org-ql-view counsel-find-file))
    (setq ivy-prescient-sort-commands (append ivy-prescient-sort-commands
                                              (list command))))
  (ivy-prescient-mode +1))

(use-package company-prescient
  :ensure t
  :after company
  :commands (company-prescient-mode)
  :config
  (company-prescient-mode +1))

;;;===========================================================================
;;; Language parsing, tree-sitter, non-language-specific bits
;;;===========================================================================

(use-package evil-nerd-commenter
  ;; Just using this for the ease of commenting lines language-independent.
  :ensure t
  :bind (("C-/" . evilnc-comment-or-uncomment-lines)))

(use-package tree-sitter
  :ensure t
  :defer t
  :delight " tree")

(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package treesit
  :preface
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (dockerfile .
                      ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript .
                      ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript .
                      ("https://github.com/tree-sitter/tree-sitter-typescript"
                       "master" "typescript/src"))
          (tsx .
               ("https://github.com/tree-sitter/tree-sitter-typescript"
                "master" "tsx/src"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (defun my/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping '((c-mode . c-ts-mode)
                     (c++-mode . c++-ts-mode)
                     (css-mode . css-ts-mode)
                     (java-mode . java-ts-mode)
                     (bash-mode . bash-ts-mode)
                     (js2-mode . js-ts-mode)
                     (typescript-mode . typescript-ts-mode)
                     (json-mode . json-ts-mode)
                     (python-mode . python-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (my/treesit-install-all-languages))

;; Wait until this is in MELPA or ELPA before trying to use it. Keep an eye on
;; https://github.com/mickeynp/combobulate for updates.

;; (use-package combobulate
;;   :ensure t
;;   :defer t
;;   :hook ((python-ts-mode-hook . combobulate-mode)
;;          (js-ts-mode-hook . combobulate-mode)
;;          (json-ts-mode . combobulate-mode)
;;          (css-ts-mode-hook . combobulate-mode)
;;          (yaml-ts-mode-hook . combobulate-mode)
;;          (typescript-ts-mode-hook . combobulate-mode)
;;          (tsx-ts-mode-hook . combobulate-mode)))

(use-package yasnippet
  ;; Text snippets/templates expansion
  :ensure t
  :commands (yas-global-mode)
  :bind (("C-c C-y" . yas-insert-snippet))
  :custom
  (yas-wrap-around-region t)
  (yas-snippet-dirs '("~/.emacs.d/snippets/"))
  :config
  (yas-global-mode 1))

;;;===========================================================================
;;; Eglot setup
;;;===========================================================================

(use-package eglot
  :defer t
  :custom
  (read-process-output-max (* 1024 1024))
  (eldoc-echo-area-use-multiline-p)
  (eglot-autoshutdown t)
  :hook ((bash-ts-mode . eglot-ensure)
         (c-ts-mode-hook . eglot-ensure)
         (c++-ts-mode-hook . eglot-ensure)
         (clojure-mode . eglot-ensure)
         (css-ts-mode-hook . eglot-ensure)
         (dockerfile-ts-mode . eglot-ensure)
         (html-mode-hook . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         (js-ts-mode-hook . eglot-ensure)
         (tsx-ts-mode-hook . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (latex-mode-hook . eglot-ensure)
         (markdown-mode . eglot-ensure)
         (cperl-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-ts-mode-hook . eglot-ensure)
         (yaml-ts-mode . eglot-ensure))
  :bind (("C-c l b" . eglot-format-buffer)
         ("C-c l a" . eglot-code-actions)
         ("C-c l e" . eglot-reconnect)
         ("C-c l r" . eglot-rename)
         ("C-c l C-d" . eldoc)
         ("C-c l C-e" . eglot-rename)
         ("C-c l C-o" . python-sort-imports))
  :config
  (setq eglot-workspace-configuration
        '((:pylsp .
                  (:configurationSources
                   ["flake8"]
                   :plugins (:pycodestyle (:enabled :json-false)
                                          :mccabe (:enabled :json-false)
                                          :pyflakes (:enabled :json-false)
                                          :flake8
                                          (:enabled :json-false
                                                    :maxLineLength 80)
                                          :ruff
                                          (:enabled t :lineLength 80)
                                          :pydocstyle
                                          (:enabled t :convention "numpy")
                                          :yapf (:enabled :json-false)
                                          :autopep8 (:enabled :json-false)
                                          :black
                                          (:enabled t
                                                    :line_length 80
                                                    :cache_config t)))))))

;;;===========================================================================
;;; Elisp, Lisp, and Clojure support.
;;;===========================================================================

(use-package aggressive-indent
  ;; Aggressive indenting for some modes
  :ensure t
  :delight
  :hook ((clojure-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)))

(use-package highlight-parentheses
  ;; Parens highlighting
  :ensure t
  :delight
  :hook ((clojure-mode . highlight-parentheses-mode)
         (emacs-lisp-mode . highlight-parentheses-mode)
         (lisp-mode . highlight-parentheses-mode)))

(use-package cider
  ;; CIDER for Clojure editing
  :ensure t
  :defer t
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
  :defer t)

(use-package eldoc
  ;; ElDoc
  :delight eldoc-mode)

;; Couldn't quite get this to work with use-package and :delight.
(require 'delight)
(delight 'emacs-lisp-mode
         '("EL/" (lexical-binding "l" "d"))
         :major)

(use-package paredit
  ;; Parens-editing supercharging
  :ensure t
  :delight
  :hook ((clojure-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)))

;;;===========================================================================
;;; Perl development
;;;===========================================================================

(use-package cperl-mode
  ;; Preferred Perl mode
  :defer t
  :custom
  (cperl-indent-parens-as-block t)
  (cperl-close-paren-offset -4)
  (cperl-indent-level 4)
  (cperl-continued-statement-offset 4)
  (cperl-continued-brace-offset 0)
  (cperl-brace-offset -4)
  (cperl-brace-imaginary-offset 0)
  (cperl-label-offset -2)
  :config
  (c-set-offset 'inline-open 0))

;;;===========================================================================
;;; Markdown
;;;===========================================================================

(use-package markdown-mode
  ;; Markdown editing
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t)
  :defines markdown-mode-map
  ;; These bindings are used for window movement
  :bind (:map markdown-mode-map
              ("C-c <left>" . nil)
              ("C-c <right>" . nil)
              ("C-c <up>" . nil)
              ("C-c <down>" . nil)))

;;;===========================================================================
;;; YAML
;;;===========================================================================

;; Also keep an eye on https://github.com/zkry/yaml-pro
(use-package yaml-ts-mode
  :ensure t
  :defer t
  :mode (("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)))

;;;===========================================================================
;;; Org Mode and related
;;;===========================================================================

(use-package org
  ;; Org Mode
  :defer t
  :bind (("C-c o" . (lambda () (interactive)
                      (find-file "~/Dropbox/org/organizer.org")))
         ("C-c C-o" . (lambda () (interactive)
                        (find-file "~/Dropbox/org"))))
  :hook ((org-mode . auto-revert-mode)
         (org-mode . (lambda ()
                       (progn
                         (local-set-key (kbd "C-c C-j") 'org-journal-new-entry)
                         (local-set-key (kbd "C-c j") 'org-goto)))))
  :custom
  (org-default-notes-file "~/Dropbox/org/organizer.org")
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6)))))

(use-package org-journal
  ;; Org journaling mode
  :ensure t
  :defer t
  :bind (("C-c C-j" . org-journal-new-entry))
  :hook ((org-journal-mode . auto-fill-mode))
  :custom
  (org-journal-dir "~/Dropbox/org/journal"))

(use-package org-bullets
  :ensure t
  :defer t
  :commands (org-bullets-mode)
  :hook ((org-mode . org-bullets-mode)))

;;;===========================================================================
;;; Magit and git-related code
;;;===========================================================================

(use-package diff-hl
  :ensure t
  :defer nil
  :commands (global-diff-hl-mode)
  :config
  (global-diff-hl-mode))

(use-package git-gutter
  ;; Git-related decorations in the gutter
  :if window-system
  :ensure t
  :commands (global-git-gutter-mode)
  :delight
  :config
  (global-git-gutter-mode +1))

(use-package magit
  ;; Supercharged git interface
  :ensure t
  :bind (("C-c m" . magit-status))
  :custom
  (magit-diff-highlight-trailing t)
  (magit-diff-paint-whitespace t)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq magit-completing-read-function 'ivy-completing-read)
  (define-key magit-status-mode-map (kbd "W")
              (lambda ()
                (interactive)
                (if magit-diff-paint-whitespace
                    (setq magit-diff-paint-whitespace nil)
                  (setq magit-diff-paint-whitespace t)))))

;; Technically only git-related in the sense that it's a Github service...
(use-package igist
  :ensure t
  :defer t
  :bind (("M-o" . igist-dispatch)
         (:map igist-list-mode-map
               ("C-j" . igist-list-view-current)
               ("RET" . igist-list-edit-gist-at-point)
               ("+" . igist-list-add-file)
               ("-" . igist-delete-current-filename)
               ("D" . igist-delete-current-gist)
               ("S" . igist-star-gist)
               ("U" . igist-unstar-gist)
               ("a" . igist-add-comment)
               ("c" . igist-load-comments)
               ("d" . igist-list-edit-description)
               ("f" . igist-fork-gist)
               ("g" . igist-list-refresh)
               ("r" . igist-browse-gist)
               ("s" . igist-tabulated-list-sort)
               ("v" . igist-list-view-current)
               ("w" . igist-copy-gist-url)
               ("K" . igist-list-cancel-load)
               ("{" . igist-tabulated-list-narrow-current-column)
               ("}" . igist-tabulated-list-widen-current-column)
               ("<tab>" . igist-toggle-row-children-at-point)
               ("<backtab>" . igist-toggle-all-children)
               ("C" . igist-table-menu))
         (:map igist-edit-mode-map
               ([remap save-buffer] . igist-save-current-gist)
               ("M-o" . igist-dispatch)
               ("C-c C-c" . igist-save-current-gist-and-exit)
               ("C-c C-k" . kill-current-buffer)
               ("C-c '" . igist-save-current-gist-and-exit))
         (:map igist-comments-list-mode-map
               ("+" . igist-add-comment)
               ("-" . igist-delete-comment-at-point)
               ("D" . igist-delete-comment-at-point)
               ("e" . igist-add-or-edit-comment)
               ("g" . igist-load-comments)
               ("q" . kill-current-buffer))
         (:map igist-comments-edit-mode-map
               ("M-o" . igist-dispatch)
               ("C-c C-c" . igist-post-comment)
               ("C-c C-k" . kill-current-buffer)))
  :config
  (let ((default-directory user-emacs-directory))
    (condition-case nil
        (progn (setq igist-current-user-name
                     (car-safe
                      (process-lines "git"
                                     "config"
                                     "github.user")))
               (setq igist-auth-marker
                     (or (ignore-errors
                           (car-safe (process-lines "git" "config"
                                                    "github.igist")))
                         igist-auth-marker)))
      (error (message "Igist-current-user-name cannot be set")))))

;;;===========================================================================
;;; Treemacs for navigation
;;;===========================================================================

(use-package treemacs
  ;; File-explorer tree
  :ensure t
  :defer t
  :bind
  (("C-c t" . treemacs))
  :config
  (setq treemacs-width 30))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;;;===========================================================================
;;; Dired-related bits
;;;===========================================================================

(use-package dired
  ;; Dired mode
  :defer t
  :config
  (defadvice dired-create-directory (after revert-buffer-after-create activate)
    "Revert the buffer after a new directory is created."
    (revert-buffer))
  (defadvice wdired-abort-changes (after revert-buffer-after-abort activate)
    "Revert the buffer after aborting wdired change."
    (revert-buffer)))

(use-package wdired
  ;; Writable-dired package
  :defer t
  :config
  (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer)
              'dired-back-to-top)
  (define-key wdired-mode-map (vector 'remap 'end-of-buffer)
              'dired-jump-to-bottom))

;;;===========================================================================
;;; Flycheck bits
;;;===========================================================================

(use-package flycheck
  ;; Flycheck
  :ensure t
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-clojure
  ;; Clojure support for Flycheck
  :after (clojure-mode)
  :ensure t)

(use-package flycheck-posframe
  :ensure t
  :commands (flycheck-posframe-mode)
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;;;===========================================================================
;;; Shell/terminal
;;;===========================================================================

(use-package vterm
  :ensure t
  :defer  t
  :commands (vterm vterm-send-key)

  :preface
  (let ((last-vterm ""))
    (defun toggle-vterm ()
      (interactive)
      (cond ((string-match-p "^\\vterm<[1-9][0-9]*>$" (buffer-name))
             (goto-non-vterm-buffer))
            ((get-buffer last-vterm) (switch-to-buffer last-vterm))
            (t (vterm (setq last-vterm "vterm<1>")))))

    (defun goto-non-vterm-buffer ()
      (let* ((r "^\\vterm<[1-9][0-9]*>$")
             (vterm-buffer-p (lambda (b) (string-match-p r (buffer-name b))))
             (non-vterms (cl-remove-if vterm-buffer-p (buffer-list))))
        (when non-vterms
          (switch-to-buffer (car non-vterms)))))

    (defun switch-vterm (n)
      (let ((buffer-name (format "vterm<%d>" n)))
        (setq last-vterm buffer-name)
        (cond ((get-buffer buffer-name)
               (switch-to-buffer buffer-name))
              (t (vterm buffer-name)
                 (rename-buffer buffer-name))))))

  :bind (("C-x C-z" . toggle-vterm)
         ("M-1" . (lambda () (interactive) (switch-vterm 1)))
         ("M-2" . (lambda () (interactive) (switch-vterm 2)))
         ("M-3" . (lambda () (interactive) (switch-vterm 3)))
         ("M-4" . (lambda () (interactive) (switch-vterm 4)))
         ("M-5" . (lambda () (interactive) (switch-vterm 5)))
         ("M-6" . (lambda () (interactive) (switch-vterm 6)))
         ("M-7" . (lambda () (interactive) (switch-vterm 7)))
         ("M-8" . (lambda () (interactive) (switch-vterm 8)))
         ("M-9" . (lambda () (interactive) (switch-vterm 9)))
         (:map vterm-mode-map
               ("C-<backspace>" . (lambda ()
                                    (interactive)
                                    (vterm-send-key (kbd "C-w"))))))

  :config
  ;; Don't query about killing vterm buffers, just kill it
  (defadvice vterm (after kill-with-no-query nil activate)
    (set-process-query-on-exit-flag (get-buffer-process ad-return-value) nil)))

;;;===========================================================================
;;; TeX/LaTeX stuff
;;;===========================================================================

(use-package auctex
  ;; TeX/LaTeX editing mode
  :defer nil
  :hook
  (LaTeX-mode . turn-on-prettify-symbols-mode)
  (LaTeX-mode . turn-on-flyspell))

;;;===========================================================================
;;; PDF stuff
;;;===========================================================================

(use-package pdf-tools
  ;; Handle viewing/annotating/etc. PDF files
  :ensure t
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

;;;===========================================================================
;;; Region/selection-related
;;;===========================================================================

(use-package selected
  ;; Set up keys/actions to only be available when there's an active selection
  :ensure t
  :delight (selected-minor-mode)
  :commands (selected-global-mode)
  :init
  (defvar selected-org-mode-map)
  (setq selected-org-mode-map (make-sparse-keymap))
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines)
              ("<tab>" . indent-region)
              ("#" . comment-or-uncomment-region)
              :map selected-org-mode-map
              ("t" . org-table-convert-region))
  :config
  (selected-global-mode))

(use-package expand-region
  ;; Region expansion with a simple key
  :ensure t
  :bind ("C-=" . er/expand-region))

;;;===========================================================================
;;; Misc tools
;;;===========================================================================

(use-package deadgrep
  ;; Ripgrep super-tool
  :ensure t
  :defer t
  :bind ("M-/" . deadgrep))

(use-package marginalia
  ;; Provides context to items in minibuffer completions, etc.
  :ensure t
  :commands (marginalia-mode)
  :custom (marginalia-annotators '(marginalia-annotators-light))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :commands (embark-prefix-help-command embark-eldoc-first-target)
  :bind
  (("C-]" . embark-act)
   ("C-\\" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package windmove
  ;; (Slightly) Easier movement between windows.
  :bind (("C-c <up>" . windmove-up)
         ("C-c <right>" . windmove-right)
         ("C-c <down>" . windmove-down)
         ("C-c <left>" . windmove-left)))

(use-package helpful
  ;; Buffed-up Help mode and buffers.
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h d" . helpful-at-point)
         ("C-h F" . helpful-function))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;;;===========================================================================
;;; Anything that is specific to an OS/platform.
;;;===========================================================================

(use-package mac
  :if *is-mac*
  :bind (:map global-map
              ("s-<up>" . my/home)
              ("s-<down>" . my/down)
              ("s-<left>" . previous-buffer)
              ("s-<right>" . next-buffer)))

;;;===========================================================================
;;; End of `use-package' parts.
;;;===========================================================================

;; If there is a directory under ~/.emacs.d named for this host, load all *.el
;; files within it:
(let ((hostdir (concat user-emacs-directory *system-name*)))
  (when (file-directory-p hostdir)
    (dolist (host-el-file (directory-files hostdir t "\\.el$"))
      (load-file host-el-file))))

(provide 'init)

;;; init.el ends here

;; LocalWords:  init Theming deadgrep minibuffer Eglot elpa
