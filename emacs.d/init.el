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
;; True if this system is Linux. Not used at the moment, but here for future use
(defconst *is-linux* (eq system-type 'gnu/linux) "Is this a Linux system?")

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; This directory holds files that are taken whole from other sources
(add-to-list 'load-path (concat user-emacs-directory "external"))

;; Packages

;; Set emacs customizations that aren't related to packages below. This should
;; be mostly global keybindings and top-level setq's. This appears before all
;; other `use-package' invocations.
(use-package emacs
  :init
  (use-package xref
    ;; A little configuration for Xref
    :commands xref-show-definitions-completing-read
    :config
    ;; Have Xref use `completing-read' to select a target
    (setq xref-show-definitions-function
          #'xref-show-definitions-completing-read))

  (use-package server
    ;; Emacs in server mode
    :commands server-running-p
    :config
    (unless (server-running-p)
      (server-start)))

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
  (defun my/count-region ()
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

  (defun my/fill-paragraph-or-region ()
    "If the region is active, call `fill-region'. Otherwise, `fill-paragraph'."
    (interactive)
    (cond ((region-active-p) (fill-region (region-beginning) (region-end)))
          (t (fill-paragraph nil))))

  (defun my/untabify-buffer-or-region ()
    "Untabify the entire buffer. If region is active, only untabify the region."
    (interactive)
    (cond ((region-active-p) (untabify (region-beginning) (region-end)))
          (t (untabify (point-min) (point-max)))))

  (defun my/text-scale-reset ()
    (interactive)
    (text-scale-set 0))

  ;; This one makes C-w kill a region when the region is active, or
  ;; otherwise do a backward-kill-word like C-w behaves in things like bash.
  (defun my/kill-region-or-word (arg)
    (interactive "p")
    (cond ((region-active-p)
           (kill-region (region-beginning) (region-end)))
          (t (backward-kill-word arg))))

  ;; Toggle two most recent buffers
  (fset 'quick-switch-buffer [?\C-x ?b return])

  :bind (:map global-map
              ("C-x C-m" . execute-extended-command)
              ("C-h h" . nil)
              ("C-w" . my/kill-region-or-word)
              ;; Function-key bindings. Don't go above f8, though, because MacOS
              ;; grabs f9 through f12. And f1-f4 are already in use.
              ("<f5>" . call-last-kbd-macro)
              ("C-<f5>" . edit-last-kbd-macro)
              ("<f6>" . search-forward-regexp)
              ("C-<f6>" . search-backward-regexp)
              ("<f7>" . my/fill-paragraph-or-region)
              ("C-<f7>" . my/untabify-buffer-or-region)
              ("<f8>" . cider-jack-in)
              ("C-!" . my/count-region)
              ("C-+" . text-scale-increase)
              ("C--" . text-scale-decrease)
              ("C-_" . my/text-scale-reset)
              ("<home>" . my/home)
              ("<end>" . my/end)
              ("s-z" . quick-switch-buffer))

  :hook ((text-mode . display-fill-column-indicator-mode)
         (text-mode . hl-line-mode)
         (prog-mode . display-fill-column-indicator-mode)
         (prog-mode . hl-line-mode)
         (prog-mode . flyspell-prog-mode))

  :custom
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
  (setq visible-bell t)
  ;; Settings that are predicated on whether this is a graphical UI.
  (when (display-graphic-p)
    (global-set-key (kbd "C-z") 'undo))
  ;; cperl-mode is preferred to perl-mode
  (defalias 'perl-mode 'cperl-mode)
  ;; Don't care for typing out "yes" and "no" all the time...
  (defvar use-short-answers t)
  ;;enable narrowing
  (put 'narrow-to-region 'disabled nil)
  (subword-mode)
  (set-language-environment 'utf-8)
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
  :if (display-graphic-p)
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

;; Vertico for interactive completion
(use-package vertico
  :ensure t
  :delight
  :commands vertico-mode
  :init
  (vertico-mode))

;; Consult for completing-read
(use-package consult
  :ensure t
  :defer nil
  :delight
  :commands (consult-register-format consult-register-window consult-xref)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package company
  ;; Company mode for completion
  :ensure t
  :delight
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package prescient
  :ensure t
  :commands (prescient-persist-mode)
  :config
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000) ;; More prescient history
  (prescient-persist-mode +1))

;; Use `prescient' for Vertico menus.
(use-package vertico-prescient
  :ensure t
  :after vertico
  :commands vertico-prescient-mode
  :config
  ;; don't prescient sort these commands
  (vertico-prescient-mode +1))

(use-package company-prescient
  :ensure t
  :after company
  :commands (company-prescient-mode)
  :config
  (company-prescient-mode +1))

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

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package dumb-jump
  ;; A go-to-def package that uses ripgrep
  :ensure t
  :commands dumb-jump-xref-activate
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-rg-search-args "--hidden --pcre2")
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;;===========================================================================
;;; Project management
;;;===========================================================================

(use-package projectile
  ;; Projectile for project-level management
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name)))
  :commands (projectile-mode)
  ;; Enable Projectile globally
  :init (projectile-mode +1)
  :config
  ;; Recommended keymap prefix on MacOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
   ;; cider-connection-message-fn #'cider-random-tip
   cider-repl-display-help-banner nil
   cider-auto-select-error-buffer t
   cider-prompt-for-symbol t
   cider-repl-display-in-current-window nil
   cider-repl-history-size 1000))

(use-package clojure-mode
  ;; Clojure
  :ensure t
  :defer t)

(use-package clojars
  ;; Clojars.org searching
  :ensure t)

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
  :mode ("README\\.md\\'" . gfm-mode)
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
  :mode ("\\.ya\\?ml\\'" . yaml-ts-mode))

;;;===========================================================================
;;; Org Mode and related
;;;===========================================================================

(use-package org
  ;; Org Mode
  :defer t
  :after flycheck
  :commands (org-link-set-parameters)
  :bind (("C-c o" . (lambda () (interactive)
                      (find-file "~/Dropbox/org"))))
  :hook (org-mode . auto-revert-mode)
  :custom
  (org-insert-heading-respect-content t)
  (org-outline-path-complete-in-steps nil)
  (org-goto-interface 'outline-path-completion)
  (org-default-notes-file "~/Dropbox/org/organizer.org")
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  :config
  (org-link-set-parameters "copy"
                           :follow (lambda (link) (kill-new link))
                           :export (lambda (_ desc &rest _) desc)))

(use-package org-journal
  ;; Org journaling mode
  :ensure t
  :defer t
  :hook ((org-journal-mode . auto-fill-mode))
  :custom
  (org-journal-dir "~/Dropbox/org/journal"))

;; (use-package org-bullets
;;   :ensure t
;;   :defer t
;;   :commands (org-bullets-mode)
;;   :hook ((org-mode . org-bullets-mode)))

(use-package org-roam
  :ensure t
  :commands (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory "~/Dropbox/org")
  :bind (("C-c C-n l" . org-roam-buffer-toggle)
         ("C-c C-n f" . org-roam-node-find)
         ("C-c C-n g" . org-roam-graph)
         ("C-c C-n i" . org-roam-node-insert)
         ("C-c C-n c" . org-roam-capture)
         ;; Dailies
         ("C-c C-n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more
  ;; informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} "
                                               (propertize "${tags:10}"
                                                           'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

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
  :if (display-graphic-p)
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
  :commands global-flycheck-mode
  :config
  (setq flycheck-global-modes '(not org-mode))
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-clojure
  ;; Clojure support for Flycheck
  :after clojure-mode
  :ensure t)

(use-package flycheck-posframe
  :ensure t
  :commands (flycheck-posframe-mode)
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;;;===========================================================================
;;; Shell/terminal
;;;===========================================================================

;;;===========================================================================
;;; TeX/LaTeX stuff
;;;===========================================================================

(use-package auctex
  ;; TeX/LaTeX editing mode
  :defer t
  :hook  ((LaTeX-mode . turn-on-prettify-symbols-mode)
          (LaTeX-mode . turn-on-flyspell)))

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

(use-package which-key
  ;; Show in minibuffer the possible keybindings available based on what has
  ;; been typed thus far.
  :ensure t
  :delight which-key-mode
  :commands (which-key-mode which-key-setup-minibuffer)
  :custom
  (which-key-idle-delay 0.750)
  (which-key-prefix-prefix "◉ ")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-max-display-columns nil)
  :init
  (which-key-mode)
  :config
  (which-key-setup-minibuffer))

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

;; Lower GC threshold back to something normal.
(setq gc-cons-threshold (* 4 1024 1024))

(provide 'init)

;;; init.el ends here

;; LocalWords:  init Theming deadgrep minibuffer Eglot elpa Treemacs
;; LocalWords:  RecentFiles
