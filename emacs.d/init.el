;;; init.el --- Master Emacs configuration file.  -*- lexical-binding: t; -*-

;; Time-stamp: <2025-07-12 13:03:47>

;; Package-Requires: ((emacs "30.1"))

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
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; This directory holds files that are taken whole from other sources
(add-to-list 'load-path (concat user-emacs-directory "external"))

;; Packages

;; Set emacs customizations that aren't related to packages below. This should
;; be mostly global keybindings and top-level setq's. This appears before all
;; other `use-package' invocations.
(use-package emacs
  :init
  (use-package bookmark)

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

  (defun my/view-exit ()
    "Advice function to disable highlighting upon exiting view-mode."
    (hl-line-mode -1))

  ;; Configuration for `view-buffer' and `view-file'. Mostly taken from Charles
  ;; Choi.
  ;; http://yummymelon.com/devnull/enhancing-navigation-in-emacs-view-mode.html
  (use-package view
    :config
    (add-hook
     'view-mode-hook
     (lambda ()
       (cond ((derived-mode-p 'org-mode)
              (define-key view-mode-map (kbd "p") 'org-previous-visible-heading)
              (define-key view-mode-map (kbd "n") 'org-next-visible-heading))
             ((derived-mode-p 'markdown-mode)
              (define-key view-mode-map (kbd "p") 'markdown-outline-previous)
              (define-key view-mode-map (kbd "n") 'markdown-outline-next))
             ((derived-mode-p 'html-mode)
              (define-key view-mode-map (kbd "p") 'sgml-skip-tag-backward)
              (define-key view-mode-map (kbd "n") 'sgml-skip-tag-forward))
             ((derived-mode-p 'python-mode)
              (define-key view-mode-map (kbd "p") 'python-nav-backward-block)
              (define-key view-mode-map (kbd "n") 'python-nav-forward-block))
             ((derived-mode-p 'emacs-lisp-mode)
              (define-key view-mode-map (kbd "p") 'backward-sexp)
              (define-key view-mode-map (kbd "n") 'forward-sexp))
             ((derived-mode-p 'makefile-mode)
              (define-key view-mode-map (kbd "p") 'makefile-previous-dependency)
              (define-key view-mode-map (kbd "n") 'makefile-next-dependency))
             ((derived-mode-p 'c-mode)
              (define-key view-mode-map (kbd "p") 'c-beginning-of-defun)
              (define-key view-mode-map (kbd "n") 'c-end-of-defun))
             (t
              (define-key view-mode-map (kbd "p") 'scroll-down-command)
              (define-key view-mode-map (kbd "n") 'scroll-up-command)))))

    (add-hook 'view-mode-hook 'hl-line-mode)

    (advice-add 'View-exit :after #'my/view-exit))

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

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
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

  ;; Update time-stamp markers on save (if present in file)
  (time-stamp-active t)
  (time-stamp-line-limit 10)
  (time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S")

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
  (add-to-list 'completion-ignored-extensions ".#")
  ;; Having this in the :hook section didn't work
  (add-hook 'write-file-functions 'time-stamp))

;; `savehist' (minibuffer and related histories)
(use-package savehist
  :hook (after-init . savehist-mode)
  :custom
  (savehist-file (locate-user-emacs-file "savehist"))
  (history-length 100)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t))

;;;===========================================================================
;;; Start-up and general interface packages.
;;;===========================================================================

;; Minor-mode visibility control.
(use-package delight
  :ensure t)

;; General desktop stuff (history, etc.)
(use-package desktop
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

;; Number ALL the lines
(use-package display-line-numbers
  :if (display-graphic-p)
  :custom
  (display-line-numbers-grow-only t)
  :config
  ;; No, seriously... all the lines.
  :hook
  (after-init . global-display-line-numbers-mode))

;; Theming: https://protesilaos.com/emacs/ef-themes
(use-package ef-themes
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

;; Pulse the current line for things like window switching, etc.
(use-package pulsar
  :ensure t
  :commands (pulsar-global-mode pulsar-pulse-line pulsar-highlight-line)
  :config
  (setq pulsar-face 'pulsar-cyan)
  (setq pulsar-delay 0.1)
  (setq pulsar-pulse-region-functions pulsar-pulse-region-common-functions)
  (pulsar-global-mode 1)
  (let ((map global-map))
    (define-key map (kbd "C-x C-p") #'pulsar-pulse-line)
    (define-key map (kbd "C-x C-h") #'pulsar-highlight-line)))

(use-package exec-path-from-shell
  :ensure t
  ;; Set up the exec-path by reading $PATH from a shell
  :hook (after-init . exec-path-from-shell-initialize))

(use-package dwim-shell-command
  :ensure t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))

;; Recent-file tracking and opening
(use-package recentf
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
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-menu-items 40)
  (recentf-max-saved-items 100)
  (recentf-exclude '("\\.ido\\.last" "/recentf$" ".emacs.d/elpa/"))
  :hook (after-init . recentf-mode))

;; Excess whitespace display
(use-package whitespace
  :delight global-whitespace-mode
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face tabs lines-tail)))

;; Support for EditorConfig files. See https://editorconfig.org/
(use-package editorconfig
  :ensure t
  :commands editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package show-font
  :ensure t
  :bind (("C-c s f" . show-font-select-preview)
         ("C-c s l" . show-font-list)))

;;;===========================================================================
;;; Various "porcelain" packages for interface enhancement.
;;;===========================================================================

(use-package casual
  :ensure t)

(use-package casual-bookmarks
  :bind (:map bookmark-bmenu-mode-map
              ("C-S-o" . casual-bookmarks-tmenu)))

(use-package casual-image
  :after image-mode
  :bind (:map image-mode-map
              ("C-o" . casual-image-tmenu)))

(use-package casual-isearch
  :bind (:map isearch-mode-map
              ("C-o" . casual-isearch-tmenu)))

(use-package casual-re-builder
  :bind ((:map reb-mode-map
               ("C-o" . casual-re-builder-tmenu))
         (:map reb-lisp-mode-map
               ("C-o" . casual-re-builder-tmenu))))

;;;===========================================================================
;;; Packages related to command-selection, completion, etc.
;;;===========================================================================

;; Vertico for interactive completion
(use-package vertico
  :ensure t
  :delight
  :commands vertico-mode
  :hook (after-init . vertico-mode))

;; Corfu (in-buffer completion popup), taken from Protesilaos Stavrou.
(use-package corfu
  :ensure t
  :defines corfu-popupinfo-delay
  :functions (corfu-popupinfo-mode corfu-history-mode)
  :hook (after-init . global-corfu-mode)
  ;; I also have (setq tab-always-indent 'complete) for TAB to complete
  ;; when it does not need to perform an indentation change.
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :after corfu
  :hook (after-init . corfu-terminal-mode))

;; Consult for completing-read
(use-package consult
  :ensure t
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

;; Use Dabbrev with Corfu.
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Orderless completion style, also taken from Protesilaos Stavrou.
(use-package orderless
  :ensure t
  :after minibuffer
  :config
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))

  ;; These taken from Prot, renamed to my convention here.
  (defun my/orderless-literal (word _index _total)
    "Read WORD= as a literal string."
    (when (string-suffix-p "=" word)
      ;; The `orderless-literal' is how this should be treated by
      ;; orderless.  The `substring' form omits the `=' from the
      ;; pattern.
      `(orderless-literal . ,(substring word 0 -1))))

  (defun my/orderless-file-ext (word _index _total)
    "Expand WORD. to a file suffix when completing file names."
    (when (and minibuffer-completing-file-name
               (string-suffix-p "." word))
      `(orderless-regexp . ,(format "\\.%s\\'" (substring word 0 -1)))))

  (defun my/orderless-beg-or-end (word _index _total)
    "Expand WORD~ to \\(^WORD\\|WORD$\\)."
    (when-let (((string-suffix-p "~" word))
               (word (substring word 0 -1)))
      `(orderless-regexp . ,(format "\\(^%s\\|%s$\\)" word word))))

  (setq orderless-style-dispatchers
        '(my/orderless-literal
          my/orderless-file-ext
          my/orderless-beg-or-end))

  (setq completion-styles '(basic substring initials flex orderless))

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)
              ("?" . nil)))

;; A go-to-def package that uses ripgrep
(use-package dumb-jump
  :ensure t
  :commands dumb-jump-xref-activate
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-rg-search-args "--hidden --pcre2")
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;;===========================================================================
;;; Project management
;;;===========================================================================

;; Projectile for project-level management
(use-package projectile
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

;; Just using this for the ease of commenting lines language-independent.
(use-package evil-nerd-commenter
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

;; Text snippets/templates expansion
(use-package yasnippet
  :ensure t
  :commands (yas-global-mode)
  :bind (("C-c C-y" . yas-insert-snippet))
  :custom
  (yas-wrap-around-region t)
  (yas-snippet-dirs '("~/.emacs.d/snippets/"))
  :hook (after-init . yas-global-mode))

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

;; Aggressive indenting for some modes
(use-package aggressive-indent
  :ensure t
  :delight
  :hook ((clojure-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)))

;; Parens highlighting
(use-package highlight-parentheses
  :ensure t
  :delight
  :hook ((clojure-mode . highlight-parentheses-mode)
         (emacs-lisp-mode . highlight-parentheses-mode)
         (lisp-mode . highlight-parentheses-mode)))

;; CIDER for Clojure editing
(use-package cider
  :ensure t
  :defer t
  :hook (clojure-mode . cider-mode)
  :custom
  (nrepl-hide-special-buffers t)
  (cider-repl-display-help-banner nil)
  (cider-auto-select-error-buffer t)
  (cider-prompt-for-symbol t)
  (cider-repl-display-in-current-window nil)
  (cider-repl-history-size 1000))

;; Clojure
(use-package clojure-mode
  :ensure t
  :defer t)

;; Clojars.org searching
(use-package clojars
  :ensure t)

;; ElDoc
(use-package eldoc
  :delight eldoc-mode)

;; Couldn't quite get this to work with use-package and :delight.
(require 'delight)
(delight 'emacs-lisp-mode
         '("EL/" (lexical-binding "l" "d"))
         :major)

;; Parens-editing supercharging
(use-package paredit
  :ensure t
  :delight
  :hook ((clojure-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)))

;;;===========================================================================
;;; Perl development
;;;===========================================================================

;; Preferred Perl mode
(use-package cperl-mode
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

;; Markdown editing
(use-package markdown-mode
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

;; Org Mode
(use-package org
  :defer t
  :after flycheck
  :commands (org-link-set-parameters)
  :bind (("C-c C-o" . (lambda ()
                        (interactive)
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

(use-package org-transclusion
  :ensure t
  :after org
  :hook (org-mode . org-transclusion-mode))

;;;===========================================================================
;;; Magit and git-related code
;;;===========================================================================

(use-package diff-hl
  :ensure t
  :defer nil
  :commands (global-diff-hl-mode)
  :hook (after-init . global-diff-hl-mode))

;; Git-related decorations in the gutter
(use-package git-gutter
  :if (display-graphic-p)
  :ensure t
  :commands (global-git-gutter-mode)
  :delight
  :hook (after-init . global-git-gutter-mode))

;; Git "time machine"
(use-package git-timemachine
  :ensure t)

;; Supercharged git interface
(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status))
  :custom
  (magit-diff-highlight-trailing t)
  (magit-diff-paint-whitespace t)
  (magit-process-finish-apply-ansi-colors t)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (define-key magit-status-mode-map (kbd "W")
              (lambda ()
                (interactive)
                (if magit-diff-paint-whitespace
                    (setq magit-diff-paint-whitespace nil)
                  (setq magit-diff-paint-whitespace t)))))

;; Magit support for range-diff
(use-package magit-tbdiff
  :ensure t
  :after magit)

;; Highlighting of to-do-like and similar keywords in Magit buffers
(use-package magit-todos
  :after magit
  :commands (magit-todos-mode)
  :config (magit-todos-mode 1))

(use-package eldoc-diffstat
  :ensure t
  :after magit
  :commands (global-eldoc-diffstat-mode)
  :config (global-eldoc-diffstat-mode))

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

;; File-explorer tree
(use-package treemacs
  :ensure t
  :defer t
  :bind (("C-c t" . treemacs))
  :custom
  (treemacs-width 30))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;;;===========================================================================
;;; Dired-related bits
;;;===========================================================================

;; Dired mode
(use-package dired
  :defer t
  :config
  (defadvice dired-create-directory (after revert-buffer-after-create activate)
    "Revert the buffer after a new directory is created."
    (revert-buffer))
  (defadvice wdired-abort-changes (after revert-buffer-after-abort activate)
    "Revert the buffer after aborting wdired change."
    (revert-buffer)))

;; Writable-dired package
(use-package wdired
  :defer t)

;; Prot's dired-preview package
(use-package dired-preview
  :ensure t
  :after (dired)
  :commands dired-preview-global-mode
  :custom
  (dired-preview-delay 0.25)
  (dired-preview-max-size (expt 2 20))
  (dired-preview-ignored-extensions-regexp
   (concat "\\."
           "\\(gz\\|"
           "zst\\|"
           "tar\\|"
           "xz\\|"
           "rar\\|"
           "zip\\|"
           "iso\\|"
           "epub"
           "\\)"))
  :hook (after-init . dired-preview-global-mode))

;;;===========================================================================
;;; Flycheck bits
;;;===========================================================================

;; Flycheck
(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :config
  (setq flycheck-global-modes '(not org-mode))
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Clojure support for Flycheck
(use-package flycheck-clojure
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

;; TeX/LaTeX editing mode
(use-package auctex
  :defer t
  :hook  ((LaTeX-mode . turn-on-prettify-symbols-mode)
          (LaTeX-mode . turn-on-flyspell)))

;;;===========================================================================
;;; PDF stuff
;;;===========================================================================

;; Handle viewing/annotating/etc. PDF files
(use-package pdf-tools
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

;; Set up keys/actions to only be available when there's an active selection
(use-package selected
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

;; Region expansion with a simple key
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;;;===========================================================================
;;; Icons
;;;===========================================================================

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :commands (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;;===========================================================================
;;; AI/LLM/Ollama tools
;;;===========================================================================

;; Interface to ChatGPT/Ollama/etc.
(use-package chatgpt-shell
  :ensure t
  :defer t)

(use-package ollama-buddy
  :ensure t
  :defer t
  :bind ("C-c o" . ollama-buddy-menu))

;;;===========================================================================
;;; Misc tools
;;;===========================================================================

;; Ripgrep super-tool
(use-package deadgrep
  :ensure t
  :defer t
  :bind ("M-/" . deadgrep))

;; Provides context to items in minibuffer completions, etc.
(use-package marginalia
  :ensure t
  :commands (marginalia-mode)
  :custom (marginalia-annotators '(marginalia-annotators-light))
  :init
  (marginalia-mode))

;; (Slightly) Easier movement between windows.
(use-package windmove
  :bind (("C-c <up>" . windmove-up)
         ("C-c <right>" . windmove-right)
         ("C-c <down>" . windmove-down)
         ("C-c <left>" . windmove-left)))

;; Buffed-up Help mode and buffers.
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h d" . helpful-at-point)
         ("C-h F" . helpful-function)))

;; Show in minibuffer the possible keybindings available based on what has
;; been typed thus far.
(use-package which-key
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

;; Clear out trailing whitespace on saves
(use-package stripspace
  :ensure t
  :delight
  :commands stripspace-local-mode

  ;; Enable for prog-mode-hook, text-mode-hook, prog-mode-hook
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))

  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean nil)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))

;; Run web searches within Emacs
(use-package emacs-websearch
  :vc (:url "https://github.com/zhenhua-wang/emacs-websearch")
  :bind (("C-c l" . emacs-websearch)))

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
