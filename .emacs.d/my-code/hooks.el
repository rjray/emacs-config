;;; All my mode-hooks should be defined here, except for those that are
;;; platform-dependent.

(add-hook 'font-lock-mode-hook
          (lambda ()
            (setq font-lock-maximum-decoration 4)))

(add-hook 'c-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (setq c-default-style "bsd")
            (setq c-basic-offset 4)
            (c-set-offset 'case-label '*)
            (c-set-offset 'statement-case-intro '*)
            (c-set-offset 'statement-case-open '*)))

(add-hook 'c++-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (setq c-default-style "bsd")
            (setq c-basic-offset 4)
            (c-set-offset 'case-label '*)
            (c-set-offset 'statement-case-intro '*)))

(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)
(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key clojure-mode-map (kbd "RET")
              'electrify-return-if-match)
            (rainbow-delimiters-mode t)
            (paredit-mode t)))

(add-hook 'cperl-mode-hook
          (lambda ()
            (require 'prove)
            (turn-on-font-lock)
            (define-key cperl-mode-map "\C-cf" 'perl-insert-file-hdr)
            (define-key cperl-mode-map "\C-cl" 'perl-insert-lib-hdr)
            (define-key cperl-mode-map "\C-cs" 'perl-insert-sub-hdr)
            (define-key cperl-mode-map "\C-c%" 'match-paren)
            (define-key cperl-mode-map (kbd "RET") 'newline-and-indent)
            (local-set-key "%" 'self-insert-command)
            (c-set-offset 'inline-open 0)
            (setq tab-width 4)
            (setq cperl-indent-parens-as-block t)
            (setq cperl-close-paren-offset -4)
            (setq cperl-tab-to-comment t)
            (setq cperl-indent-level 4)
            (setq cperl-continued-statement-offset 4)
            (setq cperl-continued-brace-offset 0)
            (setq cperl-brace-offset -4)
            (setq cperl-brace-imaginary-offset 0)
            (setq cperl-label-offset -2)
            (when (string= *system-name* "rjray")
              (setq perlcritic-profile (concat (getenv "HOME")
                                               "/.perlcriticrc-netapp")))))

(add-hook 'ediff-cleanup-hook
          (lambda ()
            (ediff-janitor t)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map "%" 'match-paren)
            (define-key emacs-lisp-mode-map (kbd "RET")
              'electrify-return-if-match)
            (rainbow-delimiters-mode t)
            (paredit-mode t)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (elisp-slime-nav-mode t)))

(add-hook 'lisp-mode-hook
          (lambda ()
            (rainbow-delimiters-mode t)
            (paredit-mode t)
            (define-key lisp-mode-map (kbd "RET")
              'electrify-return-if-match)
            (if (and (featurep 'menubar)
                     current-menubar)
                (progn
                  ;; make a local copy of the menubar, so our modes don't
                  ;; change the global menubar
                  (set-buffer-menubar current-menubar)
                  (add-submenu nil emacs-lisp-mode-menubar-menu)))))

(add-hook 'makefile-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (setq makefile-target-colon "::")
            (setq makefile-macro-assign " = ")
            (setq makefile-tab-after-target-colon t)
            (setq makefile-browser-auto-advance-after-selection-p t)
            (setq makefile-electric-keys t)
            (setq makefile-use-curly-braces-for-macros-p t)))

(add-hook 'mouse-track-click-hook 'id-select-double-click-hook)

(add-hook 'server-done-hook 'delete-frame)
(add-hook 'server-done-hook
          (lambda ()
            (kill-buffer nil)))

(add-hook 'server-switch-hook
          (lambda ()
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))

(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'zencoding-mode)
            (setq zencoding-indentation 2)
            (zencoding-mode)))
(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
            (define-key sgml-mode-map "<" 'xml--html-smart-less-than)
            (define-key sgml-mode-map ">" 'xml--html-smart-greater-than)
            (define-key sgml-mode-map "&" 'xml--html-smart-ampersand)))

;; (add-hook 'slime-repl-mode-hook
;;           (lambda ()
;;             (paredit-mode t)
;;             (define-key slime-repl-mode-map
;;               (read-kbd-macro paredit-backward-delete-key) nil)))

(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)))

;; This is the only way to get the "<" mapping out of html-mode. It has to be
;; removed globally, just removing from html-mode doesn't do it.
(add-hook 'wrap-region-hook
          (lambda ()
            (wrap-region-remove-wrapper "<")))
