;;; All my mode-hooks should be defined here, except for those that are
;;; platform-dependent.

(add-hook 'font-lock-mode-hook
          (lambda ()
            (setq font-lock-maximum-decoration 4)))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-whitespace-mode)
(add-hook 'after-init-hook 'wrap-region-global-mode)
(add-hook 'after-init-hook
          (lambda ()
            (dolist (mode '(global-whitespace
                            wrap-region
                            paredit
                            company
                            git-gutter
                            eldoc))
              (diminish (my/->mode mode) ""))))

(add-hook 'c-mode-hook
          (lambda ()
            (setq c-default-style "bsd")
            (setq c-basic-offset 4)
            (c-set-offset 'case-label '*)
            (c-set-offset 'statement-case-intro '*)
            (c-set-offset 'statement-case-open '*)))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-default-style "bsd")
            (setq c-basic-offset 4)
            (c-set-offset 'case-label '*)
            (c-set-offset 'statement-case-intro '*)))

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'aggressive-indent-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook 'cperl-mode-hook
          (lambda ()
            (define-key cperl-mode-map (kbd "RET") 'newline-and-indent)
            (local-set-key "%" 'self-insert-command)
            (c-set-offset 'inline-open 0)
            (setq cperl-indent-parens-as-block t)
            (setq cperl-close-paren-offset -4)
            (setq cperl-tab-to-comment t)
            (setq cperl-indent-level 4)
            (setq cperl-continued-statement-offset 4)
            (setq cperl-continued-brace-offset 0)
            (setq cperl-brace-offset -4)
            (setq cperl-brace-imaginary-offset 0)
            (setq cperl-label-offset -2)))

(add-hook 'css-mode-hook 'rainbow-mode)

(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-hook 'flycheck-mode-hook
          (lambda ()
            (setq flycheck-emacs-lisp-load-path 'inherit)))

(add-hook 'lisp-mode-hook 'aggressive-indent-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq makefile-target-colon "::")
            (setq makefile-macro-assign " = ")
            (setq makefile-tab-after-target-colon t)
            (setq makefile-browser-auto-advance-after-selection-p t)
            (setq makefile-electric-keys t)
            (setq makefile-use-curly-braces-for-macros-p t)))

(add-hook 'mediawiki-mode-hook
          (lambda ()
            (auto-fill-mode -1)
            (define-key mediawiki-mode-map "<" 'xml--html-smart-less-than)
            (define-key mediawiki-mode-map ">" 'xml--html-smart-greater-than)
            (define-key mediawiki-mode-map "&" 'xml--html-smart-ampersand)))

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

(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)))

;; This is the only way to get the "<" mapping out of html-mode. It has to be
;; removed globally, just removing from html-mode doesn't do it.
(add-hook 'wrap-region-hook
          (lambda ()
            (wrap-region-remove-wrapper "<")))
