;;; hooks.el --- Mode hooks.

;;; Commentary:

;;; All my mode-hooks should be defined here, except for those that are
;;; platform-dependent.

;;; Code:

(require 'diminish)
(require 'recentf)
(require 'cperl-mode)
(require 'flycheck)

(add-hook 'font-lock-mode-hook
          (lambda ()
            (setq font-lock-maximum-decoration 4)))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-whitespace-mode)
(add-hook 'after-init-hook 'save-place-mode)
(add-hook 'after-init-hook 'recentf-mode)
(add-hook 'after-init-hook
          (lambda ()
            (dolist (mode '(global-whitespace
                            paredit
                            git-gutter
                            subword
                            eldoc))
              (diminish (my/->mode mode) ""))))

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'aggressive-indent-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)

(add-hook 'cperl-mode-hook
          (lambda ()
            (define-key cperl-mode-map (kbd "RET") 'newline-and-indent)
            (c-set-offset 'inline-open 0)
            (setq cperl-indent-parens-as-block t)
            (setq cperl-close-paren-offset -4)
            (setq cperl-indent-level 4)
            (setq cperl-continued-statement-offset 4)
            (setq cperl-continued-brace-offset 0)
            (setq cperl-brace-offset -4)
            (setq cperl-brace-imaginary-offset 0)
            (setq cperl-label-offset -2)))

(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-hook 'flycheck-mode-hook
          (lambda ()
            (setq flycheck-emacs-lisp-load-path 'inherit)))

(add-hook 'lisp-mode-hook 'aggressive-indent-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)

(add-hook 'mouse-track-click-hook 'id-select-double-click-hook)

(add-hook 'recentf-mode-hook
          (lambda ()
            (setq recentf-auto-cleanup 'never
                  recentf-max-menu-items 40
                  recentf-max-saved-items 100
                  recentf-exclude '("\\.ido\\.last" "/itsalltext/" "/recentf$"
                                    ".emacs.d/elpa/"))))

(add-hook 'server-done-hook 'delete-frame)
(add-hook 'server-done-hook
          (lambda ()
            (kill-buffer nil)))

(add-hook 'server-switch-hook
          (lambda ()
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))

(provide 'hooks)
;;; hooks.el ends here
