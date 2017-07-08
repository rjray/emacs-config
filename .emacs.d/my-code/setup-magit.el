;;; setup-magit --- Loading and configuring magit

;;; Commentary:

;;; Taken and modified from https://github.com/magnars/.emacs.d

;;; Code:

(require 'magit)

;; Note trailing whitespace and whitespace discrepancies.
(setq
 magit-diff-highlight-trailing t
 magit-diff-paint-whitespace t)

;; ignoring whitespace
(defun magit-toggle-whitespace ()
  "Toggle the ignoring of whitespace."
  (interactive)
  (if magit-diff-paint-whitespace
      (setq magit-diff-paint-whitespace nil)
    (setq magit-diff-paint-whitespace t)))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; magit-related global key bindings
(global-set-key (kbd "C-c m") 'magit-status)

(provide 'setup-magit)
;;; setup-magit.el ends here
