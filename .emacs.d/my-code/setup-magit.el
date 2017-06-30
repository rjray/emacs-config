;;; Taken and modified from https://github.com/magnars/.emacs.d

(require 'magit)

;; C-x C-k to kill file on line
(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))

(define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; ignore whitespace
(defun magit-toggle-whitespace ()
  "Toggle the ignoring of whitespace."
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  "Set `magit-diff-options' to ignore whitespace."
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  "Set `magit-diff-options' to consider whitespace."
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; magit-related global key bindings
(global-set-key (kbd "C-c m") 'magit-status)

(provide 'setup-magit)
