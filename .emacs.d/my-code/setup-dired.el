;;; setup-dired --- Loading and configuring dired

;;; Commentary:

;;; Taken and modified from https://github.com/magnars/.emacs.d

;;; Code:

(require 'dired)
(require 'wdired)

;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "... ")
(dired-details-install)

;; Reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create activate)
  "Revert the buffer after a new directory is created."
  (revert-buffer))

;; Reload dired after quitting wdired
(defadvice wdired-abort-changes (after revert-buffer-after-abort activate)
  "Revert the buffer after aborting wdired change."
  (revert-buffer))

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  "Key-binding to move to the start of the file-name."
  (interactive)
  (backward-char (- (current-column) 2)))

(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

;; M-up is nicer in dired if it moves to the third line - straight to the ".."
(defun dired-back-to-top ()
  "Key-binding to move to the top of file listing."
  (interactive)
  (goto-char (point-min))
  (forward-line 2)
  (dired-back-to-start-of-files))

(define-key dired-mode-map (vector 'remap 'beginning-of-buffer)
  'dired-back-to-top)
(define-key dired-mode-map (vector 'remap 'smart-up)
  'dired-back-to-top)

;; M-down is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  "Key-binding to move to the last file in file listing."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (dired-back-to-start-of-files))

(define-key dired-mode-map (vector 'remap 'end-of-buffer)
  'dired-jump-to-bottom)
(define-key dired-mode-map (vector 'remap 'smart-down)
  'dired-jump-to-bottom)

;; Delete with C-x C-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
(define-key wdired-mode-map (vector 'remap 'beginning-of-buffer)
  'dired-back-to-top)
(define-key wdired-mode-map (vector 'remap 'end-of-buffer)
  'dired-jump-to-bottom)

(provide 'setup-dired)
;;; setup-dired.el ends here
