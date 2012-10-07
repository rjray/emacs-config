;;; Taken and modified from https://github.com/magnars/.emacs.d

(require 'yasnippet)

;; Search-dirs: First look in a directory specific to the system I'm on,
;; then in the github-shared directory, and lastly in the defaults snippets
;; shipped with YASnippet.
(setq yas-snippet-dirs (list
                        (concat *emacsdir* system-name "/snippets")
                        (concat *emacsdir* "snippets")
                        (concat *emacsmodules* "/yasnippet/snippets")))

;; Snippets everywhere!
(yas-global-mode 1)

;; Jump to the end of a snippet def
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

;; Moving within an active field
(defun yas--goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line)
      (goto-char position))))

(defun yas--goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line)
      (goto-char position))))

(define-key yas-keymap (kbd "C-a") 'yas--goto-start-of-active-field)
(define-key yas-keymap (kbd "C-e") 'yas--goto-end-of-active-field)

;; No dropdown menus
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

(provide 'setup-yasnippet)
