;;; key-functions.el --- Functions specifically for binding to keys.

;;; Commentary:

;;; These are the defun's tied to key-combos in the other config files,
;;; mostly key-bindings.el.

;;; Code:

;; Taken from crisp.el, written by Gary D. Foster
(defvar last-last-command nil
  "Internal variable.")

(defun home ()
  "Home - begin of line, once more - screen, once more - buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'home) (eq last-last-command 'home))
     (goto-char (point-min)))
    ((eq last-command 'home)
     (move-to-window-line 0))
    (t (beginning-of-line)))
  (setq last-last-command last-command))

(defun end ()
  "End - end of line, once more - screen, once more - buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'end) (eq last-last-command 'end))
     (goto-char (point-max)))
    ((eq last-command 'end)
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

(defun count-words (start end)
  "Return number of words between START and END."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (forward-word 1)
          (setq count (1+ count)))))
    count))

(defun untabify-buffer-or-region ()
  "Untabify the entire buffer. If region is active, only untabify the region."
  (interactive)
  (cond ((region-active-p) (untabify (region-beginning) (region-end)))
        (t (untabify (point-min) (point-max)))))

(defun fill-paragraph-or-region ()
  "If the region is active, call `fill-region'. Otherwise, `fill-paragraph'."
  (interactive)
  (cond ((region-active-p) (fill-region (region-beginning) (region-end)))
        (t (fill-paragraph nil))))

;; Borrowed from hm--html-mode. These aren't currently bound in nXML or
;; anything, but I'm keeping them for now.
(defvar xml--just-insert-less-than nil
  "Internal variable.")

(defun xml--html-less-than ()
  "Insert the entity '&gt;'."
  (interactive)
  (insert "&lt;"))

(defun xml--html-smart-less-than ()
  "Insert a '<' or the entity '&lt;' if you execute this command twice."
  (interactive)
  (if (and (eq last-command 'xml--html-smart-less-than)
           xml--just-insert-less-than)
      (progn
        (delete-char -1)
        (xml--html-less-than)
        (setq xml--just-insert-less-than nil))
    (insert ?<)
    (setq xml--just-insert-less-than t)))

(defvar xml--just-insert-greater-than nil
  "Internal variable.")

(defun xml--html-greater-than ()
  "Insert the entity '&gt;'."
  (interactive)
  (insert "&gt;"))

(defun xml--html-smart-greater-than ()
  "Insert a '>' or the entity '&gt;' if you execute this command twice."
  (interactive)
  (if (and (eq last-command 'xml--html-smart-greater-than)
           xml--just-insert-greater-than)
      (progn
        (delete-char -1)
        (xml--html-greater-than)
        (setq xml--just-insert-greater-than nil))
    (insert ?>)
    (setq xml--just-insert-greater-than t)))

(defvar xml--just-insert-ampersand nil
  "Internal variable.")

(defun xml--html-ampersand ()
  "Insert the entity '&amp;'."
  (interactive)
  (insert "&amp;"))

(defun xml--html-smart-ampersand ()
  "Insert a '&' or the entity '&amp;' if you execute this command twice."
  (interactive)
  (if (and (eq last-command 'xml--html-smart-ampersand)
           xml--just-insert-ampersand)
      (progn
        (delete-char -1)
        (xml--html-ampersand)
        (setq xml--just-insert-ampersand nil))
    (insert ?&)
    (setq xml--just-insert-ampersand t)))

;; For tweaking around with lisp:
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; From http://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph ()
  "Take a multi-line paragraph and make it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(provide 'key-functions)
;;; key-functions.el ends here
