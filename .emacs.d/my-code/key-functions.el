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

;; From https://www.emacswiki.org/emacs/RecentFiles
(defun recentf-open-files-compl ()
  "Open a file from the recent-files list, with completion."
  (interactive)
  (let* ((tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
                        recentf-list))
         (fname (completing-read "File name: " tocpl nil nil)))
    (when fname
      (find-file (cdr (assoc-string fname tocpl))))))

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

(provide 'key-functions)
;;; key-functions.el ends here
