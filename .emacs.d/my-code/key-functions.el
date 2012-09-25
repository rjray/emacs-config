;;; These are the defun's tied to key-combos in the other config files,
;;; mostly key-bindings.el.

;; Taken from crisp.el, written by Gary D. Foster <Gary.Foster@corp.sun.com>
;--------------------------------------------------------------------------
;                                                                  HOME/END
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

;; I forget where I got this
(defun count-region (start end)
  "Count lines, words and characters in region."
  (interactive "r")
  (let ((l (count-lines start end))
        (w (count-words start end))
        (c (- end start)))
    (message "Region has %d line%s, %d word%s and %d character%s."
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

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[({[]") (forward-sexp 1) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp 1))
        (t (self-insert-command (or arg 1)))))

;; Insert, at the point, pre-formed headers for a variety of types
(defun insert-text-hdr (language type)
  "Basic insert-header function, encapsulated by other language-specific calls."
  (interactive)
  (insert-file-contents (concat *homedir* "/lib/" language "/" type "_hdr")))

;; Create the language- and type-specific calls
(let ((languages '("perl" "c" "c++" "java" "tcl" "lisp"))
      (types     '("file" "sub" "lib")))
  (mapcar '(lambda (language)
             (mapcar '(lambda (type)
                        (let ((function-name (format "%s-insert-%s-hdr"
                                                     language type))
                              (docu-string
                               (format "Insert ~/lib/%s/%s_hdr at current point"
                                       language type)))
                          (eval (list 'defun (intern function-name) '()
                                      docu-string
                                      (list 'interactive)
                                      (list 'insert-text-hdr language type)))))
                     types))
          languages))

(defun untabify-buffer-or-region ()
  "Untabify the entire buffer. If the region is active, only untabify the
region."
  (interactive)
  (cond ((region-active-p) (untabify (region-beginning) (region-end)))
        (t (untabify (point-min) (point-max)))))

(defun indent-buffer-or-region ()
  "Indent the entire buffer. If the region is active, only indent the
region."
  (interactive)
  (cond ((region-active-p) (indent-region (region-beginning) (region-end)))
        (t (indent-region (point-min) (point-max)))))

(defun cleanup-buffer-or-region ()
  "Clean up the current buffer by untabifying it, deleting trailing whitespace
and re-indenting it. If the region is active, only act on the region."
  (interactive)
  (untabify-buffer-or-region)
  (indent-buffer-or-region)
  (cond ((region-active-p)
         (save-excursion
           (save-restriction
             (narrow-to-region (region-beginning) (region-end))
             (delete-trailing-whitespace)
             (widen))))
        (t (delete-trailing-whitespace))))

(defun fill-paragraph-or-region ()
  "If the region is active, call fill-region. Otherwise, fill-paragraph."
  (interactive)
  (cond ((region-active-p) (fill-region (region-beginning) (region-end)))
        (t (fill-paragraph nil))))

;; Borrowed from hm--html-mode. These aren't currently bound in nXML or
;; anything, but I'm keeping them for now.
(defvar xml--just-insert-less-than nil
  "Internal variable.")

(defun xml--html-less-than ()
  "Inserts the entity '&gt;'."
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
  "Inserts the entity '&gt;'."
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
  "Inserts the entity '&amp;'."
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

;; Hack to swap tab-width between 4 and 8 at a whim
(defun swap-tab-width ()
  "Swap the tab width between 4 and 8"
  (interactive)
  (cond ((= tab-width 4)
         (setq tab-width 8))
        (t (setq tab-width 4)))
  (redraw-display))

;; Set up electric-return that can be used in Lisp/ParEdit modes
(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; Some key-functions for YASnippet, taken from
;; https://github.com/magnars/.emacs.d/
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
        (position (yas/field-end (yas/snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
        (position (yas/field-start (yas/snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line)
      (goto-char position))))

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
