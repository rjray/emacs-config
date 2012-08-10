;;; Basic utility functions, most of which were obtained elsewhere. None of
;;; these should be directly bound to keys (those belong in keys-functions.el).
;;; These should only be things like support code for hooks, menu
;;; functionality, etc. Calls to "add-hook" or "add-submenu" are OK here (as
;;; opposed to hooks.el or misc.el) as long as they only refer to
;;; functionality defined here.

(require 'cl)

(defun my-delete-trailing-whitespace (begin end)
  "Delete trailing whitespace from all lines in region BEGIN and END."
  (save-excursion
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r]+$" nil t)
      (replace-match ""))
    (widen)))

;; From Michał Marczyk, via
;; http://stackoverflow.com/questions/3887362/clojure-functions-for-emacs
(defmacro -> (e &rest es)
  (if (and (consp es) (not (consp (cdr es))))
      (if (consp (car es))
          `(,(caar es) ,e ,@(cdar es))
        `(,(car es) ,e))
    (if (consp es)
        `(-> (-> ,e ,(car es)) ,@(cdr es))
      e)))

(defmacro ->> (e &rest es)
  (if (and (consp es) (not (consp (cdr es))))
      (if (consp (car es))
          `(,@(car es) ,e)
        `(,(car es) ,e))
    (if (consp es)
        `(->> (->> ,e ,(car es)) ,@(cdr es))
      e)))
