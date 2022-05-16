;;; utils.el --- Utility functions.

;;; Commentary:

;;; Basic utility functions, most of which were obtained elsewhere. None of
;;; these should be directly bound to keys (those belong in keys-functions.el).
;;; These should only be things like support code for hooks, menu
;;; functionality, etc. Calls to "add-hook" or "add-submenu" are OK here (as
;;; opposed to hooks.el or misc.el) as long as they only refer to
;;; functionality defined here.

;;; Code:

(require 'cl)

;; From Michał Marczyk, via
;; http://stackoverflow.com/questions/3887362/clojure-functions-for-emacs
(defmacro -> (e &rest es)
  "Port of Clojure's -> to Emacs Lisp."
  (if (and (consp es) (not (consp (cdr es))))
      (if (consp (car es))
          `(,(caar es) ,e ,@(cdar es))
        `(,(car es) ,e))
    (if (consp es)
        `(-> (-> ,e ,(car es)) ,@(cdr es))
      e)))

(defmacro ->> (e &rest es)
  "Port of Clojure's ->> to Emacs Lisp."
  (if (and (consp es) (not (consp (cdr es))))
      (if (consp (car es))
          `(,@(car es) ,e)
        `(,(car es) ,e))
    (if (consp es)
        `(->> (->> ,e ,(car es)) ,@(cdr es))
      e)))

(provide 'utils)
;;; utils.el ends here
