;;; Basic utility functions, most of which were obtained elsewhere. None of
;;; these should be directly bound to keys (those belong in keys-functions.el).
;;; These should only be things like support code for hooks, menu
;;; functionality, etc. Calls to "add-hook" or "add-submenu" are OK here (as
;;; opposed to hooks.el or misc.el) as long as they only refer to
;;; functionality defined here.

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

;; From Joost Diepenmaat, via
;; https://zeekat.nl/articles/making-emacs-work-for-me.html
(defun my/->string (str)
  "Coerce a string from STR, whether passed a string or a symbol."
  (cond
   ((stringp str) str)
   ((symbolp str) (symbol-name str))))

(defun my/->mode-hook (name)
  "Turn mode name NAME into hook symbol."
  (intern (replace-regexp-in-string "\\(-mode\\)?\\(-hook\\)?$"
                                    "-mode-hook"
                                    (my/->string name))))

(defun my/->mode (name)
  "Turn mode name NAME into mode symbol."
  (intern (replace-regexp-in-string "\\(-mode\\)?$"
                                    "-mode"
                                    (my/->string name))))
