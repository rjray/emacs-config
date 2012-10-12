;;; Taken and modified from https://github.com/magnars/.emacs.d

(require 'ace-jump-mode)

(defun add-hyper-char-to-ace-jump-word-mode (c prefix)
  (define-key global-map
    (read-kbd-macro (concat prefix (string c)))
    `(lambda ()
       (interactive)
       (setq ace-jump-query-char ,c)
       (setq ace-jump-current-mode 'ace-jump-word-mode)
       (ace-jump-do (concat "\\b"
                            (regexp-quote (make-string 1 ,c)))))))

(let ((prefix (cond (*is-mac* "H-")
                    (t "C-s-"))))
  (progn
    (loop for c from ?0 to ?9 do
          (add-hyper-char-to-ace-jump-word-mode c prefix))
    (loop for c from ?A to ?Z do
          (add-hyper-char-to-ace-jump-word-mode c prefix))
    (loop for c from ?a to ?z do
          (add-hyper-char-to-ace-jump-word-mode c prefix))))

(provide 'setup-ace-jump-mode)
