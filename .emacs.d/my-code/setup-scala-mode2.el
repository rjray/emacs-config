;;; Load and configuration of scala-mode2

(require 'scala-mode2)

(setq
 scala-indent:default-run-on-strategy 'eager
 scala-indent:indent-value-expression t
 scala-indent:align-parameters t
 scala-indent:align-forms t
 scala-indent:use-javadoc-style t)

(add-hook 'scala-mode-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)
             (local-set-key (kbd "M-RET") 'join-line)
             (local-set-key (kbd "<backtab>")
                            'scala-indent:indent-with-reluctant-strategy)))

(provide 'setup-scala-mode2)
