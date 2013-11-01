;;; Load and configuration of CIDER

(require 'cider)

(setq
 nrepl-hide-special-buffers t
 ;cider-repl-pop-to-buffer-on-connect nil
 ;cider-popup-stacktraces nil
 cider-repl-popup-stacktraces t
 cider-auto-select-error-buffer t
 nrepl-buffer-name-show-port t
 cider-repl-display-in-current-window t
 cider-repl-history-size 1000)

(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook 'cider-mode)

(provide 'setup-cider)
