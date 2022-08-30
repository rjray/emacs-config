;;; setup-dumb-jump --- Loading and configuration of dumb-jump

;;; Commentary:

;;; Code:

(require 'dumb-jump)

(setq dumb-jump-prefer-searcher 'rg)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(provide 'setup-dumb-jump)
;;; setup-dumb-jump.el ends here
