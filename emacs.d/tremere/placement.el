;;; placement.el --- Force size and placement of initial frame

;;; Commentary:

;;; Code:

(setq initial-frame-alist
      '((top . -8)
        (left . 2550)
        (width . 180)
        (height . 80)))

(add-to-list 'default-frame-alist
             '(font . "Cascadia Code-10"))

(provide 'placement)
;;; placement.el ends here
