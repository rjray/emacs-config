;;; placement.el --- Force size and placement of initial frame

;;; Commentary:

;;; Code:

(setq initial-frame-alist
      '((top . 22)
        (left - 0)
        (width . 180)
        (height . 70)))

(add-to-list 'default-frame-alist
             '(font . "Cascadia Code-11"))

(provide 'placement)
;;; placement.el ends here
