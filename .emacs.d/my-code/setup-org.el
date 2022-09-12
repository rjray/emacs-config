;;; setup-org --- Loading and configuration of org-mode and related modes

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-journal)

;; Set up org-mode:
(setq
 org-default-notes-file "~/Dropbox/org/organizer.org"
 org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c o")
                (lambda () (interactive)
                  (find-file "~/Dropbox/org/organizer.org")))
(global-set-key (kbd "C-c C-o")
                (lambda () (interactive)
                  (find-file "~/Dropbox/org")))

(add-hook 'org-mode-hook 'auto-revert-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "C-c C-j") 'org-journal-new-entry)
              (local-set-key (kbd "C-c j") 'org-goto))))

;; Set up org-journal:
(setq org-journal-dir "~/Dropbox/org/journal")

(add-hook 'org-journal-mode-hook 'auto-fill-mode)

(provide 'setup-org)
;;; setup-org.el ends here
