;;; setup-org --- Loading and configuration of org-mode

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-journal)

(setq
 org-default-notes-file "~/Dropbox/org/organizer.org"
 org-journal-dir "~/Dropbox/org/journal"
 org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c o")
                (lambda () (interactive)
                  (find-file "~/Dropbox/org/organizer.org")))
(global-set-key (kbd "C-c O")
                (lambda () (interactive)
                  (find-file "~/Dropbox/org")))

(provide 'setup-org)
;;; setup-org.el ends here
