;;; Building up of/alteration of the auto-mode-alist is limited to here. Other
;;; variables similar in nature (interpreter-mode-alist) are covered here, as
;;; well.

(defun extensions-list (mode extlist)
  (mapcar '(lambda (x) (cons x mode)) extlist))

(setq auto-mode-alist
      (append
       ;; Perl mode
       (extensions-list 'cperl-mode
                        '("\\.pm$" "\\.PM$" "\\.PL$" "\\.pl$" "\\.al$"
                          "\\.t$" "\\.thpl$"))

       ;; HTML mode is applied to several
       (extensions-list 'html-mode
                        '("\\.html?" "\\.HTML?"))

       ;; NXML
       (extensions-list 'nxml-mode
                        '("\\.rdf$" "\\.xsd$" "\\.xslt?$" "\\.xml$"
                          "\\.xhtml?$" "\\.dbx$" "\\.page$"))

       ;; Extras for C mode and related
       (list (cons "\\.xs$" 'c-mode))

       ;; Edit bash-related files in sh-mode
       (list (cons "\\.bash" 'sh-mode))

       ;; TT mode
       (list (cons "\\.tt$" 'tt-mode))

       ;; YAML mode
       (extensions-list 'yaml-mode
                        '("\\.yml$" "\\.yaml$"))

       ;; Clojure mode
       (list (cons "\\.clj$" 'clojure-mode))

       ;; Wikimedia mode, specifically for ItsAllText emacsclient buffers
       (list (cons ".*wiki.*\\.txt" 'mediawiki-mode))

       ;; Markdown mode
       (extensions-list 'gfm-mode
                        '("\\.md$" "\\.markdown$"))

       ;; Edit .gitconfig and .gitmodules in conf-mode
       (list (cons "\\.git\\(modules\\|config\\)$" 'conf-mode))

       auto-mode-alist))

(setq interpreter-mode-alist (append interpreter-mode-alist
                                     '(("miniperl" . perl-mode))))
