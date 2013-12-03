;;; Various auto-load commands to associate function names and mode names with
;;; specific files

(autoload 'prove "prove" "Perl prove" t)

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(autoload 'yaml-mode "yaml-mode" "YAML Mode" t)

(autoload 'clojure-mode "clojure-mode" "Clojure mode" t)
(autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
(autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)

(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(autoload 'gfm-mode "markdown-mode" "GitHub-Flavored Markdown mode" t)

(autoload 'mediawiki-mode "mediawiki" "MediaWiki mode" t)

(autoload 'xml-mode "nxml" "XML Editing Mode" t)

(autoload 'perlcritic        "perlcritic" "" t)
(autoload 'perlcritic-region "perlcritic" "" t)
(autoload 'perlcritic-mode   "perlcritic" "" t)

(autoload 'elisp-slime-nav-mode "elisp-slime-nav" "" t)
