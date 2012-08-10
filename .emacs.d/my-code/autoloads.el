;;; Various auto-load commands to associate function names and mode names with
;;; specific files

(autoload 'prove "prove" "Perl prove" t)

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(autoload 'gdb "gud"
  "Run gdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)
(autoload 'sdb "gud"
  "Run sdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)
(autoload 'dbx "gud"
  "Run dbx on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)
(autoload 'xdb "gud"
  "Run xdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

You can set the variable 'gud-xdb-directories' to a list of program source
directories if your program contains sources from more than one directory."
  t nil)
(autoload 'perldb "gud"
  "Run perldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

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
