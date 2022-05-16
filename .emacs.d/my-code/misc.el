;;; misc.el --- Miscellaneous things that don't fit anywhere else.

;;; Commentary:

;;; Anything that doesn't fit into the other files under my-code/. This should
;;; be loaded after key-bindings.el, et al.

;;; Code:

;; Set some default settings
(setq-default
 default-case-fold-search nil
 x-select-enable-clipboard 1
 tramp-default-method "ssh"
 fill-column 79
 show-trailing-whitespace t
 transient-mark-mode t

 ;; Startup stuff supression
 inhibit-splash-screen t
 inhibit-startup-echo-area-message t
 inhibit-startup-screen t

 ;; Backup stuff
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 auto-save-list-file-name nil
 delete-auto-save-files t

 ;; How to uniquify similar buffer names
 uniquify-buffer-name-style 'forward

 ;; Inhibit "magic" mode selection
 magic-mode-alist nil

 ;; No double-spaces when I fill a paragraph
 sentence-end-double-space nil

 ;; Always number lines and columns in the status line
 line-number-mode t
 column-number-mode t

 ;; Show indication of the buffer size (right before the line/col)
 size-indication-mode t

 ;; Tabs and lines
 tab-width 4
 indent-tabs-mode nil)

;; Set-up for desktop-save-mode
(require 'desktop)
(setq desktop-save t
      desktop-restore-eager 5
      desktop-globals-to-save (append '((extended-command-history . 30)
                                        (file-name-history        . 100)
                                        (grep-history             . 30)
                                        (compile-history          . 30)
                                        (minibuffer-history       . 50)
                                        (query-replace-history    . 60)
                                        (read-expression-history  . 60)
                                        (regexp-history           . 60)
                                        (regexp-search-ring       . 20)
                                        (search-ring              . 20)
                                        (shell-command-history    . 50)
                                        tags-file-name
                                        register-alist)))
(desktop-save-mode 1)

;; Whitespace
(require 'whitespace)
(setq whitespace-style '(face tabs lines-tail))

;; Visual Bell (flash the mode-line instead of an audio bell)
;; Cribbed from Jason Filsinger, https://github.com/filsinger/emacs-config
(setq visible-bell nil
      ring-bell-function `(lambda ()
                            (let ((mode-line-bell-orig-bg
                                   (face-background 'mode-line))
                                  (mode-line-bell-orig-fg
                                   (face-foreground 'mode-line)))
                              (set-face-background 'mode-line "#ED3B3B")
                              (set-face-foreground 'mode-line "#7F2020")
                              (sit-for 0.1)
                              (set-face-background 'mode-line
                                                   mode-line-bell-orig-bg)
                              (set-face-foreground 'mode-line
                                                   mode-line-bell-orig-fg))))

(provide 'misc)
;;; misc.el ends here
