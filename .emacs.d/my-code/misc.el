;;; Anything that doesn't fit into the other files under my-code/. This should
;;; be loaded after key-bindings.el, et al.

(defadvice cperl-indent-command
    (around cperl-indent-or-complete)

  "Change \\[cperl-indent-command] so it autocompletes when at the end of a word."
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    ad-do-it))
(eval-after-load "cperl-mode"
  '(progn (require 'dabbrev) (ad-activate 'cperl-indent-command)))

;; Don't try to adjust rng-schema-locating-files until the file has been loaded
(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files "~/.schema/schemas.xml"))

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

 ;; Save places in files between visits
 save-place t

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

;; Set-up for recent-file minor mode
(setq recentf-auto-cleanup 'never ;; disable before we start recentf!
      recentf-max-menu-items 40
      recentf-max-saved-items 100
      recentf-exclude '("\\.ido\\.last" "/itsalltext/"))
(recentf-mode 1)

;; Whitespace
(setq whitespace-style '(face tabs lines-tail))

;; Wrap region minor mode
;; Don't screw up key bindings in magit-mode
(add-to-list 'wrap-region-except-modes 'magit-status-mode)
(wrap-region-add-wrapper "<p>" "</p>" "p" 'html-mode)
(wrap-region-add-wrapper "<div>" "</div>" "d" 'html-mode)
(wrap-region-add-wrapper "<li>" "</li>" "l" 'html-mode)
(wrap-region-add-wrapper "<strong>" "</strong>" "s" 'html-mode)
(wrap-region-add-wrapper "<a href=\"\">" "</a>" "a" 'html-mode)
(wrap-region-add-wrapper "<h1>" "</h1>" "h" 'html-mode)
(wrap-region-add-wrapper "*" "*" "*" 'markdown-mode)

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
