;;; Anything that doesn't fit into the other files under my-code/. This should
;;; be loaded after key-bindings.el, et al.

(defadvice cperl-indent-command
  (around cperl-indent-or-complete)

  "Changes \\[cperl-indent-command] so it autocompletes when at the end of a word."
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    ad-do-it))
(eval-after-load "cperl-mode"
  '(progn (require 'dabbrev) (ad-activate 'cperl-indent-command)))

(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files "~/.schema/schemas.xml"))

;; Set some default settings
(setq-default
 default-case-fold-search nil
 x-select-enable-clipboard 1
 tramp-default-method "ssh"

 ; Startup stuff supression
 inhibit-splash-screen t
 inhibit-startup-echo-area-message t
 inhibit-startup-screen t

 ; Backup stuff
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 auto-save-list-file-name nil
 delete-auto-save-files t

 ; Inhibit "magic" mode selection
 magic-mode-alist nil

 ; Tabs and lines
 tab-stop-list (mapcar (lambda (x) (* 4 x))
                       '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
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

;; Set-up for IDO mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-ignore-extensions t
      ido-auto-merge-work-directories-length -1
      ido-file-extensions-order
      '(".org" ".txt" ".pm" ".pl" ".clj" ".emacs" ".xml" ".el")
      ido-ignore-buffers (list (rx (or (and bos  " ")
                                       (and bos
                                            (or "*Completions*"
                                                "*Shell Command Output*"
                                                "*vc-diff*")
                                            eos)))))
(define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
;;; Allow spaces when using ido-find-file
(add-hook 'ido-make-file-list-hook
          (lambda ()
            (define-key ido-file-dir-completion-map
              (kbd "SPC") 'self-insert-command)))

;; Set-up for SLIME
(slime-setup '(slime-fancy slime-asdf))
(unload-feature 'slime-autodoc t)
(setq slime-multiprocessing t)
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)
(cond ((string-match "Aquamacs" emacs-build-system)
       (setq slime-lisp-implementations
             '((clisp   ("/usr/local/bin/clisp" "-K full"))
               (sbcl    ("/usr/local/bin/sbcl")))))
      (t
       (setq slime-lisp-implementations
             '((clisp   ("/usr/bin/clisp" "-K full"))
               (sbcl    ("/usr/bin/sbcl"))))))
(setf slime-default-lisp 'sbcl)

;; Whitespace
(setq whitespace-style '(face tabs lines-tail))
(global-whitespace-mode 1)

;; Wrap region minor mode
(wrap-region-global-mode t)

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
