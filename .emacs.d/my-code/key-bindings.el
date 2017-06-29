;;; Global-level key-bindings. Not to be confused with mode-level keys, which
;;; should generally be done via mode hooks.

;; Easy one-line-at-a-time scrolling.
(global-set-key (kbd "H-.")
                (lambda ()
                  (interactive)
                  (scroll-down 1)))
(global-set-key (kbd "H-,")
                (lambda ()
                  (interactive)
                  (scroll-up 1)))

(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Browse the kill-ring with C-c k:
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Replace a lisp sexp with its eval'd value:
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Bind expand-region to C-=:
(global-set-key (kbd "C-=") 'er/expand-region)

;; Bindings for funcs from multiple-cursors:
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-\"") 'mc/mark-all-like-this)
(global-set-key (kbd "C-'") 'mc/mark-all-in-region)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Use this instead of hitting M-x all the time:
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Highlighting symbols:
(global-set-key (kbd "C-\\") 'highlight-symbol-at-point)
(global-set-key (kbd "C-.") 'highlight-symbol-next)
(global-set-key (kbd "C-,") 'highlight-symbol-prev)
(global-set-key (kbd "C-;") 'highlight-symbol-query-replace)

;; This one is neat-- make C-w kill a region when the region is active, or
;; otherwise do a backward-kill-word like C-w behaves in things like bash.
(global-set-key "\C-w"
                (lambda (arg)
                  (interactive "p")
                  (cond ((region-active-p)
                         (kill-region (region-beginning) (region-end)))
                        (t (backward-kill-word arg)))))


;; Function-key bindings. Don't go above f8, though, because MacOS grabs f9
;; through f12. And f1-f4 are already in use.
(global-set-key [(f5)]         'call-last-kbd-macro)
(global-set-key [(control f5)] 'edit-last-kbd-macro)

(global-set-key [(f6)]         'search-forward-regexp)
(global-set-key [(control f6)] 'search-backward-regexp)

(global-set-key [(f7)]         'fill-paragraph-or-region)
(global-set-key [(control f7)] 'untabify-buffer-or-region)

(global-set-key [(f8)]         'cider-jack-in)

;; Meta-key combinations
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(meta i)] 'overwrite-mode)
(global-set-key [(meta q)] 'quote)

;; I miss these keys on my Macbook... but at least I have them on full
;; keyboards...
(global-set-key [(insert)] 'overwrite-mode)
(global-set-key [(home)] 'home)
(global-set-key [(end)] 'end)
