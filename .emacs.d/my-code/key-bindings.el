;;; Global-level key-bindings. Not to be confused with mode-level keys, which
;;; should generally be done via mode hooks.

;; Easy one-line-at-a-time scrolling. Find the syntax for this way of
;; providing the keyspec a little odd, but I couldn't get it to work
;; any other way...
(global-set-key [?\C-.]
                (lambda ()
                  (interactive)
                  (scroll-down 1)))
(global-set-key [?\C-,]
                (lambda ()
                  (interactive)
                  (scroll-up 1)))

;; Browse the kill-ring with C-c k:
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Bind expand-region to C-=:
(global-set-key (kbd "C-=") 'er/expand-region)

;; Use this instead of hitting M-x all the time:
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-c\C-i" 'swap-tab-width)
(global-set-key "\C-x\C-r" 'ido-recentf-open)

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
(global-set-key [(f5)]           'call-last-kbd-macro)
(global-set-key [(control f5)]   'edit-last-kbd-macro)

(global-set-key [(f6)]           'search-forward-regexp)
(global-set-key [(control f6)]   'search-backward-regexp)

(global-set-key [(f7)]           'fill-paragraph-or-region)
(global-set-key [(control f7)]   'untab-buffer)

(global-set-key [(f8)]           (lambda ()
                                   (interactive)
                                   (cond ((region-active-p)
                                          (count-region (region-beginning)
                                                        (region-end)))
                                         (t (count-region (point-min)
                                                          (point-max))))))

;; Meta-key combinations
(global-set-key [(meta g)]       'goto-line)
(global-set-key [(meta i)]       'overwrite-mode)
(global-set-key [(meta q)]       'quote)

;; I miss these keys on my Macbook... but at least I have them on full
;; keyboards...
(global-set-key [(insert)] 'overwrite-mode)
(global-set-key [(home)] 'home)
(global-set-key [(end)] 'end)
