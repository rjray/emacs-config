;;; Taken and modified from https://github.com/magnars/.emacs.d

;; Make the fn key set the "hyper" keysym
(setq ns-function-modifier 'hyper)

;; A more Mac-friendly font (and size)
(set-face-attribute 'default nil :family "Monaco" :height 120)

;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; keybinding to toggle full screen mode
(global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

;; Do something with command+arrow keys
(global-set-key [(super up)] 'beginning-of-buffer)
(global-set-key [(super down)] 'end-of-buffer)
(global-set-key [(super left)] 'previous-buffer)
(global-set-key [(super right)] 'next-buffer)

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

(provide 'mac)
