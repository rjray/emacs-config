;;; mac.el --- Emacs configuration items for Mac.  -*- lexical-binding: t; -*-

;;; Commentary:
;; This is for things that only apply to MacOS.

;;; Code:

(require 'dwim-shell-command)

(setq mac-option-modifier 'super
      mac-right-option-modifier 'hyper)

(defun dwim-shell-commands-macos-toggle-menu-bar-autohide ()
  "Toggle macOS menu bar auto-hide."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Toggle menu bar auto-hide."
   "current_status=$(osascript -e 'tell application \"System Events\" to get autohide menu bar of dock preferences')

if [ \"$current_status\" = \"true\" ]; then
    osascript -e 'tell application \"System Events\" to set autohide menu bar of dock preferences to false'
    echo \"Auto-hide disabled.\"
else
    osascript -e 'tell application \"System Events\" to set autohide menu bar of dock preferences to true'
    echo \"Auto-hide enabled.\"
fi"
   :utils "osascript"
   :silent-success t))

(provide 'mac)
;;; mac.el ends here

