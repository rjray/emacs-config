;;; early-init.el --- Pre-init init  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; First things loaded/evaluated by Emacs.

;;; Code:

;; Adapted from https://git.mitchmarq42.xyz/mitch/emacs.d/
(defun display-startup-echo-area-message ()
  "Replace the startup message in the echo area."
  (message
   (concat (emacs-init-time) ", gc ran " (number-to-string gcs-done) " times")))

;; Allow high initial memory allocation.
(setq gc-cons-threshold (* 1024 1024 1024))

;; Startup stuff suppression
(setq inhibit-splash-screen t
      inhibit-startup-screen t)

;; Unwanted UI elements
(unless (display-graphic-p)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Don't pop up error window on native-comp issues
(defvar native-comp-async-report-warnings-errors 'silent)

(provide 'early-init)

;;; early-init.el ends here
