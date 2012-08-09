;; @(#) iswitch-buffer.el -- switch between buffers using substrings

;; Copyright (C) 1996 Stephen Eglen
;;
;; Author: Stephen Eglen <stephene@cogs.susx.ac.uk>
;; Maintainer: Stephen Eglen <stephene@cogs.susx.ac.uk>
;; Created: 15 Oct 1996
;; Version: 1.0
;; Keywords: extensions


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; <stephene@cogs.susx.ac.uk>) or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; iswitch-buffer|Stephen Eglen|<stephene@cogs.susx.ac.uk>
;; |switch between buffers using substrings
;; |$Date: 1996/10/29 11:07:38 $|$Revision: 1.7 $|~/packages/iswitch-buffer.el



;;; Installation:
;;
;; To autoload the package, do:
;;
;;      (autoload 'iswitch-default-keybindings "iswitch-buffer"
;;                "Switch buffer by susbtring" t)
;;
;; Or you can just load it normally with a require
;;
;;      (require 'iswitch-buffer)
;;
;; This file overrides the normal keybindings for
;;
;;      C-x b, C-x 4 b and C-x 5 b
;;
;; Adjust the iswitch-load-hook if you don't want new bindings.

;;; Commentary:
;;
;;  Has been tested on emacs 19.27, 19.34 and Xemacs 19.14.
;;
;;; Iswitch -- how it works
;;
;;   As you type in the substring for a buffer match, the buffers
;;   currently matching the substring are displayed as you type.  This
;;   is inspired by the code written by Michael R Cook
;;   <mcook@cognex.com>.  The difference between this code and
;;   Michael's is that his looks for exact matches rather than
;;   substring matches.  Jari should take credit for creating this
;;   function based on Michael's.  (Another related package is icomplete.)
;;
;;
;;   Example, say you are looking for a file called "file.c"
;;   when you first use the iswitch function you get a default suggestion
;;   which is the "other-buffer"
;;
;;      iswitch buffer: file.h
;;
;;   Then I type "i", so this gives me all buffers with "i" in the name:
;;
;;      iswitch buffer: i  {file.h,iswitch-switch-buffer.el,file.data,...}
;;
;;  now I type "l" and I get just a few selections left
;;
;;      iswitch buffer: il  {file.h,file.data,file.c,MAIL,*mail*}
;;
;;  and at this point,  I can either type "e.c" to specify the c file, or
;;  just hit ctrl-s twice so that file.c is at the top of the list:
;;
;;      (C-s) iswitch buffer: il  {file.data,file.c,MAIL,*mail*,file.h}
;;      (C-s) iswitch buffer: il  {file.c,MAIL,*mail*,file.h,file.data}
;;
;;  and finally, I can press RTN to select the first item in the list!
;;  If you want to select the buffer given by the prompt, rather than
;;  the first item in the list press C-j.  If a buffer doesnt exist,
;;  then you will be prompted to create a new buffer.

;;  See the doc string of iswitch-buffer for keybindings.
;;  (describe-function 'iswitch-mode)
;;  Note also that SPC is defined to be the same as C-s.
;;
;;  See also the section on Customisable variables if you want to
;;  change the setup.

;;  Version available from http://www.cogs.susx.ac.uk/users/stephene/emacs

;;; Code:


;;; ............................................................ hooks ...

(defvar iswitch-load-hook
  '(iswitch-define-mode-map iswitch-default-keybindings)
  "*Hook run when file has been loaded.")

(defvar iswitch-mode-hook nil
  "Function(s) to call after starting up iswitch.")

;;; ................................................... User variables ...

(defvar iswitch-buffer-ignore
  '("^ ")
  "*List of regexps or functions matching buffer names to ignore.  For
example, traditional behavior is not to list buffers whose names begin
with a space, for which the regexp is \"^ \".  See the source file for
example functions that filter buffernames.")

(defvar iswitch-regexp nil
 "*Non nil means that iswitch will do regexp
matching. Value can be toggled within iswitch using C-r")

(defvar iswitch-buffer-prompt-newbuffer t
  "*Non nil means prompt user to confirm before creating new buffers.")

(defvar iswitch-buffer-newbuffer t
  "*Non nil means that new buffers can possibly be created if they
dont exist.")

;;; ............................................... internal variables ...


(defconst iswitch-buffer-version (substring "$Revision: 1.7 $" 11 -2)
  "$Id: iswitch-buffer.el,v 1.7 1996/10/29 11:07:38 stephene Exp stephene $

Report bugs to: Stephen Eglen <stephene@cogs.susx.ac.uk>")

(defvar iswitch-buffer-ignore-orig nil
"Stores original value of iswitch-buffer-ignore before iswitch starts")

(defvar iswitch-case nil
"Non-nil means case-sensitive searching of buffer names is performed.")

(defvar iswitch-rescan nil
  "Internal variable -- whether we need to regenerate the list of matching
buffers")


(defvar iswitch-method nil
  "Method by which new buffer is shown. One of 'samewindow, 'otherwindow or
'otherframe")

(defvar iswitch-text nil
  "Stores the users string as it is typed in")

(defvar iswitch-matches nil
  "List of buffers currenly matching the text typed in")

(defvar iswitch-msg nil
  "Message that is displayed on the modeline")

;;; .................................................. mode definition ...

(defvar iswitch-mode nil
  "Name of the minor mode, if non-nil.")
(make-variable-buffer-local 'iswitch-mode)

;;; add iswitch-mode to the list of minor modes.
(or (assq 'iswitch-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(iswitch-mode iswitch-mode))))


(defvar iswitch-mode-map nil
  "Keymap for iswitch-mode.")


;;; define the minor mode map.

(defun iswitch-define-mode-map ()
  (interactive)
  (or iswitch-mode-map
      (let ((map (make-keymap)))
	;;(set-keymap-name map 'iswitch-mode-map) ; only for convienience
	
	;; Bind all printing characters to `iswitch-printing-char'.
	;; This isn't normally necessary, but if a printing character were
	;; bound to something other than self-insert-command in global-map,
	;; then it would terminate the search and be executed without this.
	(let ((i 32)
	      (str (make-string 1 0)))
	  (while (< i 127)
	    (aset str 0 i)
	    (define-key map str 'iswitch-printing-char)
	    (setq i (1+ i))))

	;; Several non-printing chars change the searching behavior.
	
	(define-key map "\C-s" 'iswitch-next-match)
	(define-key map "\C-r" 'iswitch-prev-match)
	(define-key map "\C-t" 'iswitch-toggle-regexp)
	(define-key map "\C-a" 'iswitch-toggle-ignore)
	(define-key map "\C-c" 'iswitch-toggle-case)
	(define-key map "\177" 'iswitch-delete-char)
	(define-key map "\C-g" 'iswitch-abort)
	(define-key map "\C-q" 'iswitch-quote-char)
	;; old bindings
	;;(define-key map "\r" 'iswitch-select-buffer-text)
	;;(define-key map "\t" 'iswitch-select-first-match)
	;; new bindings
	(define-key map "\r" 'iswitch-select-first-match)
	(define-key map "\C-j" 'iswitch-select-buffer-text)
	(define-key map "\t" 'iswitch-complete)
	;; change the binding of the space bar if you want.
	;; space is normally defined to next match.
	(define-key map " " 'iswitch-next-match)
	;;(define-key map " " 'iswitch-printing-char)
	
	(setq iswitch-mode-map map) ) ))


;;; Keybindings
(defun iswitch-default-keybindings ()
  "Set up default keybindings for iswitch"
  (interactive)
  (global-set-key "b" 'iswitch-buffer)
  (global-set-key "4b" 'iswitch-buffer-other-window)
  (global-set-key "5b" 'iswitch-buffer-other-frame))

;;; .................................................... user callable ...
;;; Entry functions

;;;###autoload
(defun iswitch-buffer ()
  "Switch buffer by substring: see documentation of iswitch-mode for details."
  (interactive)
  (setq iswitch-method 'samewindow)
  (iswitch-entry))


;;;###autoload
(defun iswitch-buffer-other-window ()
  "Switch buffer by substring: see documentation of iswitch-mode for details.
Buffer is shown in other window."
  (interactive)
  (setq iswitch-method 'otherwindow)
  (iswitch-entry))



;;;###autoload
(defun iswitch-buffer-other-frame ()
  "Switch buffer by substring: see documentation of iswitch-mode for details.
Buffer is shown in other frame."
  (interactive)
  (setq iswitch-method 'otherframe)
  (iswitch-entry))

(defun iswitch-entry ()
  "Simply fall into iswitch mode"
  (interactive)
  (iswitch-mode))


;;;###autoload
(defun iswitch-mode ()
  "Start iswitch minor mode.  As you type in a string, all of the buffers
matching the string are displayed.  When you have found the buffer you want,
it can then be selected.

Kebindings:
\\<iswitch-mode-map>

\\[iswitch-select-first-match] Pick _first_ selection in list 
\\[iswitch-next-match] Put the first element at the end of the list
\\[iswitch-prev-match] Put the last element at the start of the list

\\[iswitch-select-buffer-text] Select the current prompt as the buffer.  
If no buffer is found, prompt for a new one.
\\[iswitch-complete] Complete a common suffix to the current string that \
matches all buffers
\\[iswitch-toggle-regexp] Toggle rexep searching
\\[iswitch-toggle-ignore] Toggle ignoring certain buffers (see \
iswitch-buffer-ignore)
\\[iswitch-toggle-case] Toggle case-sensitive searching of buffer names
\\[iswitch-abort] Quit
"

  ;; Initialize global vars
  (setq iswitch-text "")
  (setq iswitch-matches nil)
  (setq iswitch-msg "")
  (setq	iswitch-mode " Iswitch");; what appears in the modeline
  (redraw-modeline)

  ;; make the iswitch-mode-map dominant
  ;(setq overriding-terminal-local-map iswitch-mode-map)
  (setq overriding-local-map iswitch-mode-map)

  ;; run any hooks?
  (run-hooks 'iswitch-mode-hook) ; todo -test hooks?

  ;; this is the first time the msg is displayed
  (setq iswitch-rescan t)
  (iswitch-update-msg)
  )

;;; ................................................... mode functions ...

;;; Exit functions
(defun iswitch-abort ()
  "Abort iswitch."
  (interactive)
  (iswitch-done)
  (ding)
  )

(defun iswitch-exit (buf)
  "Exit switch normally and visualize BUF according to ISWITCH-METHOD."
  (interactive)
  (iswitch-done)


  (let ( newbufcreated )
    ;; check that the buffer exists
    (cond
     ((and buf (get-buffer buf))	;does it exist ?

      ;; buffer exists, so view it and then exit
      (iswitch-visit-buf buf)
      (message "" ))

     ;; buffer doesnt exist

     (t
      (if (and iswitch-buffer-newbuffer
	       (or
		(not iswitch-buffer-prompt-newbuffer)

		(and iswitch-buffer-prompt-newbuffer
		     (y-or-n-p
		      (format
		       "No buffer matching '%s', create one? "
		  buf)))))
	  ;; then create a new buffer
	  (progn
	    (setq newbufcreated (get-buffer-create buf))
	    (if (fboundp 'set-buffer-major-mode)
		(set-buffer-major-mode newbufcreated))
	    ;;(iswitch-visit-buf newbufcreated))
	    (iswitch-visit-buf buf))
	;; else wont create new buffer
	(message (format "no buffer matching '%s'" buf))
	)))))



(defun iswitch-visit-buf (buf)
  "Visit buffer BUF in either same window, other window, or other frame."
  (cond
   ( (eq iswitch-method 'samewindow)
     (switch-to-buffer buf))

   ( (eq iswitch-method 'otherwindow)
     (switch-to-buffer-other-window buf))

   ( (eq iswitch-method 'otherframe)
     (switch-to-buffer-other-frame buf))
   )
  )


(defun iswitch-done (&optional nopush edit)
  ;; todo - is mouse ok.
  ;;(setq mouse-leave-buffer-hook nil)
  ;; Called by all commands that terminate iswitch-mode.
  ;; If NOPUSH is non-nil, we don't push the string on the search ring.

  (setq overriding-local-map nil)
  ;(setq overriding-terminal-local-map nil)
  ;; (setq pre-command-hook iswitch-old-pre-command-hook) ; for lemacs
  ;;(iswitch-dehighlight t)

  (setq iswitch-mode nil)
  (redraw-modeline)
  ;;(run-hooks 'iswitch-mode-end-hook)
  )


;;; ............................................ command line handling ...
;;; Processing functions


(defun iswitch-printing-char ()
  "Process a normal printing char"
  (interactive)
  (iswitch-process-char (iswitch-last-command-char)))

(defun iswitch-process-char (char)
  "process another char"
  ;; this is a probably a very inefficient way to do add the text onto
  ;; the end of the text; find a more efficient way!
  (setq iswitch-text (concat iswitch-text (make-string 1 char)))
  (iswitch-update-msg)
  )


(defun iswitch-delete-char ()
  "delete the last character from iswitch-text."
  (interactive)
  (let (len)
    (if (stringp iswitch-text)		;len needed later
	(setq len (length iswitch-text))
      (setq len 0))

    (if (<= len 1)
	(setq iswitch-text "")
      (setq iswitch-text (substring iswitch-text 0 (1- len)))
      )

    (setq iswitch-matches nil)		;Force full rescan

    (iswitch-update-msg)
    ))


(defun iswitch-last-command-char ()
  "General function to return the last command character. (Simple wrapper)"
  last-command-char)



;;; Toggle functions

(defun iswitch-toggle-case ()
  "Toggle the value of iswitch-case"
  (interactive)
  (setq iswitch-case (not iswitch-case))
  (iswitch-update-msg))

(defun iswitch-toggle-ignore ()
  "Toggle the value of iswitch-buffer-ignore between its original value and
nil to switch it off."
  (interactive)
  (if iswitch-buffer-ignore
      (progn
	(setq iswitch-buffer-ignore-orig iswitch-buffer-ignore)
	(setq iswitch-buffer-ignore nil)
	)
    ;; else
    (setq iswitch-buffer-ignore iswitch-buffer-ignore-orig)
    )
  (iswitch-update-msg)
  )



;;; Examples for setting the value of iswitch-buffer-ignore
;;(defun ignore-c-mode (name)
;;  "ignore all c mode buffers -- example function for iswitch"
;;  (save-excursion
;;    (set-buffer name)
;;    (string-match "^C$" mode-name)))

;;(setq iswitch-buffer-ignore '("^ " ignore-c-mode ))
;;(setq iswitch-buffer-ignore '("^ " "\\.c$" "\\.h$"))

(defun iswitch-toggle-regexp ()
  "Toggle the value of iswitch-regexp."
  (interactive)
  (setq iswitch-regexp (not iswitch-regexp))
  (setq iswitch-rescan t); eggy -check this?
  (iswitch-update-msg)
  )

(defun iswitch-update-msg ()
  "Update the msg displayed in the modeline."

  (if iswitch-rescan
      (setq iswitch-matches
	    (if (> (length iswitch-text) 0)

		;;  if there is already a list, reuse it
		(if (> (length iswitch-matches) 1)
		    (iswitch-get-matched-buffers
		     iswitch-text iswitch-regexp iswitch-matches)
		  ;; Otw get fresh list
		  (iswitch-get-matched-buffers iswitch-text iswitch-regexp))

	      ;; else no text typed in, so give a default
	      ;; buffer to change to.
	      (list  (buffer-name (other-buffer))))))


  (setq iswitch-rescan t)

  (setq iswitch-msg (format "%siswitch buffer %s: {%s}"
			    (if iswitch-regexp "Regexp " "")
			    iswitch-text
			    (if iswitch-matches
				(mapconcat 'concat iswitch-matches ",")
				iswitch-matches
			      "<no match>")))
  (message iswitch-msg)
  )


;;; Buffer has been selected

(defun iswitch-select-first-match ()
  "Select the first match in the list of matching buffers.  
If there are no matching buffers, possibly create a new one."
  (interactive)
  (if (> (length iswitch-matches) 0)
      (iswitch-exit (car iswitch-matches))
    ;; else assume that a new buffer is to be creted.
    (iswitch-exit iswitch-text)
    ))



(defun iswitch-select-buffer-text ()
  "Select the buffer named by the prompt, or possibly create a new buffer 
if no such buffer exists."
  (interactive)
  (iswitch-exit iswitch-text))



;;; Rotate through the list of matches

(defun iswitch-next-match ()
  "move onto the next buffer"
  (interactive)
  (let ( (tmp  (car iswitch-matches)) )
    (setq iswitch-matches (cdr iswitch-matches))
    (setq iswitch-matches (append iswitch-matches (list tmp)))

    (setq iswitch-rescan nil)
    (iswitch-update-msg)))

(defun iswitch-prev-match ()
  "move back to the previous buffer"
  (interactive)
  (setq iswitch-matches (iswitch-rotate-list iswitch-matches))
  (setq iswitch-rescan nil)
  (iswitch-update-msg))

;;; Completion functions

(defun iswitch-complete ()
  "Try and complete the current pattern amongst the buffer names"
  (interactive)
  (let (res )

    (setq res (find-common-substring
	       iswitch-matches iswitch-text))
    (if (not (eq res t))
	;; match was not exact
	(setq iswitch-text (find-common-substring
			    iswitch-matches iswitch-text)))
    
    (setq iswitch-rescan nil)
    (iswitch-update-msg)))

(defun word-matching-substring (word)
  "Remove the part of WORD before the first match to STR.  If STR cannot be 
found in WORD, return nil. HACK: currently STR is passed to the function
through the global variable change-word-sub"
  (let  ((m (string-match change-word-sub word)))
    (if m
	(substring word m)
      ;; else no match
      nil)))
;; Note. Change-word should really take two arguments.  However, we
;; are using mapcar, which only passes one argument to the
;; function. So, my naughty solution is to use a global variable to
;; pass the other argument to the function. 



(defun find-common-substring (lis subs)
  "Return the common element of each word in LIS beginning with the 
substring SUBS" 
  (let ( res alist)
    (setq change-word-sub 
	  (if iswitch-regexp
	      subs
	    (regexp-quote subs)))
    (setq res (mapcar 'word-matching-substring lis))
    (setq res (delq nil res)) ;; remove any nil elements (shoudlnt happen)
    (setq alist (mapcar 'iswitch-makealist res)) ;; could use an  OBARRAY
    ;; try-completion returns t if there is an exact match.
    (try-completion subs alist)))

;;; Auxiliary functions

;; taken from listbuf-ignore-buffername-p in listbuf.el
;; by friedman@prep.ai.mit.edu

(defun iswitch-ignore-buffername-p (bufname)
  "returns T if the buffer should be ignored."
  (let ((data	    (match-data))
	(re-list    iswitch-buffer-ignore)
	ignorep
	nextstr
	)
    (while re-list
      (setq nextstr (car re-list))
      (cond
       ((stringp nextstr)
	(if (string-match nextstr bufname)
	    (progn
	      (setq ignorep t)
	      (setq re-list nil))))
       ((fboundp nextstr)
	(if (funcall nextstr bufname)
	    (progn
	      (setq ignorep t)
	      (setq re-list nil))
	  ))
       )
      (setq re-list (cdr re-list)))
    (store-match-data data)

    ;; return the result
    ignorep)
  )


(defun iswitch-get-matched-buffers
  (regexp &optional string-format buffer-list)
  "Return matched buffers. If STRING-FORMAT is non-nil, consider
REGEXP as string.

BUFFER-LIST can be list of buffers or list of strings.
"
  (let* ((case-fold-search  iswitch-case)
	 (list		    (or buffer-list (reverse (buffer-list))))
	 (do-string          (stringp (car list)))
	 name
	 ret
	 )
    (mapcar
     (function
      (lambda (x)

	(if do-string
	    (setq name x)		;We already have the name
	  (setq name (buffer-name x)))

	(cond
	 ((and (or (and string-format (string-match regexp name))
		   (and (null string-format)
			(string-match (regexp-quote regexp) name)))
	       (not (iswitch-ignore-buffername-p name)))
	  (setq ret (cons name ret))
	  ))))
     list)
    ret
    ))


(defun iswitch-makealist (res)
  "return a dotted pair (RES . 1)"
  (cons res 1))

;; from Wayne Mesard <wmesard@esd.sgi.com>
(defun iswitch-rotate-list (lis)
  "Destructively removes the last element from LIS.  
Returns the modified list with the last element prepended to it"
  (if (<= (length lis) 1)
      lis
    (let ((las lis)
	  (prev lis))
      (while (consp (cdr las))
	(setq prev las
	      las (cdr las)))
      (setcdr prev nil)
      (cons (car las) lis))
    ))



;;; Todo:
;;
;;  switching to another frame seems to take a fair while on gnu emacs
;;
;;  doesnt yet take into account the values of
;;  same-window-buffer-names
;;  same-window-regexps
;;  but do they cause a problem?
;;
;;  what is the difference between overriding-terminal-local-map and
;;  overriding-local-map?  I got overriding-terminal-local-map from
;;  isearch.el but the terminal version doesnt exist in emacs 19.27.
;;
;;  hooks have not yet been tested (havent needed to use them).
	
      
;; Another inefficiency:
;; Everytime the iswich-text changes, the whole list of matching
;; buffers is scanned.  However, we already have a list of matching
;; buffers stored in iswitch-matches.  Surely we only need to search
;; amongst iswitch-matches rather than the buffer list if we are just
;; adding new text.  
;; However, sometimes you need to rescan the whole buffer list:
;; when changing case-folding, regexp changing, ignoring certain
;; buffers, deleting a char...  For now, we wont bother changing it,
;; just make a note of it.


(provide   'iswitch-buffer)
(run-hooks 'iswitch-load-hook)

;;; iswitch-buffer.el ends here
