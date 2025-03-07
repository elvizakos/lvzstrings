;;; lvzstrings-mode.el --- Minor mode for working with strings on emacs.
;;; -*- coding: utf-8 -*-

;; Copyright © 2019, Nikos Skarmoutsos

;; Version: %%VERSION%%
;; Author: Nikos Skarmoutsos <elvizakos AT yahoo DOT gr>
;; Maintainer: Nikos Skarmoutsos
;; Created: Dec 2019
;; Keywords: string
;; License: GNU General Public License >=2
;; Distribution: This file is not part of Emacs

;;; Code:

;;---- CONSTANTS ------------------------------------------------------------------
(defconst lvzstrings/lvzstrings-version "%%VERSION%%" "LVzStrings version.")

;;---- VARIABLES ------------------------------------------------------------------
(defvar lvzstrings/lvzstrings-keymap (make-sparse-keymap) "Keymap for lvzstrings.")

;;---- OPTIONS --------------------------------------------------------------------
(defgroup lvzstrings nil "LVzStrings minor mode settings."
  :group 'tools)

(defcustom lvzstrings/lighter " LVzA-Z" "Label of minor mode for the modeline."
  :type 'string
  :group 'lvzstrings)

(defcustom lvzstrings/translate-command "/usr/bin/trans %s %s:%s %s" "Shell command to use for translate."
  :type 'string
  :group 'lvzstrings)

(defcustom lvzstrings/dictionary-command "/usr/bin/sdcv %s" "Shell command of the dictionary."
  :type 'string
  :group 'lvzstrings)

(defgroup lvzstrings/lvzstrings-keys nil "LVzStrings minor mode key settings."
  :group 'tools)

(defcustom lvzstrings/lvzstrings-moveup-keycomb "C-s-<up>" "Default key combination for moving text up one line."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-movedown-keycomb "C-s-<down>" "Default key combination for moving text down one line."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-indent-keycomb "C-s-<right>" "Default key combination for indenting current line or selected lines."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-unindent-keycomb "C-s-<left>" "Default key combination for unindenting curret line or selected lines."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-remove-spaces-keycomb "C-c C-v SPC" "Default key combination for removing unnecessary spaces."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-trim-spaces-keycomb "C-c C-v t t" "Default key combination for removing beginning and ending spaces from selection."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-ltrim-spaces-keycomb "C-c C-v t l" "Default key combination for removing beginning spaces from selection."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-rtrim-spaces-keycomb "C-c C-v t r" "Default key combination for removing ending spaces from selection."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-base64-encode-keycomb "C-c C-v b e" "Default key combination for encoding selected string to base64."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-base64-decode-keycomb "C-c C-v b d" "Default key combination for decoding base64 encoded selected string."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-url-encode-keycomb "C-c C-v u e" "Default key combination for encoding selected string for use in a url."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-url-decode-keycomb "C-c C-v u d" "Default key combination for decoding selected url encoded string."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-html-encode-keycomb "C-c C-v h e" "Default key combination for encoding selected string for use in html."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-html-decode-keycomb "C-c C-v h d" "Default key combination for decoding html entities in selected string."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-whitespace-keycomb "C-c C-v C-SPC" "Default key combination for enabling/disabling whitespace mode."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-translate-keycomb "C-c C-v C-y" "Default key combination for translating the text in region."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-dictionary-keycomb "C-c C-v C-w" "Default key combination for searching the word in the dictionary."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-spellcheck-keycomb "C-c C-v s" "Default key combination for spell checking the current buffer."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-spellcheck-region-keycomb "C-c C-v ," "Default key combination for spell checking the region."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-spellcheck-goto-next-error-keycomb "C-c C-v [" " Default key combination for moving to next error for flyspell."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-spellcheck-correct-word-keycomb "C-c C-v ]" "Default key combination for correcting current word."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-langtool-check-keycomb "C-c C-v ;" "Default key combination for checking document using language tool."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-langtool-goto-next-error-keycomb "C-c C-v :" "Default key combination for moving to next error language tool found."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-langtool-correct-buffer-keycomb "C-c C-v '" "Default key combination for correcting the buffer using language tool."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-langtool-check-done-keycomb "C-c C-v \"" "Defualt key combination for ending checking process with language tool."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defgroup lvzstrings/lvzregexes nil "LVzStrings minor mode regular exressions settings."
  :group 'tools)

(defcustom lvzstrings/lvzstrings-onespace-regex "[ ]\\{2,\\}" "Regular expression for removing unnecessary spaces."
  :type 'string
  :group 'lvzstrings/lvzregexes)

;;---- FUNCTIONS ------------------------------------------------------------------
(defun lvzstrings/move-line (n) "Move the current line or selected lines, up or down N lines."
	   (interactive "p")
	   (let (
			 (cpoint (point))  ; Current cursor position
			 (bolpoint 0)	   ; Begining of line position
			 (eolpoint 0)	   ; End of line position
			 (bcpos 0)		   ; Cursor position from begining of line
			 (ecpos 0)		   ; Cursor position from end of line
			 (mvtext "")	   ; Text to move
			 (rb 0)			   ; Region begining
			 (re 0)			   ; Region end
			 (ractive nil)	   ; Region active
			 (lmvt 0)		   ; 
			 )

		 (if (use-region-p)		 ; If there is an active selection
			 (progn
			   (setq ractive t
					 rb (region-beginning)
					 re (region-end))

			   (goto-char rb)
			   (beginning-of-line)
			   (setq bolpoint (point)
					 bcpos 0)			; Move selection to begining of line

			   (goto-char re)
			   (end-of-line)
			   (setq eolpoint (+ (point) 1)
					 ecpos 1			; Move selection to end of line (and include new line character)
					 mvtext (delete-and-extract-region bolpoint eolpoint))
			   )

		   (progn						; If there is no selection
			 (beginning-of-line)
			 (setq bolpoint (point)
				   bcpos (- cpoint bolpoint))
			 (end-of-line)
			 (setq eolpoint (+ (point) 1)
				   ecpos (- eolpoint cpoint)
				   mvtext (delete-and-extract-region bolpoint eolpoint))
			 ))

		 ;; Move it
		 (if (= n 0) (setq n 1))
		 (if (< 0 n)
			 (progn					   ; If n greater than 0 move down
			   (forward-line n)
			   (beginning-of-line)
			   (insert mvtext)
			   (beginning-of-line)
			   (backward-char ecpos)	; Restore cursor position in moved line
			   )
		   (progn						; If n less than 0 move up
			 (if (> bolpoint 0)
				 (progn
				   (forward-line n)
				   (beginning-of-line)
				   (insert mvtext)
				   (beginning-of-line)
				   (backward-char ecpos)
				   ))
			 ))

		 ;; If there was a selection, activate it again
		 (if ractive (progn
					   (setq lmvt (- (length mvtext) 1)
							 deactivate-mark nil)
					   (set-mark (point))
					   (backward-char lmvt)
					   ))
		 ))

(defun lvzstrings/simple-move-line (n) "Move the current line up or down N lines."
	   (interactive "p")
	   (if (> 0 n)						; Moving up
		   (progn
			 (transpose-lines 1)
			 (forward-line -2))
		 (progn							; Moving down
		   (forward-line 1)
		   (transpose-lines 1)
		   (forward-line -1))))

(defun lvzstrings/move-line-up (n) "Move the current line up by N lines."
  (interactive "p")
  (lvzstrings/move-line (if (null n) -1 (- n))))

(defun lvzstrings/move-line-down (n) "Move the current line down by N lines."
  (interactive "p")
  (lvzstrings/move-line (if (null n) 1 n)))

(defun lvzstrings/indent-selection (level) "Intent selected text or current line."
	   (interactive)
	   (if (use-region-p)
		   (progn
			 (indent-rigidly (region-beginning) (region-end) (* level standard-indent))
			 (setq deactivate-mark nil)
			 )
		 (indent-rigidly (line-beginning-position) (line-end-position) (* level standard-indent)))
	   )

(defun lvzstrings/indent () "Intent one level for selected text or current line."
	   (interactive)
	   (lvzstrings/indent-selection 1))

(defun lvzstrings/unindent () "Unintent one level for selected text or current line."
	   (interactive)
	   (lvzstrings/indent-selection -1))

(defun lvzstrings/onespace () "Function for removing unnecessary spaces on region or to the end of line."
	   (interactive)
	   (let (
			 (begin (point))
			 (end (line-end-position))
			 (str "")
			 )

		 ;; If region is active
		 (if (use-region-p) (progn (setq
									begin (region-beginning)
									end (region-end))))

		 (setq str (replace-regexp-in-string lvzstrings/lvzstrings-onespace-regex " " (buffer-substring-no-properties begin end)))
		 (kill-region begin end)
		 (goto-char begin)
		 (insert str)))

(defun lvzstrings/trim () "Trim space from the begining and ending of current line or selection. "
	   (interactive)
	   (let (
			 (begin (point))    
			 (end (line-end-position))
			 (str "")
			 )

		 ;; If region is active
		 (if (use-region-p) (progn (setq
									begin (region-beginning)
									end (region-end))))

		 (setq str (replace-regexp-in-string "[ ]+$" "" (replace-regexp-in-string "^[ ]+" "" (buffer-substring-no-properties begin end))))
		 (kill-region begin end)
		 (goto-char begin)
		 (insert str)))

(defun lvzstrings/ltrim () "Trim space from the begining of current line or selection. "
	   (interactive)
	   (let (
			 (begin (point))    
			 (end (line-end-position))
			 (str "")
			 )

		 ;; If region is active
		 (if (use-region-p) (progn (setq
									begin (region-beginning)
									end (region-end))))

		 (setq str (replace-regexp-in-string "^[ ]+" "" (buffer-substring-no-properties begin end)))
		 (kill-region begin end)
		 (goto-char begin)
		 (insert str)))

(defun lvzstrings/rtrim () "Trim space from the ending of current line or selection."
	   (interactive)
	   (let (
			 (begin (point))
			 (end (line-end-position))
			 (str "")
			 )

		 ;; If region is active
		 (if (use-region-p) (progn (setq
									begin (region-beginning)
									end (region-end))))

		 (setq str (replace-regexp-in-string "[ ]+$" "" (buffer-substring-no-properties begin end)))
		 (kill-region begin end)
		 (goto-char begin)
		 (insert str)))

(defun lvzstrings/translate-region (start end) "Function to translate region. This function just runs the \"trans\" command."
	   (interactive "r")
	   (let ((cmdstr lvzstrings/translate-command)
			 (options "-show-original n -show-original-phonetics n -show-translation Y -show-translation-phonetics n -show-prompt-message n -show-languages n -show-original-dictionary n -show-dictionary n -show-alternatives n")
			 (inlang "")
			 (outlang "el")
			 (txt (json-encode-string (buffer-substring start end)))
			 )

		 (if (region-active-p) (progn
		 						 (setq inlang (completing-read "Input language: " '("" "en" "el")))
		 						 (setq outlang (completing-read "Output language: " '("en" "el")))
		 						 (kill-region start end)
		 						 (insert (shell-command-to-string (format cmdstr options inlang outlang txt)))
		 						 )
		   (error "There must be a selection"))
		 ))

(defun lvzstrings/urlencode (start end) "Function to url encode region. This function just uses the \"url-hexify-string\" function."
	   (interactive "r")
	   (let ((txt (buffer-substring start end)))
		 (if (region-active-p) (progn
								 (kill-region start end)
								 (insert (url-hexify-string txt))
								 )
		   (error "There must be a selection"))
		 ))

(defun lvzstrings/urldecode (start end) "Function to url decode region. This function just uses the \"url-unhex-string\" function."
	   (interactive "r")
	   (let ((txt (buffer-substring start end)))
		 (if (region-active-p) (progn
								 (kill-region start end)
								 (insert (url-unhex-string txt))
								 )
		   (error "There must be a selection"))
		 ))

(defun lvzstrings/htmlencode (start end) "Function to encode region to HTML entities. This function just calls \"recode\" program."
	   (interactive "r")
	   (let ((txt (buffer-substring start end)))
		 (if (region-active-p) (progn
								 (call-process-region start end "recode" t t nil "..HTML_4.0"))
		   (error "There must be a selection"))
		 ))

(defun lvzstrings/htmldecode (start end) "Function to decode HTML. This function just calls \"recode\" program."
	   (interactive "r")
	   (let ((txt (buffer-substring start end)))
		 (if (region-active-p) (progn
								 (call-process-region start end "recode" t t nil "html..UTF-8"))
		   (error "There must be a selection"))
		 ))

(defun lvzstrings/removehtml (start end) "Function to remove HTML tags from string."
	   ;; xahlee.info/emacs/emacs/emacs_html_delete_tags.html
	   (interactive "r")
	   (let ((txt (buffer-substring start end))
			 (p 0))
		 ;; (looking-at "<")
		 ;; (setq p (point))
		 ;; (search-forward ">")
		 ;; (delete-region p (point))
		 ))

(defun lvzstrings/spellcheck () "Function for spellchecking current buffer."
	   (interactive)
	   (flyspell-buffer))

(defun lvzstrings/spellCheckOntheFlyProg () "Function for enabling spell check."
	   (interactive)
	   (flyspell-mode 1)
	   (flyspell-prog-mode)
	   (ispell-change-dictionary "en_US,el_GR,de_DE")
	   )

(defun lvzstrings/dictionary (start end) "Function for creating new buffer with the definitions of the selected word."
	   (interactive "r")
	   (let ((dictcmd lvzstrings/dictionary-command)
			 (txt (json-encode-string (buffer-substring-no-properties start end)))
			 ($buff nil)
			 (dictionary-keymap nil)
			 )
		 
		 (if (region-active-p) (progn
								 (setq $buff (generate-new-buffer "*Dictionary*")
									   dictionary-keymap (make-sparse-keymap))

								 (switch-to-buffer $buff)
								 (insert (shell-command-to-string (format dictcmd txt)))
								 (font-lock-mode 1)
								 (use-local-map dictionary-keymap)

								 (define-key dictionary-keymap (kbd "q") (lambda()
																		   (interactive)
																		   (kill-buffer)
																		   ))
								 (read-only-mode 1)
								 (goto-char 0)
								 )
		   (error "There must be a selection"))
		 ))

;;---- MINOR MODE ------------------------------------------------------------------

(define-minor-mode lvzstrings-mode "Minor mode for working strings."
  :lighter lvzstrings/lighter
  :keymap (let ((lvzstringsmap (make-sparse-keymap)))

			(if (> emacs-major-version 23) (progn
											 (with-eval-after-load "ispell"

											   (require 'flyspell)
											   (add-hook 'text-mode-hook 'flyspell-mode)
											   (add-hook 'org-mode-hook 'flyspell-mode)

											   (setq ispell-program-name "hunspell")		   ; Φόρτωση του hunspell για έλεγχο ορθογραφίας
											   (setq ispell-dictionary "en_US,el_GR,de_DE") ; Φόρτωση αγγλικών, ελληνικών και γερμανικών λεξικών
											   (setq ispell-local-dictionary "en_US,el_GR,de_DE")
											   (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,el_GR,de_DE") nil utf-8)))
											   (ispell-set-spellchecker-params)
											   (ispell-change-dictionary ispell-local-dictionary)
											   (if (> emacs-major-version 24)			   ; Η συνάρτηση "ispell-hunspell-add-multi-dic" δεν υπάρχει σε παλιότερες εκδόσεις του emacs
												   (ispell-hunspell-add-multi-dic ispell-local-dictionary))

											   (add-hook 'prog-mode-hook 'lvzstrings/spellCheckOntheFlyProg))
											 ))

			;; Menu under Tools
			(define-key-after global-map [menu-bar tools lvzstringstmenu]
			  (cons "LVzStrings" (make-sparse-keymap "major modes")) 'kill-buffer )

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode] ; Submenu Encode/decode
			  (cons "Encode/Decode" (make-sparse-keymap "encodedecode")))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode lvzstringstmenuencodedecodehtml] ; Submenu Encode/decode HTML
			  (cons "HTML" (make-sparse-keymap "encodedecodehtml")))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode lvzstringstmenuencodedecodehtml lvzstringstmenuencodedecodehtmldecode]
			  '("Decode selection" . lvzstrings/htmldecode))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode lvzstringstmenuencodedecodehtml lvzstringstmenuencodedecodehtmlencode]
			  '("Encode selection" . lvzstrings/htmlencode))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode lvzstringstmenuencodedecodeurl] ; Submenu Encode/decode URL
			  (cons "URL" (make-sparse-keymap "encodedecodeurl")))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode lvzstringstmenuencodedecodeurl lvzstringstmenuencodedecodeurldecode]
			  '("Decode selection" . lvzstrings/urldecode))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode lvzstringstmenuencodedecodeurl lvzstringstmenuencodedecodeurlencode]
			  '("Encode selection" . lvzstrings/urlencode))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode lvzstringstmenuencodedecodebase64] ; Submenu Encode/decode BASE64
			  (cons "BASE64" (make-sparse-keymap "encodedecodebase64")))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode lvzstringstmenuencodedecodebase64 lvzstringstmenuencodedecodebase64decode]
			  '("Decode selection" . base64-decode-region))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuencodedecode lvzstringstmenuencodedecodebase64 lvzstringstmenuencodedecodebase64encode]
			  '("Encode selection" . base64-encode-region))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuspacetrimm] ; Submenu space trimming
			  (cons "Remove spaces" (make-sparse-keymap "removespaces")))

			(define-key global-map  [menu-bar tools lvzstringstmenu lvzstringstmenuspacetrimm lvzstringstmenuspacetrimmftrim]
			  '("Remove spaces from ending of selection " . lvzstrings/rtrim))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuspacetrimm lvzstringstmenuspacetrimmltrim]
			  '("Remove spaces from beginning of selection " . lvzstrings/ltrim))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuspacetrimm lvzstringstmenuspacetrimmrtrim]
			  '("Remove spaces from beginning and ending of selection " . lvzstrings/trim))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuspacetrimm lvzstringstmenuspacetrimmrspace]
			  '("Remove unnecessary spaces" . lvzstrings/onespace))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuspacetrimm lvzstringstmenuspacetrimmsep1] '("--"))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenuspacetrimm lvzstringstmenuspacetrimwspmode]
			  '("Enable/disable whitespace mode" . whitespace-mode))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenulinemove] ; Submenu moving lines
			  (cons "Moving lines" (make-sparse-keymap "movinglines")))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenulinemove lvzstringstmenuunindent]
			  '("Unindent line or selected lines" . lvzstrings/unindent))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenulinemove lvzstringstmenuindent]
			  '("Indent line or selected lines" . lvzstrings/indent))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenulinemove tseparator1] '("--"))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenulinemove lvzstringstmenumlinedown]
			  '("Move line Down" . lvzstrings/move-line-down))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstmenulinemove lvzstringstmenumlineup]
			  '("Move line Up" . lvzstrings/move-line-up))

			(define-key global-map [menu-bar tools lvzstringstmenu tseparator0] '("--"))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage]
			  (cons "Language" (make-sparse-keymap "language")))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstflyspell]
			  (cons "Flyspell" (make-sparse-keymap "flyspell")))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstflyspell lvzstringstmenuspellcheck]
			  '("Spellcheck buffer" . lvzstrings/spellcheck))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstflyspell lvzstringstmenuspellcheckregion]
			  '("Spellcheck region" . flyspell-region))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstflyspell lvzstringstmenuspellnexterror]
			  '("Go to next error" . flyspell-goto-next-error))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstflyspell lvzstringstmenuspellcorrect]
			  '("Correct word" . flyspell-correct-word-before-point))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstlanguagetool]
			  (cons "Languagetool" (make-sparse-keymap "languagetool")))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstlanguagetool lvzstringstlanguagetoolcheck]
			  '("Check document" . langtool-check))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstlanguagetool lvzstringstlanguagetoolnexterror]
			  '("Next error" . langtool-goto-next-error))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstlanguagetool lvzstringstlanguagetoolcorrectbuffer]
			  '("Correct buffer" . langtool-correct-buffer))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstlanguagetool lvzstringstlanguagetooldone]
			  '("End process" . langtool-check-done))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstmenutranslate]
			  '("Translate region" . lvzstrings/translate-region))

			(define-key global-map [menu-bar tools lvzstringstmenu lvzstringstlanguage lvzstringstmenudictionary]
			  '("Dictionary" . lvzstrings/dictionary))

			;;;;;;;; Lighter menu
			(define-key-after		 ; Menu for LVzStrings mode
			  lvzstrings/lvzstrings-keymap
			  [menu-bar lvzstringsmenu]
			  (cons "LVzStrings" (make-sparse-keymap "lvzstrings mode"))
			  'kill-buffer)

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode] ; Submenu Encode/decode
			  (cons "Encode/Decode" (make-sparse-keymap "encodedecode")))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode lvzstringsencodedecodehtml] ; Submenu Encode/decode HTML
			  (cons "HTML" (make-sparse-keymap "encodedecodehtml")))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode lvzstringsencodedecodehtml lvzstringsencodedecodehtmldecode]
			  '("Decode selection" . lvzstrings/htmldecode))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode lvzstringsencodedecodehtml lvzstringsencodedecodehtmlencode]
			  '("Encode selection" . lvzstrings/htmlencode))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode lvzstringsencodedecodeurl] ; Submenu Encode/decode URL
			  (cons "URL" (make-sparse-keymap "encodedecodeurl")))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode lvzstringsencodedecodeurl lvzstringsencodedecodeurldecode] ; decode url
			  '("Decode selection" . lvzstrings/urldecode))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode lvzstringsencodedecodeurl lvzstringsencodedecodeurlencode] ; encode url
			  '("Encode selection" . lvzstrings/urlencode))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode lvzstringsencodedecodebase64] ; Submenu Encode/decode BASE64
			  (cons "BASE64" (make-sparse-keymap "encodedecodebase64")))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode lvzstringsencodedecodebase64 lvzstringsencodedecodebase64decode] ; decode base64
			  '("Decode selection" . base64-decode-region))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsencodedecode lvzstringsencodedecodebase64 lvzstringsencodedecodebase64encode] ; encode base64
			  '("Encode selection" . base64-encode-region))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuspacetrimm] ; Submenu space trimming
			  (cons "Remove spaces" (make-sparse-keymap "removespaces")))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuspacetrimm lvzstringsmenuspacetrimmrtrimm] ; rtrim spaces
			  '("Remove spaces from ending of selection (right trim) " . lvzstrings/rtrim))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuspacetrimm lvzstringsmenuspacetrimmltrimm] ; ltrim spaces
			  '("Remove spaces from beginning of selection (left trim) " . lvzstrings/ltrim))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuspacetrimm lvzstringsmenuspacetrimmtrimm] ; trim spaces
			  '("Remove spaces from beginning and ending of selection (full trim) " . lvzstrings/trim))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuspacetrimm lvzstringsmenuspacetrimmremovespaces] ; remove spaces
			  '("Remove unnecessary spaces" . lvzstrings/onespace))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuspacetrimm lvzstringsmenuspacetrimmseparator1] '("--"))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuspacetrimm lvzstringsmenuspacetrimmwhispacemode] ;enable/disable whitespace mode
			  '("Enable/disable whitespace mode" . whitespace-mode))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenulinemove] ; Submenu moving lines
			  (cons "Moving lines" (make-sparse-keymap "movinglines")))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenulinemove lvzstringsmenulinemoveunindent] ; unindent
			  '("Unindent line or selected lines" . lvzstrings/unindent))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenulinemove lvzstringsmenulinemoveindent] ; indent
			  '("Indent line or selected lines" . lvzstrings/indent))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenulinemove lvzstringsmenulinemoveseparator1] '("--"))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenulinemove lvzstringsmenulinemovelinedown] ; move line down
			  '("Move line Down" . lvzstrings/move-line-down))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenulinemove lvzstringsmenulinemovelineup] ; move line up
			  '("Move line Up" . lvzstrings/move-line-up))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage] ; Submenu for tools about language
			  (cons "Language" (make-sparse-keymap "language")))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringsflyspell] ; Submenu for the spell checker
			  (cons "Flyspell" (make-sparse-keymap "flyspell")))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringsflyspell lvzstringsmenuspellcheck]  ; Spellcheck current buffer
			  '("Spellcheck buffer" . lvzstrings/spellcheck))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringsflyspell lvzstringsmenuspellcheckregion]  ; Spellcheck active region
			  '("Spellcheck region" . flyspell-region))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringsflyspell lvzstringsmenuspellnexterror]  ; Spellcheck goto next error
			  '("Go to next error" . flyspell-goto-next-error))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringsflyspell lvzstringsmenuspellcorrect]  ; Spellcheck correct word
			  '("Correct word" . flyspell-correct-word-before-point))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringslanguagetool]
			  (cons "Languagetool" (make-sparse-keymap "languagetool")))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringslanguagetool lvzstringslanguagetoolcheck]
			  '("Check document" . langtool-check))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringslanguagetool lvzstringslanguagetoolnexterror]
			  '("Next error" . langtool-goto-next-error))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringslanguagetool lvzstringslanguagetoolcorrectbuffer]
			  '("Correct buffer" . langtool-correct-buffer))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringslanguagetool lvzstringslanguagetooldone]
			  '("End process" . langtool-check-done))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringsmenutranslate] ; Translate selected text
			  '("Translate region" . lvzstrings/translate-region))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringslanguage lvzstringsmenudictionary] ; Dictionary
			  '("Dictionary" . lvzstrings/dictionary))

			;; (global-unset-key (kbd lvzstrings/lvzstrings-moveup-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-moveup-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-indent-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-unindent-keycomb))

			;; (global-unset-key (kbd lvzstrings/lvzstrings-remove-spaces-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-base64-encode-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-base64-decode-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-url-encode-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-url-decode-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-trim-spaces-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-ltrim-spaces-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-rtrim-spaces-keycomb))
			;; (global-unset-key (kbd lvzstrings/lvzstrings-whitespace-keycomb))

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-remove-spaces-keycomb) 'lvzstrings/onespace)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-base64-encode-keycomb) 'base64-encode-region)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-base64-decode-keycomb) 'base64-decode-region)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-url-encode-keycomb) 'lvzstrings/urlencode)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-url-decode-keycomb) 'lvzstrings/urldecode)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-html-encode-keycomb) 'lvzstrings/htmlencode)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-html-decode-keycomb) 'lvzstrings/htmldecode)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-trim-spaces-keycomb) 'lvzstrings/trim)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-ltrim-spaces-keycomb) 'lvzstrings/ltrim)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-rtrim-spaces-keycomb) 'lvzstrings/rtrim)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-whitespace-keycomb) 'whitespace-mode)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-moveup-keycomb) 'lvzstrings/move-line-up)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-movedown-keycomb) 'lvzstrings/move-line-down)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-indent-keycomb) 'lvzstrings/indent)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-unindent-keycomb) 'lvzstrings/unindent)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-translate-keycomb) 'lvzstrings/translate-region)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-dictionary-keycomb) 'lvzstrings/dictionary)

			(require 'langtool)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-langtool-check-keycomb) 'langtool-check)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-langtool-goto-next-error-keycomb) 'langtool-goto-next-error)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-langtool-correct-buffer-keycomb) 'langtool-correct-buffer)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-langtool-check-done-keycomb) 'langtool-check-done)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-spellcheck-keycomb) 'lvzstrings/spellcheck)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-spellcheck-region-keycomb) 'flyspell-region)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-spellcheck-goto-next-error-keycomb) 'flyspell-goto-next-error)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-spellcheck-correct-word-keycomb) 'flyspell-correct-word-before-point)

			lvzstrings/lvzstrings-keymap)
  :global 1

  (make-local-variable 'lvzstrings/lvzstrings-keymap)
  )

(lvzstrings-mode 1)

(provide 'lvzstrings-mode)

;; (url-encode-url "https://google.com/this   is a url")
;; (url-hexify-string
;; (url-unhex-string (url-encode-url "https://google.com/this   is a url"))


;; w3m-url-decode-string
;; Test here. Use ~M-x whitespace-mode~ to see
;; this is 		a    string
;; this is  a    string   


;; TEST HTML ENTITIES:
;; ENCODE: <>&&&

