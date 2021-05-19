;;; lvzstrings-mode.el --- Minor mode for working with strings on emacs.
;;; -*- coding: utf-8 -*-

;; Copyright © 2019, Nikos Skarmoutsos

;; Version: VERSION
;; Author: Nikos Skarmoutsos <elvizakos AT yahoo DOT gr>
;; Maintainer: Nikos Skarmoutsos
;; Created: Dec 2019
;; Keywords: string
;; License: GNU General Public License >=2
;; Distribution: This file is not part of Emacs

;;; Code:

;;---- CONSTANTS ------------------------------------------------------------------
(defconst lvzstrings/lvzstrings-version "VERSION" "LVzStrings version.")

;;---- VARIABLES ------------------------------------------------------------------
(defvar lvzstrings/lvzstrings-keymap (make-sparse-keymap) "Keymap for lvzstrings.")

;;---- OPTIONS --------------------------------------------------------------------
(defgroup lvzstrings nil "LVzStrings minor mode settings."
  :group 'tools)

(defcustom lvzstrings/lighter " LVzA-Z" "Label of minor mode for the modeline."
  :type 'string
  :group 'lvzstrings)

(defgroup lvzstrings/lvzstrings-keys nil "LVzStrings minor mode key settings."
  :group 'tools)

(defcustom lvzstrings/lvzstrings-moveup-keycomb "M-s-<up>" "Default key combination for moving text up one line."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-movedown-keycomb "M-s-<down>" "Default key combination for moving text down one line."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-indent-keycomb "M-s-<right>" "Default key combination for indenting current line or selected lines."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-unindent-keycomb "M-s-<left>" "Default key combination for unindenting curret line or selected lines."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-remove-spaces-keycomb "C-c C-v SPC" "Default key combination for removing unnecessary spaces."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-trim-spaces-keycomb "C-c C-t t" "Default key combination for removing beginning and ending spaces from selection."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-ltrim-spaces-keycomb "C-c C-t l" "Default key combination for removing beginning spaces from selection."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-rtrim-spaces-keycomb "C-c C-t r" "Default key combination for removing ending spaces from selection."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-base64-encode-keycomb "C-c C-b e" "Default key combination for encoding selected string to base64."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-base64-decode-keycomb "C-c C-b d" "Default key combination for decoding base64 encoded selected string."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-url-encode-keycomb "C-c C-u e" "Default key combination for encoding selected string for use in a url."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-url-decode-keycomb "C-c C-u d" "Default key combination for decoding selected url encoded string."
  :type 'string
  :group 'lvzstrings/lvzstrings-keys)

(defcustom lvzstrings/lvzstrings-whitespace-keycomb "C-c C-v C-SPC" "Default key combination for enabling/disabling whitespace mode."
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

;;---- MINOR MODE ------------------------------------------------------------------

(define-minor-mode lvzstrings-mode "Minor mode for working strings."
  :lighter lvzstrings/lighter
  :keymap (let ((lvzstringsmap (make-sparse-keymap)))

			(define-key-after		 ; Menu for LVzStrings mode
			  lvzstrings/lvzstrings-keymap
			  [menu-bar lvzstringsmenu]
			  (cons "LVzStrings" (make-sparse-keymap "lvzstrings mode"))
			  'kill-buffer)

			;; (define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenudecodeurl] ; decode url
			;;   '("Decode URL selection" . url-decode-selection))

			;; (define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuecodeurl] ; encode url
			;;   '("Encode selection for use in URL" . url-encode-selection))

			;; (define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu separator4] '("--"))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenudecodeb64] ; decode base64
			  '("Decode a Base64 selection" . base64-decode-region))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuencodeb64] ; encode base64
			  '("Encode selection to Base64" . base64-encode-region))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu separator3] '("--"))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenurtrim] ; rtrim spaces
			  '("Remove spaces from ending of selection " . lvzstrings/rtrim))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenultrim] ; ltrim spaces
			  '("Remove spaces from beginning of selection " . lvzstrings/ltrim))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenutrim] ; trim spaces
			  '("Remove spaces from beginning and ending of selection " . lvzstrings/trim))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuremovespaces] ; remove spaces
			  '("Remove unnecessary spaces" . lvzstrings/onespace))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu separator2] '("--"))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuwhispacemode] ;enable/disable whitespace mode
			  '("Enable/disable whitespace mode" . whitespace-mode))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu separator1] '("--"))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuunindent] ; unindent
			  '("Unindent line or selected lines" . lvzstrings/unindent))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenuindent] ; indent
			  '("Indent line or selected lines" . lvzstrings/indent))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu separator0] '("--"))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenulinedown] ; move line down
			  '("Move line Down" . lvzstrings/move-line-down))

			(define-key lvzstrings/lvzstrings-keymap [menu-bar lvzstringsmenu lvzstringsmenulineup] ; move line up
			  '("Move line Up" . lvzstrings/move-line-up))

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

			;; (define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-url-encode-keycomb) 'url-encode-selection)
			;; (define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-url-decode-keycomb) 'url-decode-selection)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-trim-spaces-keycomb) 'lvzstrings/trim)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-ltrim-spaces-keycomb) 'lvzstrings/ltrim)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-rtrim-spaces-keycomb) 'lvzstrings/rtrim)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-whitespace-keycomb) 'whitespace-mode)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-moveup-keycomb) 'lvzstrings/move-line-up)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-movedown-keycomb) 'lvzstrings/move-line-down)

			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-indent-keycomb) 'lvzstrings/indent)
			(define-key lvzstrings/lvzstrings-keymap (kbd lvzstrings/lvzstrings-unindent-keycomb) 'lvzstrings/unindent)

			lvzstrings/lvzstrings-keymap)
  :global 1

  (make-local-variable 'lvzstrings/lvzstrings-keymap)
  )

(lvzstrings-mode 1)

(provide 'lvzstrings-mode)

;; Test here. Use ~M-x whitespace-mode~ to see
;; this is 		a    string
;; this is  a    string   
