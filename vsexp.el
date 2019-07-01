;;; vsexp.el -- Marking, Killing and Kill-Saving contents of a sexp
;;; Commentary:
;;; Several very simple functions for easier manipulation
;;; of /sexp/s.  I know that this functionality more or less exists
;;; with the existing package smartparens but I found myself needing
;;; the implemented here behavior more and more in my daily
;;; programming life.  Suggested (and the prefered by me) keybinding
;;; can be found in the git repository


;; Copyright (C) 2019 Stanislav Arnaudov

;; Author: Stanislav Arnaudov Keywords: Lisp, sexp, smartparens
;; Version: 1.1
;; Package-Requires: ((smartparens))
;; Keywords: sexp, kill, mark, vim
;; URL: https://github.com/palikar/arnaud-sexp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Code:
(require 'smartparens)


(defun vsexp-mark-sexp ()
	"Mark the content inside of the current sexp."
	(interactive)
	(if (use-region-p)
			(progn
				(sp-backward-up-sexp)
				(sp-backward-up-sexp)
				(forward-char 1)
				(set-mark-command nil)
				(sp-up-sexp)
				(backward-char 1))
		(progn
			(sp-backward-up-sexp)
			(forward-char 1)
			(set-mark-command nil)
			(sp-up-sexp)
			(backward-char 1))))

(defun vsexp-mark-sexp-whole ()
	"Mark the things inside of the current sexp."
	(interactive)
	(sp-backward-up-sexp)
	(set-mark-command nil)
	(forward-sexp))

(defun vsexp-kill-save-sexp ()
	"Kill the things inside of the current sexp."
	(interactive)
	(sp-backward-up-sexp)
	(forward-char 1)
	(set-mark-command nil)
	(sp-up-sexp)
	(backward-char 1)
	(kill-ring-save (region-beginning) (region-end))
	(deactivate-mark))

(defun vsexp-kill-save-sexp-whole ()
	"Kill the current sexp."
	(interactive)
	(sp-backward-up-sexp)
	(set-mark-command nil)
	(sp-up-sexp)
	(kill-ring-save (region-beginning) (region-end))
	(deactivate-mark))

(defun vsexp-kill-sexp ()
	"Kill the things inside of the current sexp."
	(interactive)
	(sp-backward-up-sexp)
	(forward-char 1)
	(set-mark-command nil)
	(sp-up-sexp)
	(backward-char 1)
	(kill-region (region-beginning) (region-end)))

(defun vsexp-kill-sexp-whole ()
	"Mark the things inside of the current sexp."
	(interactive)
	(sp-backward-up-sexp)
	(set-mark-command nil)
	(forward-sexp)
	(kill-region (region-beginning) (region-end)))


(setq bracket-map '(("[" . "]") ("{" . "}")))

(length bracket-map)

(let (value)
	(dolist (braces bracket-map)
		(message (car braces))
		))




(defun xah-get-matching-bracket (@bracket-char-string)
	"Find the brackets."
  (interactive)
  (let (($syntableValue (aref (syntax-table) (string-to-char @bracket-char-string))))
    (if (or (eq (car $syntableValue ) 4)
						(eq (car $syntableValue ) 5))
        (char-to-string (cdr $syntableValue))
			(progn
				(dolist (braces bracket-map)
					(cond
					 ((eq (car baces) @bracket-char-string) (cdr baces))
					 ((eq (cdr baces) @bracket-char-string) (car baces)))
					)
				))))




(xah-get-matching-bracket "}")

(defun vsexp-mark-inside ()
	"Mark inside the thing."
	(interactive)
	(let ((char (read-key "Mark inside:")))
		
		
		(search-backward (char-to-string char) nil nil 1)
		))

(provide 'vsexp-sexp)
;;; vsexp.el ends here
