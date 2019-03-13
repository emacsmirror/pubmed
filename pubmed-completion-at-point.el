;;; pubmed-competion-at-point.el --- Completion-at-point-functions for PubMed autocompletion  -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@xs4all.nl>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a for autocompleting using PubMed suggestions.

;; Documentation:

;; Emacs provides context aware completion via the complete-symbol command, bound to C-M-i by default. In order for it to do something useful, completion-at-point-functions has to be set up. The API completion at point function can be found in the documentation of `completion-at-point-functions'. The completion functions look for word at point (the library thingatpt is used to find the bounds of word) and completes it using PubMed suggestions.

;; Integration with company-mode

;; Company mode integrates very well with completion-at-point-functions using the company-capf backend, so it should work out of the box for you, but you can enhance the completions offered by company by returning additional props in the result of capf function. The props currently supported are:
;; :company-doc-buffer - Used by company to display metadata for current candidate
;; :company-docsig - Used by company to echo metadata about the candidate in the minibuffer
;; :company-location - Used by company to jump to the location of current candidate

;;; Code:

;;;; Requirements

(require 's)
(require 'url)

;;;; Variables

(defvar pubmed-completion-url
  "https://www.ncbi.nlm.nih.gov/portal/utils/autocomp.fcgi?dict=pm_related_queries_2&callback=?&q=%s")

(defvar pubmed-author-completion-url
  "https://www.ncbi.nlm.nih.gov/portal/utils/autocomp.fcgi?dict=auf&q=%s")

(defvar pubmed-journal-completion-url
  "https://www.ncbi.nlm.nih.gov/portal/utils/autocomp.fcgi?dict=jrz&q=%s")

;;;;; Commands

(defun pubmed-complete (&optional _predicate)
  "Perform completion using PubMed suggestions preceding point."
  (interactive)
  (let* ((data (pubmed-completion-at-point))
         (plist (nthcdr 3 data)))
    (if (null data)
        (minibuffer-message "Nothing to complete")
      (let ((completion-extra-properties plist))
        (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)
                              (plist-get plist :predicate))))))

(defun pubmed-author-complete (&optional _predicate)
  "Perform completion using PubMed suggestions preceding point."
  (interactive)
  (let* ((data (pubmed-author-completion-at-point))
         (plist (nthcdr 3 data)))
    (if (null data)
        (minibuffer-message "Nothing to complete")
      (let ((completion-extra-properties plist))
	(completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)
                              (plist-get plist :predicate))))))

(defun pubmed-journal-complete (&optional _predicate)
  "Perform completion using PubMed suggestions preceding point."
  (interactive)
  (let* ((data (pubmed-journal-completion-at-point))
         (plist (nthcdr 3 data)))
    (if (null data)
        (minibuffer-message "Nothing to complete")
      (let ((completion-extra-properties plist))
        (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)
                              (plist-get plist :predicate))))))

(defun pubmed-completion-at-point ()
  "Function used for `completion-at-point-functions'"
  ;; FIXME: integration with company-mode
  (interactive)
  (let* ((start (line-beginning-position))
	 (end (line-end-position))
	 (text (buffer-substring-no-properties start end)))
    (list start
	  end
	  (completion-table-with-cache
           (lambda (_)
	     (pubmed--completion-candidates text)))
	  :exclusive 'no)))

(defun pubmed-author-completion-at-point ()
  "Function used for `completion-at-point-functions'"
  ;; FIXME: integration with company-mode
  (interactive)
  (let* ((start (line-beginning-position))
	 (end (line-end-position))
	 (text (buffer-substring-no-properties start end)))
    (list start
	  end
	  (completion-table-with-cache
           (lambda (_)
	     (pubmed--author-completion-candidates text)))
	  :exclusive 'no)))

(defun pubmed-journal-completion-at-point ()
  "Function used for `completion-at-point-functions'"
  ;; FIXME: integration with company-mode
  (interactive)
  (let* ((start (line-beginning-position))
	 (end (line-end-position))
	 (text (buffer-substring-no-properties start end)))
    (list start
	  end
	  (completion-table-with-cache
           (lambda (_)
	     (pubmed--journal-completion-candidates text)))
	  :exclusive 'no)))

;;;; Functions

(defun pubmed--sentence-at-point ()
  "Return the sentence at point."
  (interactive)
  (let* ((current-line (line-number-at-pos))
	 sentence-line
	 (sentence
	  (save-excursion
	    (backward-sentence 1)
	    (setq sentence-line (line-number-at-pos))
	    ;; don't span prefix over following lines
	    (when (thing-at-point 'sentence)
	      (replace-regexp-in-string
	       ".*?\\([[:alnum:]][[:space:][:alnum:]]*\\)"
	       "\\1"
	       (replace-regexp-in-string "\\(.*\\)[ \t\n]*.*" "\\1" (thing-at-point 'sentence)))))))
    (or (if (eq sentence-line current-line) sentence) (thing-at-point 'word)))) ;fallback to word

(defun pubmed--completion-candidates (string)
  "Return a list of NCBI autocomplete suggestions matching STRING."
  (let* ((url (format pubmed-completion-url (url-hexify-string string)))
	 (response (with-temp-buffer (url-insert-file-contents url)(buffer-string)))
	 ;; Return the positions of the words delimited by double quotes
	 (matched-positions (s-matched-positions-all "\"\\([[:word:][:ascii:][:space:]]+?\\)\"" response 1))
	 ;; Return a list of words
	 (matched-words (mapcar (lambda (x) (substring response (car x) (cdr x))) matched-positions))
	 ;; Remove the prefix and keep the autocomplete suggestions
	 (candidates (cdr matched-words)))
    candidates))

(defun pubmed--author-completion-candidates (string)
  "Return a list of NCBI autocomplete suggestions matching STRING."
  (let* ((url (format pubmed-author-completion-url (url-hexify-string string)))
	 (response (with-temp-buffer (url-insert-file-contents url)(buffer-string)))
	 ;; Return the positions of the words delimited by double quotes
	 (matched-positions (s-matched-positions-all "\"\\([[:word:][:ascii:][:space:]]+?\\)\"" response 1))
	 ;; Return a list of matched items, containing matched authors bounded by an @-sign.
	 (matched-items (mapcar (lambda (x) (substring response (car x) (cdr x))) matched-positions))
	 ;; Return a list of matched authors
	 (matched-authors (mapcan (lambda (x) (s-split "@" x)) matched-items))
	 ;; Remove the prefix and keep the autocomplete suggestions
	 (candidates (cdr matched-authors)))
    candidates))

(defun pubmed--journal-completion-candidates (string)
  "Return a list of NCBI autocomplete suggestions matching STRING."
  (let* ((url (format pubmed-journal-completion-url (url-hexify-string string)))
	 (response (with-temp-buffer (url-insert-file-contents url)(buffer-string)))
	 ;; Return the positions of the words delimited by double quotes
	 (matched-positions (s-matched-positions-all "\"\\([[:word:][:ascii:][:space:]]+?\\)\"" response 1))
	 ;; Return a list of matched items, containing matched journals bounded by an @-sign.
	 (matched-items (mapcar (lambda (x) (substring response (car x) (cdr x))) matched-positions))
	 ;; Return a list of matched journals
	 (matched-journals (mapcan (lambda (x) (s-split "@" x)) matched-items))
	 ;; Remove the prefix and keep the autocomplete suggestions
	 (candidates (cdr matched-journals)))
    candidates))

;;;; Footer

(provide 'pubmed-completion-at-point)

;;; pubmed-completion-at-point.el ends here
