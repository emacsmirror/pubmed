;;; pubmed-history.el --- PubMed History  -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@xs4all.nl>
;; Created: 2018-05-23
;; Version: 0.1
;; Keywords: pubmed, hypermedia
;; Package-Requires: ((emacs "25.1") (s "1.12.0"))
;; URL: https://gitlab.com/fvdbeek/emacs-pubmed

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a GNU Emacs interface to the PubMed database of references on life
;; sciences and biomedical topics. Use the PubMed Advanced search Builder to
;; search for terms in a specific search field, browse the index of terms,
;; combine searches using history and preview the number of search results. See
;; <https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.Advanced_Search> for
;; instructions.

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'json)
(require 's)

(defvar pubmed-history nil
  "The Pubmed history.")

(defvar pubmed-history-counter 0)

(defvar pubmed-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "s") #'pubmed-search)
    map)
  "Local keymap for `pubmed-history-mode'.")

;;;; Mode

;;;###autoload
(define-derived-mode pubmed-history-mode tabulated-list-mode "pubmed-history"
  "Major mode for PubMed History."
  :group 'pubmed
  (setq tabulated-list-format [("#" 3 t)
			       ("AND" 3 t)
			       ("OR" 3 t)
			       ("NOT" 3 t)
			       ("Del" 3 t)
			       ("Query" 75 nil)
			       ("Items found" 11 nil)
			       ("Time" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

;;;;; Commands

;;;###autoload
(defun pubmed-history-show ()
  "Populate the tabulated list mode buffer."
  (interactive)
  (let ((pubmed-history-buffer (get-buffer-create "*PubMed History*")))
    (with-current-buffer pubmed-history-buffer
      (pubmed-history-mode)
      (setq tabulated-list-entries pubmed-history)
      (tabulated-list-print t))
    (save-selected-window
      (display-buffer pubmed-history-buffer))))

;;;###autoload
(defun pubmed-history-add (query)
  "Add QUERY to PUBMED-HISTORY."
  (interactive)
  (setq pubmed-history-counter (1+ pubmed-history-counter))
  (let* ((result (pubmed-history--get-count query))
	 (count (plist-get result :count))
	 (querykey (plist-get result :querykey))
	 (webenv (plist-get result :webenv))
	 (time (format-time-string "%H:%M:%S"))
	 (entry (list
		 pubmed-history-counter
		 (vector
		  (concat "#" querykey)
		  (cons "AND" `(help-echo "AND in builder" action (lambda (button)
								    (progn
								      (with-current-buffer "*PubMed Advanced Search Builder*"
									(pubmed-advanced-search-build-search))
								      (pubmed-advanced-search-add-to-builder ,query "AND")))))
		  (cons "OR" `(help-echo "OR in builder" action (lambda (button)
								  (progn
								    (with-current-buffer "*PubMed Advanced Search Builder*"
								      (pubmed-advanced-search-build-search))
								    (pubmed-advanced-search-add-to-builder ,query "OR")))))
		  (cons "NOT" `(help-echo "NOT in builder" action (lambda (button)
								    (progn
								      (with-current-buffer "*PubMed Advanced Search Builder*"
									(pubmed-advanced-search-build-search))
								      (pubmed-advanced-search-add-to-builder ,query "NOT")))))
		  (cons "Del" `(help-echo "Delete from history" action
					  (lambda (button)
					    (when (y-or-n-p (concat "Are you sure you want to delete query #" ,querykey "?"))
					      (setq pubmed-history (delq (assoc (tabulated-list-get-id) pubmed-history) pubmed-history))
					      (pubmed-history-show)))))
		  query
		  (cons count `(help-echo "Show search results" action (lambda (button)
									 (funcall #'pubmed--get-docsums ,querykey ,webenv (string-to-number ,count)))))
		  time))))
    (push entry pubmed-history)
    (pubmed-history-show)))

;;;###autoload
(defun pubmed-history-clear ()
  "Clear PUBMED-HISTORY."
  (interactive)
  (setq pubmed-history-counter 0
	pubmed-history nil)
  (pubmed-history-show))

;;;; Functions

(defun pubmed-history--get-count (query)
  "Search PubMed with QUERY. Use ESearch to post the UIDs on the Entrez History server. Return a plist with the count, querykey and webenv."
  (interactive)
  (let* ((hexified-query (url-hexify-string query)) ;  All special characters are URL encoded.
	 (encoded-query (s-replace "%20" "+" hexified-query)) ; All (hexified) spaces are replaced by '+' signs
	 (url-request-method "POST")
	 (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data (concat "db=" pubmed-db
				   "&retmode=" pubmed-retmode
				   "&term=" encoded-query
				   "&usehistory=" pubmed-usehistory
 				   "&webenv=" pubmed-webenv
				   (when (boundp 'pubmed-api_key)
				     (concat "&api_key=" pubmed-api_key))))
	 (json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type nil)
	 (json (with-current-buffer (url-retrieve-synchronously pubmed-esearch-url) (json-read-from-string (buffer-substring (1+ url-http-end-of-headers) (point-max)))))
	 (esearchresult (plist-get json :esearchresult))
	 (count (plist-get esearchresult :count))
	 (querykey (plist-get esearchresult :querykey))
	 (webenv (plist-get esearchresult :webenv)))
    (setq pubmed-webenv webenv)
    (list :count count :querykey querykey :webenv webenv)))

;;;; Footer

(provide 'pubmed-history)

;;; pubmed-history.el ends here
