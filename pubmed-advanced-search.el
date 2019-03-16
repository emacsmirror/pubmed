;;; pubmed-advanced-search.el --- PubMed Advanced search Builder -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>

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
;; sciences and biomedical topics. Use the PubMed Advanced Search Builder to
;; search for terms in a specific search field, browse the index of terms,
;; combine searches using history and preview the number of search results. See
;; <https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.Advanced_Search> for
;; instructions.

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'json)
(require 's)
(require 'widget)

;;;; Variables

(defvar pubmed-advanced-search-query ""
  "The PubMed search query.")

(defvar pubmed-advanced-search-lookup-table nil
  "A table for lookup markers created in current buffer.")

(defvar pubmed-advanced-search-history nil
  "The Pubmed history.")

(defvar pubmed-advanced-search-history-counter 0)

(defvar pubmed-advanced-search-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "s") #'pubmed-search)
    map)
  "Local keymap for `pubmed-advanced-search-history-mode'.")

;;;; Mode

;;;###autoload
(define-derived-mode pubmed-advanced-search-history-mode tabulated-list-mode "pubmed-advanced-search-history"
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

;;;; Commands

;;;###autoload
(defun pubmed-advanced-search ()
  "Show PubMed Advanced Search Builder."
  (interactive)
  (switch-to-buffer "*PubMed Advanced Search Builder*")
  (kill-all-local-variables)
  (make-local-variable 'pubmed-search-builder)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "PubMed Advanced Search Builder\n\n")

  (pubmed-advanced-search--widget-add 'searchbox
				      (widget-create 'editable-field
						     "Use the builder below to create your search"))
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (pubmed-advanced-search))
		 "Clear")
  
  (widget-insert "\n\n")

  (widget-insert "Builder\n\n")
  
  (setq pubmed-search-builder
	(pubmed-advanced-search--widget-add 'search-builder
					    (widget-create 'editable-list
  							   ;; for each entry in the list, insert the [INS] button, the [DEL] button and the "group" widget.
							   :entry-format "%i %d %v"
							   ;; start with two entries with default values
							   :value  '(("AND" "" ""))
							   ;; custom :list-length property with the length of the list as argument
							   :list-length 2
							   ;; update the :list-length property when the "editable-list" is changed
							   :notify (lambda (widget &rest ignore)
								     (pubmed-advanced-search--build-search))
							   '(group
		       					     (menu-choice
		 					      :format "%[%v%]"
							      :value "AND"
							      :help-echo "Select Boolean operator"
							      :notify (lambda (widget &rest ignore)
									(pubmed-advanced-search--build-search))
							      (item "AND")
							      (item "OR")
							      (item "NOT"))
							     (menu-choice
							      :format "%[%v%]"
							      :tag "All Fields"
							      :value ""
							      :help-echo "Select Field"
							      :notify (lambda (widget &rest ignore)
					     				(pubmed-advanced-search--build-search)
									(let ((value (widget-get widget :value))
									      (sibling (pubmed-advanced-search--widget-get-sibling widget 'editable-field)))
									  (cond
									   ((equal value "")
									    (widget-put sibling :complete-function #'pubmed-complete))
									   ((string-prefix-p "[Author" value)
									    (widget-put sibling :complete-function #'pubmed-author-complete))
									   ((equal value "[Journal]")
									    (widget-put sibling :complete-function #'pubmed-journal-complete))
									   (t
									    (widget-put sibling :complete-function nil)))
									  (widget-setup)))
    							      (item :tag "Affiliation" :value "[Affiliation]")
  							      (item :tag "All Fields" :value "")
  							      (item :tag "Author" :value "[Author]")
  							      (item :tag "Author - Corporate" :value "[Author - Corporate]")
  							      (item :tag "Author - First" :value "[Author - First]")
  							      (item :tag "Author - Full" :value "[Author - Full]")
  							      (item :tag "Author - Identifier" :value "[Author - Identifier]")
  							      (item :tag "Author - Last" :value "[Author - Last]")
  							      (item :tag "Book" :value "[Book]")
  							      (item :tag "Conflict of Interest Statements" :value "[Conflict of Interest Statements]")
  							      (item :tag "Date - Completion" :value "[Date - Completion]")
  							      (item :tag "Date - Create" :value "[Date - Create]")
  							      (item :tag "Date - Entrez" :value "[Date - Entrez]")
  							      (item :tag "Date - MeSH" :value "[Date - MeSH]")
  							      (item :tag "Date - Modification" :value "[Date - Modification]")
  							      (item :tag "Date - Publication" :value "[Date - Publication]")
  							      (item :tag "EC/RN Number" :value "[EC/RN Number]")
  							      (item :tag "Editor" :value "[Editor]")
  							      (item :tag "Filter" :value "[Filter]")
  							      (item :tag "Grant Number" :value "[Grant Number]")
  							      (item :tag "ISBN" :value "[ISBN]")
  							      (item :tag "Investigator" :value "[Investigator]")
  							      (item :tag "Investigator - Full" :value "[Investigator - Full]")
  							      (item :tag "Issue" :value "[Issue]")
  							      (item :tag "Journal" :value "[Journal]")
  							      (item :tag "Language" :value "[Language]")
  							      (item :tag "Location ID" :value "[Location ID]")
  							      (item :tag "MeSH Major Topic" :value "[MeSH Major Topic]")
  							      (item :tag "MeSH Subheading" :value "[MeSH Subheading]")
  							      (item :tag "MeSH Terms" :value "[MeSH Terms]")
  							      (item :tag "Other Term" :value "[Other Term]")
  							      (item :tag "Pagination" :value "[Pagination]")
  							      (item :tag "Pharmacological Action" :value "[Pharmacological Action]")
  							      (item :tag "Publication Type" :value "[Publication Type]")
  							      (item :tag "Publisher" :value "[Publisher]")
  							      (item :tag "Secondary Source ID" :value "[Secondary Source ID]")
  							      (item :tag "Subject - Personal Name" :value "[Subject - Personal Name]")
  							      (item :tag "Supplementary Concept" :value "[Supplementary Concept]")
  							      (item :tag "Text Word" :value "[Text Word]")
  							      (item :tag "Title" :value "[Title]")
  							      (item :tag "Title/Abstract" :value "[Title/Abstract]")
  							      (item :tag "Transliterated Title" :value "[Transliterated Title]")
  							      (item :tag "Volume" :value "[Volume]"))
							     (editable-field
							      :action (lambda (widget &rest ignore)
									;; Insert a new group when <RET> is hit
									(widget-apply (pubmed-advanced-search--widget-get 'search-builder) :insert-before (widget-get (pubmed-advanced-search--widget-get 'search-builder) :widget))
									(widget-move 5))
							      :notify (lambda (widget &rest ignore)
									(pubmed-advanced-search--build-search))
							      :complete-function pubmed-complete
							      "")))))
  
  (widget-insert "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (pubmed-search pubmed-advanced-search-query)
			   (pubmed-advanced-search))
		 "Search")
  (widget-insert " or ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (pubmed-advanced-search--history-add pubmed-advanced-search-query)
			   (pubmed-advanced-search))
		 "Add to history")
  (widget-insert "\n\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (pubmed-advanced-search--history-show))
		 "Show history")
  (widget-insert " or ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (when (y-or-n-p "Your entire search history is about to be cleared. Are you sure? ")
			     (pubmed-advanced-search-history-clear)
			     (pubmed-advanced-search)))
		 "Clear history")

  (use-local-map widget-keymap)
  
  (widget-setup)
  (pubmed-advanced-search--widget-set-point))



;;;; Functions

(defun pubmed-advanced-search--widget-create (id widget)
  "Store ID and WIDGET in `pubmed-advanced-search-lookup-table'."
  (if (assoc id pubmed-advanced-search-lookup-table)
      (error "Identifier %S is used!" id)
    (push (cons id widget) pubmed-advanced-search-lookup-table)))

(defun pubmed-advanced-search--widget-add (id widget)
  "Store ID and WIDGET in `pubmed-advanced-search-lookup-table', and replace ID if it is in use already."
  (let ((old (assoc id pubmed-advanced-search-lookup-table)))
    (if old
        (setcdr old widget)
      (push (cons id widget) pubmed-advanced-search-lookup-table))))

(defun pubmed-advanced-search--widget-get (id)
  "Look up the widget by ID in `pubmed-advanced-search-lookup-table'."
  (cdr (assoc id pubmed-advanced-search-lookup-table)))

(defun pubmed-advanced-search--widget-get-sibling (widget widget-type)
  "Get the sibling of WIDGET with type WIDGET-TYPE."
  (let ((siblings (widget-get (widget-get widget :parent) :children))
	match)
    (dolist (sibling siblings match)
      (when (eq (widget-type sibling) widget-type)
	(setq match sibling)))))

(defun pubmed-advanced-search--widget-change-text (widget value)
  "Set editable text field WIDGET to VALUE."
  (save-excursion
    (goto-char (widget-field-start widget))
    (delete-region (point) (widget-field-end widget))
    (insert value)))

(defun pubmed-advanced-search--widget-set-point ()
  "Set point to the last editable-field widget of the \"search-builder\"."
  (goto-char (point-max))
  (widget-move -5))

(defun pubmed-advanced-search-history--get-count (query)
  "Search PubMed with QUERY. Use ESearch to post the UIDs on the Entrez History server. Return a plist with the count, querykey and webenv."
  (let* ((hexified-query (url-hexify-string query)) ;  All special characters are URL encoded.
	 (encoded-query (s-replace "%20" "+" hexified-query)) ; All (hexified) spaces are replaced by '+' signs
	 (url-request-method "POST")
	 (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data (concat "db=pubmed"
				   "&retmode=json"
				   "&term=" encoded-query
				   "&usehistory=y"
				   (when (not (string-empty-p pubmed-webenv))
				     (concat "&webenv=" pubmed-webenv))
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

(defun pubmed-advanced-search--build-search ()
  "Build the search and return PUBMED-ADVANCED-SEARCH-QUERY."
  ;; Get values of the child widgets of the "search-builder"
  (let ((search-list (widget-editable-list-value-get (pubmed-advanced-search--widget-get 'search-builder))))
    (if (eq search-list nil)
	(setq pubmed-advanced-search-query "")
      (dolist (element search-list)
	(let ((boolean (nth 0 element))
	      (field (nth 1 element))
	      (text (nth 2 element)))
	  ;; Construct PUBMED-ADVANCED-SEARCH-QUERY
	  ;; Use only the elements with text in the editable field
	  (when (not (string-empty-p text))
	    ;; If the element is the first in SEARCH-LIST and BOOLEAN is "AND", ignore BOOLEAN
	    (if (and (eq element (nth 0 search-list)) (equal boolean "AND"))
		(progn
		  (setq pubmed-advanced-search-query (s-concat text field)))
	      ;; Else wrap the previous query in parentheses and append the query BOOLEAN TEXT[FIELD]
	      (progn
		(setq pubmed-advanced-search-query (s-append (s-concat " " boolean " " text field) (s-wrap pubmed-advanced-search-query "(" ")")))))))))
    ;; Show PUBMED-ADVANCED-SEARCH-QUERY in the "searchbox" widget
    (save-excursion
      (pubmed-advanced-search--widget-change-text (pubmed-advanced-search--widget-get 'searchbox) pubmed-advanced-search-query))
    pubmed-advanced-search-query))

(defun pubmed-advanced-search--add-to-builder (query &optional boolean)
  "Add BOOLEAN and QUERY from the history to the \"search-builder\"."
  (with-current-buffer "*PubMed Advanced Search Builder*"
    (let* ((old-value (widget-editable-list-value-get (pubmed-advanced-search--widget-get 'search-builder)))
	   (new-value (append old-value (list `(,boolean "" ,query)))))
      (widget-value-set (pubmed-advanced-search--widget-get 'search-builder) new-value)
      (widget-setup)
      (pubmed-advanced-search--build-search))))

(defun pubmed-advanced-search--history-show ()
  "Populate the tabulated list mode buffer."
  (let ((pubmed-advanced-search-history-buffer (get-buffer-create "*PubMed History*")))
    (with-current-buffer pubmed-advanced-search-history-buffer
      (pubmed-advanced-search-history-mode)
      (setq tabulated-list-entries pubmed-advanced-search-history)
      (tabulated-list-print t))
    (save-selected-window
      (display-buffer pubmed-advanced-search-history-buffer))))

(defun pubmed-advanced-search--history-add (query)
  "Add QUERY to PUBMED-ADVANCED-SEARCH-HISTORY."
  (setq pubmed-advanced-search-history-counter (1+ pubmed-advanced-search-history-counter))
  (let* ((result (pubmed-advanced-search-history--get-count query))
	 (count (plist-get result :count))
	 (querykey (plist-get result :querykey))
	 (webenv (plist-get result :webenv))
	 (time (format-time-string "%H:%M:%S"))
	 (entry (list
		 pubmed-advanced-search-history-counter
		 (vector
		  (concat "#" querykey)
		  (cons "AND" `(help-echo "AND in builder" action (lambda (button)
								    (progn
								      (with-current-buffer "*PubMed Advanced Search Builder*"
									(pubmed-advanced-search--build-search))
								      (pubmed-advanced-search--add-to-builder ,query "AND")))))
		  (cons "OR" `(help-echo "OR in builder" action (lambda (button)
								  (progn
								    (with-current-buffer "*PubMed Advanced Search Builder*"
								      (pubmed-advanced-search--build-search))
								    (pubmed-advanced-search--add-to-builder ,query "OR")))))
		  (cons "NOT" `(help-echo "NOT in builder" action (lambda (button)
								    (progn
								      (with-current-buffer "*PubMed Advanced Search Builder*"
									(pubmed-advanced-search--build-search))
								      (pubmed-advanced-search--add-to-builder ,query "NOT")))))
		  (cons "Del" `(help-echo "Delete from history" action
					  (lambda (button)
					    (when (y-or-n-p (concat "Are you sure you want to delete query #" ,querykey "? "))
					      (setq pubmed-advanced-search-history (delq (assoc (tabulated-list-get-id) pubmed-advanced-search-history) pubmed-advanced-search-history))
					      (pubmed-advanced-search--history-show)))))
		  query
		  (cons count `(help-echo "Show search results" action (lambda (button)
									 (funcall #'pubmed--get-docsums ,querykey ,webenv (string-to-number ,count)))))
		  time))))
    (push entry pubmed-advanced-search-history)
    (pubmed-advanced-search--history-show)))

(defun pubmed-advanced-search-history-clear ()
  "Clear PUBMED-ADVANCED-SEARCH-HISTORY."
  (setq pubmed-advanced-search-history-counter 0
	pubmed-advanced-search-history nil)
  (pubmed-advanced-search--history-show))
;;;; Footer

(provide 'pubmed-advanced-search)

;;; pubmed-advanced-search.el ends here
