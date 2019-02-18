;;; pubmed-advanced-search.el --- PubMed Advanced search Builder -*-
;;; lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@xs4all.nl>
;; Created: 2018-05-23
;; Version: 0.1
;; Keywords: pubmed
;; Package-Requires: ((emacs "25.1") (s "1.10"))
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
(require 'pubmed-history)
(require 'pubmed-completion-at-point)
(require 's)
(require 'widget)

(defvar pubmed-query ""
  "The PubMed search query.")

(defvar pubmed-widget-lookup-table nil
  "A table for lookup markers created in current buffer.")

(defun pubmed-widget-create (id widget)
  (if (assoc id widget-demo-form)
      (error "identifier %S is used!" id)
    (push (cons id widget) pubmed-widget-lookup-table)))

(defun pubmed-widget-add (id widget)
  (let ((old (assoc id pubmed-widget-lookup-table)))
    (if old
        (setcdr old widget)
      (push (cons id widget) pubmed-widget-lookup-table))))

(defun pubmed-widget-get (id)
  "Look up the widget by ID."
  (cdr (assoc id pubmed-widget-lookup-table)))

(defun pubmed-widget-build-search ()
  "Build the search and return PUBMED-QUERY."
  (interactive)
  ;; Get values of the child widgets of the `search-builder'
  (let ((search-list (widget-editable-list-value-get (pubmed-widget-get 'search-builder))))
    (if (eq search-list nil)
	(setq pubmed-query "")
      (dolist (element search-list)
	(let ((boolean (nth 0 element))
	      (field (nth 1 element))
	      (text (nth 2 element)))
	  ;; Construct PUBMED-QUERY
	  ;; Use only the elements with text in the editable field
	  (when (not (string-empty-p text))
	    ;; If the element is the first in SEARCH-LIST, ignore BOOLEAN
	    (if (eq element (nth 0 search-list))
		(progn
		  (setq pubmed-query (s-concat text field)))
	      ;; Else wrap the previous query in parentheses and append the query BOOLEAN TEXT[FIELD]
	      (progn
		(setq pubmed-query (s-append (s-concat " " boolean " " text field) (s-wrap pubmed-query "(" ")")))))))))
    ;; Show PUBMED-QUERY in the `searchbox' widget
    (save-excursion
      (pubmed-widget-change-text (pubmed-widget-get 'searchbox) pubmed-query))
    pubmed-query))

(defun pubmed-widget-change-text (widget string)
  (save-excursion
    (goto-char (widget-field-start widget))
    (delete-region (point) (widget-field-end widget))
    (insert string)))

(defun pubmed-add-to-builder (query &optional boolean)
  "Add BOOLEAN and QUERY from the history to the `search-builder'."
  (interactive)
  (with-current-buffer "*PubMed Advanced Search Builder*"
    (let* ((old-value (widget-editable-list-value-get (pubmed-widget-get 'search-builder)))
	   (new-value (append old-value (list `(,boolean "" ,query)))))
      (widget-value-set (pubmed-widget-get 'search-builder) new-value)
      (widget-setup)
      (pubmed-widget-build-search))))

(defun pubmed-widget-get-sibling (widget widget-type)
  "Get the sibling of WIDGET with type WIDGET-TYPE."
  (let ((siblings (widget-get (widget-get widget :parent) :children))
	sibling-of-type)
    (dolist (sibling siblings sibling-of-type)
      (when (eq (widget-type sibling) 'editable-field)
	(setq sibling-of-type sibling)))))

(defun pubmed-advanced-search ()
  (interactive)
  (switch-to-buffer "*PubMed Advanced Search Builder*")
  (kill-all-local-variables)
  (make-local-variable 'pubmed-search-builder)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "PubMed Advanced Search Builder\n\n")

  (pubmed-widget-add 'searchbox
		     (widget-create 'editable-field
				    "Use the builder below to create your search"))
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (pubmed-advanced-search))
		 "Clear")
  
  (widget-insert "\n\n")

  (widget-insert "Builder\n\n")
  
  (setq pubmed-search-builder
	(pubmed-widget-add 'search-builder
			   (widget-create 'editable-list
  					  ;; for each entry in the list, insert the [INS] button, the [DEL] button and the `group' widget.
					  :entry-format "%i %d %v"
					  ;; start with two entries with default values
					  :value  '(("AND" "" ""))
					  ;; custom :list-length property with the length of the list as argument
					  :list-length 2
					  ;; update the :list-length property when the `editable-list' is changed
					  :notify (lambda (widget &rest ignore)
						    (pubmed-widget-build-search))
					  '(group
		       			    (menu-choice
		 			     :format "%[%v%]"
					     :value "AND"
					     :help-echo "Select Boolean operator"
					     :notify (lambda (widget &rest ignore)
						       (pubmed-widget-build-search))
					     (item "AND")
					     (item "OR")
					     (item "NOT"))
					    (menu-choice
					     :format "%[%v%]"
					     :tag "All Fields"
					     :value ""
					     :help-echo "Select Field"
					     :notify (lambda (widget &rest ignore)
					     	       (pubmed-widget-build-search)
						       (let ((value (widget-get widget :value))
							     (sibling (pubmed-widget-get-sibling widget 'editable-field)))
							 (cond
							  ((equal value "")
							   (widget-put sibling :complete-function 'pubmed-complete))
							  ((string-prefix-p "[Author" value)
							   (widget-put sibling :complete-function 'pubmed-author-complete))
							  ((equal value "[Journal]")
							   (widget-put sibling :complete-function 'pubmed-journal-complete))
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
						       ;; Insert a new item.
						       (widget-apply (pubmed-widget-get 'search-builder) :insert-before (widget-get (pubmed-widget-get 'search-builder) :widget)))
					     :notify (lambda (widget &rest ignore)
						       (pubmed-widget-build-search))
					     "")))))
  
  (widget-insert "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (pubmed-search pubmed-query))
		 "Search")
  (widget-insert " or ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (pubmed-add-to-history pubmed-query))
		 "Add to history")
  (widget-insert "\n\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (pubmed-show-history))
		 "Show history")
  (widget-insert " or ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (when (yes-or-no-p "Your entire search history is about to be cleared.")
			     (pubmed-clear-history)))
		 "Clear history")

  (use-local-map widget-keymap)
  
  (widget-setup))

;;;; Footer

(provide 'pubmed-advanced-search)

;;; pubmed.el ends here
