;;; pubmed-bibtex.el --- Export BibTex from PubMed -*- lexical-binding: t; -*-

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

;; Translate the PubMed document summaries (DocSums) to BibTeX
;; entries. PubMed includes citations for journal articles and a
;; subset of books and book chapters available on the NCBI Bookshelf.
;; These are declared as @article, @book, and @incollection,
;; respectively.

;; Choosing BibTeX field types

;; The @article reference type is used for an article from a magazine
;; or a journal. Required fields are: author, title, journal, year.
;; Optional fields are: volume, number, pages, month, note.

;; The @book reference type is used for a published book. Required
;; fields are: author/editor, title, publisher, year. Optional fields
;; are: volume/number, series, address, edition, month, note.

;; The @incollection is used for a section of a book having its own
;; title. Required fields are: author, title, booktitle, publisher,
;; year. Optional fields are: editor, volume/number, series, type,
;; chapter, pages, address, edition, month, note.

;; Of the optional fields, the series, type, and chapter fields are
;; not included in the PubMed DocSums. When the note field is enabled,
;; it is set to an empty value for later use. Besides the required and
;; optional fields, the following non-standard fields can be included
;; in the BibTeX entries: issn, pubmed, pii, doi, url.

;; The optional and non-standard fields can be included by setting the
;; values of the corresponding variables in your init.el or .emacs,
;; e.g. (setq pubmed-bibtex-article-number t)

;; Choosing BibTeX citation key

;; The first key of a BibTeX entry the citation key, or the BibTeX
;; key. This key must be unique for all entries in your bibliography.
;; It is this identifier that you will use within your document to
;; cross-reference it to this entry (i.e., the identifier you use in
;; the \cite{} command in your LaTeX file). By default the key is
;; composed of a the PMID prefixed with the string "pmid".
;; Alternatively, you could use the author-year citation style, that
;; follow the loose standard in which the author's surname followed by
;; the year of publication is used. This could be more convenient,
;; because it enables you to guess the key from looking at the
;; reference. This protects you from accidentally duplicating a
;; reference in the Bibtex file and referring to it under two
;; different keys.

;; Configuring default BibTeX file

;; If you have a master BibTeX file, e.g. bibliography.bib, and want
;; it to serve as the default file to append or write the BibTeX
;; reference to, you can set PUBMED-BIBTEX-DEFAULT-FILE in your
;; init.el or .emacs: (setq pubmed-bibtex-default-file
;; "/path/to/bibliography.bib") `pubmed-bibtex-default-file'

;;; Code:

;;;; Requirements

(require 'json)
(require 's)

;;;; Variables

(defvar pubmed-bibtex-default-file nil
  "Default BibTeX file.")

(defvar pubmed-bibtex-citation-key "pmid"
  "The scheme for creating citation keys of the BibTeX entries.
Default is \"pmid\", in which \"pmid\" followed by the PMID of
the publication is used. Alternatively, you could use
\"authoryear\", to follow the loose standard in which the
author's surname followed by the year of publication is used.")

(defvar pubmed-bibtex-citation-keys nil)

(defconst pubmed-bibtex-entry-buffer-name "*BibTeX entry*"
  "Buffer name for BibTeX entry.")

(defvar pubmed-bibtex-article-volume t
  "If non-nil, include the optional \"volume\" field name in the \"@article\" reference type.")

(defvar pubmed-bibtex-article-number nil
  "If non-nil, include the optional \"number\" field name in the \"@article\" reference type.")

(defvar pubmed-bibtex-article-pages t
  "If non-nil, include the optional \"pages\" field name in the \"@article\" reference type.")

(defvar pubmed-bibtex-article-month nil
  "If non-nil, include the optional \"month\" field name in the \"@article\" reference type.")

(defvar pubmed-bibtex-article-note nil
  "If non-nil, include the optional \"note\" field name in the \"@article\" reference type.")

(defvar pubmed-bibtex-article-issn nil
  "If non-nil, include the non-standard \"issn\" name in the \"@article\" reference type.")

(defvar pubmed-bibtex-article-pubmed nil
  "If non-nil, include the non-standard \"pubmed\" name in the \"@article\" reference type.")

(defvar pubmed-bibtex-article-pii nil
  "If non-nil, include the non-standard \"pii\" field name in the \"@article\" reference type.")

(defvar pubmed-bibtex-article-doi t
  "If non-nil, include the non-standard \"doi\" field name in the \"@article\" reference type.")

(defvar pubmed-bibtex-article-url nil
  "If non-nil, include the non-standard \"url\" field name in the \"@article\" reference type.")

(defvar pubmed-bibtex-book-volume t
  "If non-nil, include the optional \"volume\" field name in the \"@book\" reference type.")

(defvar pubmed-bibtex-book-number nil
  "If non-nil, include the optional \"number\" field name in the \"@book\" reference type.")

(defvar pubmed-bibtex-book-address t
  "If non-nil, include the optional \"address\" field name in the \"@book\" reference type.")

(defvar pubmed-bibtex-book-edition t
  "If non-nil, include the optional \"edition\" field name in the \"@book\" reference type.")

(defvar pubmed-bibtex-book-month nil
  "If non-nil, include the optional \"month\" field name in the \"@book\" reference type.")

(defvar pubmed-bibtex-book-note nil
  "If non-nil, include the optional \"note\" field name in the \"@book\" reference type.")

(defvar pubmed-bibtex-book-pubmed nil
  "If non-nil, include the non-standard \"pubmed\" name in the \"@book\" reference type.")

(defvar pubmed-bibtex-book-url nil
  "If non-nil, include the non-standard \"url\" field name in the \"@book\" reference type.")

(defvar pubmed-bibtex-incollection-editor t
  "If non-nil, include the optional \"editor\" field name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-volume nil
  "If non-nil, include the optional \"volume\" field name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-number nil
  "If non-nil, include the optional \"number\" field name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-chapter nil
  "If non-nil, include the optional \"chapter\" field name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-pages t
  "If non-nil, include the optional \"pages\" field name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-address t
  "If non-nil, include the optional \"address\" field name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-edition t
  "If non-nil, include the optional \"edition\" field name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-month nil
  "If non-nil, include the optional \"month\" field name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-note nil
  "If non-nil, include the optional \"note\" field name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-pubmed nil
  "If non-nil, include the non-standard \"pubmed\" name in the \"@incollection\" reference type.")

(defvar pubmed-bibtex-incollection-url nil
  "If non-nil, include the non-standard \"url\" field name in the \"@incollection\" reference type.")

;;;;; Commands

;;;###autoload
(defun pubmed-bibtex-show (&optional entries)
  "In PubMed, show the BibTeX references of the marked entries or current entry. If optional argument ENTRIES is a list of UIDs, show the BibTeX references of the entries."
  (interactive "P")
  (pubmed--guard)
  (let (mark
	mark-list
	pubmed-uid)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq mark (char-after))
        (setq pubmed-uid (tabulated-list-get-id))
	(when (eq mark ?*)
          (push pubmed-uid mark-list))
	(forward-line)))
    (cond
     (entries
      (let ((bibtex-entry-buffer (get-buffer-create pubmed-bibtex-entry-buffer-name)))
	(with-current-buffer bibtex-entry-buffer
	  (erase-buffer)
	  (mapc (lambda (x) (pubmed-bibtex--insert x)) entries)
	  (bibtex-mode)
	  ;; Call `bibtex-set-dialect' to set `bibtex-entry-head', or
	  ;; `bibtex-parse-buffers-stealthily' will throw an error
	  ;; because the buffer is not associated with an existing
	  ;; file
	  (bibtex-set-dialect)
	  (goto-char (point-min)))
	(save-selected-window
	  (display-buffer bibtex-entry-buffer))))
     (mark-list
      (let ((bibtex-entry-buffer (get-buffer-create pubmed-bibtex-entry-buffer-name)))
	(with-current-buffer bibtex-entry-buffer
	  (erase-buffer)
	  (mapc (lambda (x) (pubmed-bibtex--insert x)) mark-list)
	  (bibtex-mode)
	  (bibtex-set-dialect)
	  (goto-char (point-min)))
	(save-selected-window
	  (display-buffer bibtex-entry-buffer))))
     ((tabulated-list-get-id)
      (let ((bibtex-entry-buffer (get-buffer-create pubmed-bibtex-entry-buffer-name))
	    (pubmed-uid (tabulated-list-get-id)))
	(with-current-buffer bibtex-entry-buffer
	  (erase-buffer)
	  (pubmed-bibtex--insert pubmed-uid)
	  (bibtex-mode)
	  (bibtex-set-dialect)
	  (goto-char (point-min)))
	(save-selected-window
	  (display-buffer bibtex-entry-buffer))))
     (t
      (error "No entry selected")))))

;;;###autoload
(defun pubmed-bibtex-write (&optional file entries)
  "In PubMed, write the BibTeX references of the marked entries or current entry to file FILE. If optional argument ENTRIES is a list of UIDs, write the BibTeX references of the entries."
  (interactive
   (list (read-file-name "Write to BibTeX file: " nil pubmed-bibtex-default-file nil pubmed-bibtex-default-file)))
  (pubmed--guard)
  (if (not (file-writable-p file)) (error "Output file not writable")
    (if entries
	(pubmed-bibtex--write file entries)
      (pubmed-bibtex--write file))))

;;;###autoload
(defun pubmed-bibtex-append (&optional file entries)
  "In PubMed, append the BibTeX references of the marked entries or current entry to file FILE. If optional argument ENTRIES is a list of UIDs, write the BibTeX references of the entries."
  (interactive "FAppend to BibTeX file: ")
  (pubmed--guard)
  (if (not (file-writable-p file)) (error "Output file not writable")
    (if entries
	(pubmed-bibtex--write file entries append)
      (pubmed-bibtex--write file nil append))))

(defun pubmed-bibtex--list-citation-keys (string)
  "Return a list of all BibTeX citation keys in STRING."
  (let ((regexp "\\(@article\\|@book\\|@incollection\\){\\([[:alnum:]]+\\),$")
	(pos 0)
        matches)
    (while (string-match regexp string pos)
      (push (match-string 2 string) matches)
      (setq pos (match-end 0)))
    (matches (reverse matches))
    matches))

(defun pubmed-bibtex--write (file &optional entries append)
  "In PubMed, write the BibTeX references of the marked entries or current entry to file FILE. If optional argument ENTRIES is a list of UIDs, write the BibTeX references of the entries. If optional argument APPEND is non-nil, append the BibTeX references to a BibTeX database."
  (let (mark
	mark-list
	pubmed-uid)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq mark (char-after))
        (setq pubmed-uid (tabulated-list-get-id))
	(when (eq mark ?*)
          (push pubmed-uid mark-list))
	(forward-line)))
    (cond
     (entries
      (let ((bibtex-entry-buffer (get-buffer-create pubmed-bibtex-entry-buffer-name)))
     	(with-current-buffer bibtex-entry-buffer
     	  (erase-buffer)
     	  (mapc (lambda (x) (pubmed-bibtex--insert x)) entries)
     	  (bibtex-mode)
     	  (goto-char (point-min)))
     	(switch-to-buffer-other-window bibtex-entry-buffer)))
     (mark-list
      (let ((bibtex-entry-buffer (get-buffer-create pubmed-bibtex-entry-buffer-name)))
	(with-current-buffer bibtex-entry-buffer
	  (erase-buffer)
	  (mapc (lambda (x) (pubmed-bibtex--insert x)) mark-list)
	  (bibtex-mode)
	  (goto-char (point-min)))))
     ((tabulated-list-get-id)
      (let ((bibtex-entry-buffer (get-buffer-create pubmed-bibtex-entry-buffer-name)))
	(with-current-buffer bibtex-entry-buffer
	  (erase-buffer)
	  (pubmed-bibtex--insert pubmed-uid)
	  (bibtex-mode)
	  (goto-char (point-min)))))
     (t
      (error "No entry selected")))
    (with-current-buffer (get-buffer pubmed-bibtex-entry-buffer-name)
      (write-region nil nil file append))))

(defun pubmed-bibtex--insert (uid)
  "Insert the BibTeX reference of UID in the current buffer."
  ;; TODO: Ensure unique citation keys by appending letters to multiple papers from the same author and year when using the "authoryear" naming scheme
  ;; TODO: Correct incorrectly capitalized authors, e.g. uid 21026888
  ;; FIXME: Fix parsing of author values where the initials aren't last, e.g. uid 15137
  (let* ((keyword (intern (concat ":" uid)))
	 (value (plist-get pubmed-entries keyword)))
    (cond
     ((equal (plist-get value :doctype) "citation")
      (progn
	(insert "@article{")
	(cond
	 ((equal pubmed-bibtex-citation-key "pmid")
	  (insert "pmid" (plist-get value :uid)))
	 ((equal pubmed-bibtex-citation-key "authoryear")
	  (let* ((author (downcase (s-join "" (butlast (split-string (plist-get value :sortfirstauthor) " ")))))
		 (year (nth 0 (split-string (plist-get value :pubdate) " ")))
		 (authoryear (concat author year)))
	    (push authoryear pubmed-bibtex-citation-keys)
	    (insert authoryear))))
	;; BibTeX accepts names in the format "forename surname" or "surname, forename". Here, the latter is used.
	(let ((authorlist (plist-get value :authors))
	      (authors))
	  (dolist (author authorlist authors)
	    (let* ((name (plist-get author :name))
		   (initials (car (last (split-string name " "))))
		   ;; Add periods and spaces after all initials, and then remove the space after the last initial
		   (dotted-initials (s-trim-right (mapconcat (lambda (x) (string x ?. ?\s)) initials "")))
		   (lastname (s-join " " (butlast (split-string name " ")))))
	      (push (concat lastname ", " dotted-initials) authors)))
	  (insert ",\n"
	   	  "author = {"
	   	  (s-join " and " (nreverse authors))
	   	  "}"))
	(insert ",\n"
		"title = {{"
		(plist-get value :title)
		"}}")
	(insert ",\n"
		"journal = {{"
		(plist-get value :source)
		"}}")
	(when (and pubmed-bibtex-article-issn (string> (plist-get value :issn) ""))
	  (insert ",\n"
	       	  "issn = {"
	       	  (plist-get value :issn)
	       	  "}"))
	(let* ((pubdatelist (split-string (plist-get value :pubdate) " "))
	       (year (nth 0 pubdatelist))
	       (month (nth 1 pubdatelist)))
	  (when year
	    (insert ",\n"
	   	    "year = {"
		    year
	   	    "}"))
	  (when (and pubmed-bibtex-article-month month)
	    (insert ",\n"
	   	    "month = {"
		    month
	   	    "}")))
	(when (and pubmed-bibtex-article-volume
		   (string> (plist-get value :volume) ""))
	  (insert ",\n"
		  "volume = {"
		  (plist-get value :volume)
		  "}"))
	(when (and pubmed-bibtex-article-number
		   (string> (plist-get value :issue) ""))
	  (insert ",\n"
		  "number = {"
		  (plist-get value :issue)
		  "}"))
	(when (and pubmed-bibtex-article-pages
		   (string> (plist-get value :pages) ""))
	  (insert ",\n"
		  "pages = {"
		  (plist-get value :pages)
		  "}"))
	(let ((articleids (plist-get value :articleids)))
	  (dolist (articleid articleids)
	    (when (and pubmed-bibtex-article-pubmed (equal (plist-get articleid :idtype) "pubmed"))
	      (insert ",\n"
	   	      (plist-get articleid :idtype)
	   	      " = {"
	   	      (plist-get articleid :value)
	   	      "}"))
	    (when (and pubmed-bibtex-article-pii (equal (plist-get articleid :idtype) "pii"))
	      (insert ",\n"
	   	      (plist-get articleid :idtype)
	   	      " = {"
	   	      (plist-get articleid :value)
	   	      "}"))
	    (when (and pubmed-bibtex-article-doi (equal (plist-get articleid :idtype) "doi"))
	      (insert ",\n"
	   	      (plist-get articleid :idtype)
	   	      " = {"
	   	      (plist-get articleid :value)
	   	      "}"))))
	(when (and pubmed-bibtex-article-url (string> (plist-get value :availablefromurl) ""))
	  (insert ",\n"
	   	  "url = {"
	   	  (plist-get value :availablefromurl)
	   	  "}"))
	(when pubmed-bibtex-article-note
	  (insert ",\n"
	   	  "note = {}"))
	(insert "\n"
		"}\n\n")))
     ((equal (plist-get value :doctype) "book")
      (progn
	(insert "@book{")
	(cond
	 ((equal pubmed-bibtex-citation-key "pmid")
	  (insert "pmid" (plist-get value :uid)))
	 ((equal pubmed-bibtex-citation-key "authoryear")
	  (let* ((author (downcase (s-join "" (butlast (split-string (plist-get value :sortfirstauthor) " ")))))
		 (year (nth 0 (split-string (plist-get value :pubdate) " ")))
		 (authoryear (concat author year)))
	    (push authoryear pubmed-bibtex-citation-keys)
	    (insert authoryear))))
	;; BibTeX accepts names in the format "forename/initials surname" or "surname, forename/initials".
	;; Here, the latter is used.
	(let ((authorlist (plist-get value :authors))
	      (authors)
	      (editors))
	  (dolist (author authorlist)
	    (let* ((name (plist-get author :name))
		   (initials (car (last (split-string name " "))))
		   ;; Add periods and spaces after all initials, and then remove the space after the last initial
		   (dotted-initials (s-trim-right (mapconcat (lambda (x) (string x ?. ?\s)) initials "")))
		   (lastname (s-join " " (butlast (split-string name " ")))))
	      (when (or (equal (plist-get author :authtype) "Author")
			(equal (plist-get author :authtype) "CollectiveName"))
		(push (concat lastname ", " dotted-initials) authors))
	      (when (equal (plist-get author :authtype) "Editor")
		(push (concat lastname ", " dotted-initials) editors))))
	  (insert ",\n"
	   	  "author = {"
	   	  (s-join " and " (nreverse authors))
	   	  "}")
	  (when editors
	    (insert ",\n"
	   	    "editor = {"
	   	    (s-join " and " (nreverse editors))
	   	    "}")))
	(insert ",\n"
		"title = {{"
		(plist-get value :booktitle)
		"}}")
	(insert ",\n"
		"publisher = {"
		(plist-get value :publishername)
		"}")
	(when (and pubmed-bibtex-book-volume (string> (plist-get value :volume) ""))
	  (insert ",\n"
		  "volume = {"
		  (plist-get value :volume)
		  "}"))
	(when (and pubmed-bibtex-book-number (string> (plist-get value :issue) ""))
	  (insert ",\n"
		  "number = {"
		  (plist-get value :issue)
		  "}"))
	(when (and pubmed-bibtex-book-address (string> (plist-get value :publisherlocation) ""))
	  (insert ",\n"
		  "address = {"
		  (plist-get value :publisherlocation)
		  "}"))
	(when (and pubmed-bibtex-book-edition (string> (plist-get value :edition) ""))
	  (insert ",\n"
	   	  "edition = {"
	   	  (plist-get value :edition)
	   	  "}"))
	(let* ((pubdatelist (split-string (plist-get value :pubdate) " "))
	       (year (nth 0 pubdatelist))
	       (month (nth 1 pubdatelist)))
	  (when year
	    (insert ",\n"
	   	    "year = {"
		    year
	   	    "}"))
	  (when (and pubmed-bibtex-book-month month)
	    (insert ",\n"
	   	    "month = {"
		    month
	   	    "}")))
	(let ((articleids (plist-get value :articleids)))
	  (dolist (articleid articleids)
	    (when (and pubmed-bibtex-book-pubmed (equal (plist-get articleid :idtype) "pubmed"))
	      (insert ",\n"
	   	      (plist-get articleid :idtype)
	   	      " = {"
	   	      (plist-get articleid :value)
	   	      "}"))))
	(when (and pubmed-bibtex-book-url (string> (plist-get value :availablefromurl) ""))
	  (insert ",\n"
	   	  "url = {"
	   	  (plist-get value :availablefromurl)
	   	  "}"))
	(when pubmed-bibtex-book-note
	  (insert ",\n"
	   	  "note = {}"))
	(insert "\n"
		"}\n\n")))
     ((equal (plist-get value :doctype) "chapter")
      (progn
	(insert "@incollection{")
	(cond
	 ((equal pubmed-bibtex-citation-key "pmid")
	  (insert "pmid" (plist-get value :uid)))
	 ((equal pubmed-bibtex-citation-key "authoryear")
	  (let* ((author (downcase (s-join "" (split-string (plist-get value :sortfirstauthor) " "))))
		 (year (nth 0 (split-string (plist-get value :pubdate) " ")))
		 (authoryear (concat author year)))
	    (push authoryear pubmed-bibtex-citation-keys)
	    (insert authoryear))))
	;; BibTeX accepts names in the format "forename surname" or "surname, forename". Here, the latter is used.
	(let ((authorlist (plist-get value :authors))
	      (authors)
	      (editors))
	  (dolist (author authorlist)
	    (let* ((name (plist-get author :name))
		   (initials (car (last (split-string name " "))))
		   ;; Add periods and spaces after all initials, and then remove the space after the last initial
		   (dotted-initials (s-trim-right (mapconcat (lambda (x) (string x ?. ?\s)) initials "")))
		   (lastname (s-join " " (butlast (split-string name " ")))))
	      (when (or (equal (plist-get author :authtype) "Author")
			(equal (plist-get author :authtype) "CollectiveName"))
		(push (concat lastname ", " dotted-initials) authors))
	      (when (equal (plist-get author :authtype) "Editor")
		(push (concat lastname ", " dotted-initials) editors))))
	  (insert ",\n"
	   	  "author = {"
	   	  (s-join " and " (nreverse authors))
	   	  "}")
	  (when (and pubmed-bibtex-incollection-editor editors)
	    (insert ",\n"
	   	    "editor = {"
	   	    (s-join " and " (nreverse editors))
	   	    "}")))
	(insert ",\n"
		"title = {{"
		(plist-get value :title)
		"}}")
	(insert ",\n"
		"booktitle = {{"
		(plist-get value :booktitle)
		"}}")
	(insert ",\n"
		"publisher = {"
		(plist-get value :publishername)
		"}")
	(when (and pubmed-bibtex-incollection-volume (string> (plist-get value :volume) ""))
	  (insert ",\n"
		  "volume = {"
		  (plist-get value :volume)
		  "}"))
	(when (and pubmed-bibtex-incollection-number (string> (plist-get value :issue) ""))
	  (insert ",\n"
		  "number = {"
		  (plist-get value :issue)
		  "}"))
	(when (and pubmed-bibtex-incollection-chapter (string> (plist-get value :locationlabel) ""))
	  (insert ",\n"
	       	  "chapter = {"
	       	  (plist-get value :locationlabel)
	       	  "}"))
	(when (and pubmed-bibtex-incollection-pages (string> (plist-get value :pages) ""))
	  (insert ",\n"
		  "pages = {"
		  (plist-get value :pages)
		  "}"))
	(when (and pubmed-bibtex-incollection-address (string> (plist-get value :publisherlocation) ""))
	  (insert ",\n"
		  "address = {"
		  (plist-get value :publisherlocation)
		  "}"))
	(when (and pubmed-bibtex-incollection-edition (string> (plist-get value :edition) ""))
	  (insert ",\n"
	   	  "edition = {"
	   	  (plist-get value :edition)
	   	  "}"))
	(let* ((pubdatelist (split-string (plist-get value :pubdate) " "))
	       (year (nth 0 pubdatelist))
	       (month (nth 1 pubdatelist)))
	  (when year
	    (insert ",\n"
	   	    "year = {"
		    year
	   	    "}"))
	  (when (and pubmed-bibtex-incollection-month month)
	    (insert ",\n"
	   	    "month = {"
		    month
	   	    "}")))
	(let ((articleids (plist-get value :articleids)))
	  (dolist (articleid articleids)
	    (when (and pubmed-bibtex-incollection-pubmed (equal (plist-get articleid :idtype) "pubmed"))
	      (insert ",\n"
	   	      (plist-get articleid :idtype)
	   	      " = {"
	   	      (plist-get articleid :value)
	   	      "}"))))
	(when (and pubmed-bibtex-incollection-url (string> (plist-get value :availablefromurl) ""))
	  (insert ",\n"
	   	  "url = {"
	   	  (plist-get value :availablefromurl)
	   	  "}"))
	(when pubmed-bibtex-incollection-note
	  (insert ",\n"
	   	  "note = {}"))
	(insert "\n"
		"}\n\n"))))))

;;;; Footer

(provide 'pubmed-bibtex)

;;; pubmed-bibtex.el ends here
