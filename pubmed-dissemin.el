;;; pubmed-dissemin.el --- Fetch fulltext PDFs from Dissemin -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>

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

;; Download fulltext PDFs of Open Access articles using Dissemin
;; <https://dev.dissem.in/api.html>. Dissemin gathers metadata from
;; multiple sources and links to full text publications in third-party
;; repositories (Zenodo <https://zenodo.org/>, HAL
;; <https://hal.archives-ouvertes.fr/>, and OSF preprints
;; <https://osf.io/preprints/>). Using Dissemin is legal.

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'deferred)
(require 'eww)
(require 'json)
(require 'url)

;;;; Variables

(defvar pubmed-dissemin-url "https://dissem.in/api/"
  "Dissemin URL.")

;;;; Customization

(defgroup pubmed-dissemin nil
  "Fetch fulltext PDFs from Dissemin."
  :group 'pubmed)

(defcustom pubmed-dissemin-timeout 5000
  "Dissemin timeout in milliseconds."
  :group 'pubmed-dissemin
  :type 'integer)

;;;; Commands

;;;###autoload
(defun pubmed-get-dissemin (&optional entries)
  "In PubMed, try to fetch the fulltext PDF of the marked entries, the current entry or the optional argument ENTRIES."
  ;; TODO: optional argument NOQUERY non-nil means do not ask the user to confirm.
  (interactive "P")
  (pubmed--guard)
  (let (mark
	mark-list
	pubmed-uid)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq mark (char-after))
        (setq pubmed-uid (pubmed--get-uid))
	(when (eq mark ?*)
          (push pubmed-uid mark-list))
	(forward-line)))
    (cond
     (entries
      (mapcar #'pubmed--get-dissemin entries))
     (mark-list
      (mapcar #'pubmed--get-dissemin mark-list))
     ((pubmed--get-uid)
      (pubmed--get-dissemin (pubmed--get-uid)))
     (t
      (error "No entry selected")))))

;;;; Functions

(defun pubmed--get-dissemin (uid)
  "Try to fetch the fulltext PDF of UID, using DISSEMIN."
  (deferred:$
    (deferred:call #'pubmed-dissemin uid)

    (deferred:nextc it
      (lambda (result)
	(when (bufferp result)
	  (pubmed--view-pdf result))))))

(defun pubmed-dissemin (uid)
  "Deferred chain to retrieve the fulltext PDF of the UID."
  (let (pdf-url)
    (deferred:$
      ;; try
      (deferred:$
	(deferred:next
	  (lambda ()
	    (let* ((keyword (intern (concat ":" uid)))
		   (value (plist-get pubmed-entries keyword))
		   (articleids (plist-get value :articleids))
		   doi)
	      (dolist (articleid articleids doi)
		(when (equal (plist-get articleid :idtype) "doi")
		  (setq doi (plist-get articleid :value)))))))

	(deferred:nextc it
	  (lambda (doi)
	    (deferred:timeout pubmed-dissemin-timeout (error "Timeout")
	      (let* ((url (concat pubmed-dissemin-url doi)))
                (if doi
		    (deferred:url-get url)
		  (error "Article has no doi"))))))

	(deferred:nextc it
	  (lambda (buffer)
	    "Parse the JSON object in BUFFER. Return the url of the Open Access fulltext article or nil if none is found."
	    (let* ((json (with-current-buffer buffer (decode-coding-string (buffer-string) 'utf-8)))
		   (json-object-type 'plist)
		   (json-array-type 'list)
		   (json-key-type nil)
		   (json-object (json-read-from-string json))
                   (status (plist-get json-object :status))
                   (paper (plist-get json-object :paper))
                   (records (plist-get paper :records)))
              (cond
	       ((not (equal status "ok"))
		(error "Dissemin status: %s" status))
	       ;; Loop through records to find a pdf_url and return the first one found
	       (records
		(let ((i 0))
		  (while (not (or (>= i (length records))
				  (plist-get (nth i records) :pdf_url)))
		    (setq i (1+ i)))
		  (if (>= i (length records))
		      (error "Dissemin found no fulltext article")
		    (setq pdf-url (plist-get (nth i records) :pdf_url)))))
	       (t
		(error "Dissemin found no fulltext article"))))))

	(deferred:nextc it
	  (lambda (url)
	    (if url
		(lexical-let ((d (deferred:new #'identity)))
		  (url-retrieve url (lambda (status)
				      ;; Start the following callback queue now.
				      (deferred:callback-post d status)))
		  ;; Return the unregistered (not yet started) callback
		  ;; queue, so that the following queue will wait until it
		  ;; is started.
		  d)
	      (deferred:cancel it))))

	;; You can connect deferred callback queues
	(deferred:nextc it
	  (lambda (status)
	    (let ((url-error (plist-get status :error))
		  (url-redirect (plist-get status :redirect)))
	      (cond
	       (url-error
		(signal (car url-error) (cdr url-error)))
	       (url-redirect
		(progn
		  (message "Redirected-to: %s" url-redirect)
		  url-redirect))
	       (t
		pdf-url)))))

	(deferred:nextc it
	  (lambda (url)
	    (deferred:timeout pubmed-dissemin-timeout (error "Timeout")
	      (deferred:url-retrieve url))))

	(deferred:nextc it
	  (lambda (buffer)
	    "Parse the HTML object in BUFFER and show the PDF."
	    (let* ((headers (with-current-buffer buffer (eww-parse-headers)))
		   (content-type (cdr (assoc "content-type" headers))))
	      (cond
	       ;; Return buffer if the iframe contains a pdf file
	       ((equal content-type "application/pdf")
		buffer)
	       (t
		(error "Unknown content-type: %s" content-type)))))))

      ;; catch
      (deferred:error it
	(lambda (deferred-error)
	  "Catch any errors that occur during the deferred chain and return nil."
	  (message "%s" (cadr deferred-error))
	  nil))

      ;; finally
      (deferred:nextc it
	(lambda (result)
	  "Return non-nil if a fulltext article is found, otherwise nil."
	  result)))))

;;;; Footer

(provide 'pubmed-dissemin)

;;; pubmed-dissemin.el ends here
