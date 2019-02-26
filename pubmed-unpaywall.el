;;; pubmed-unpaywall.el --- Deferred functions to PubMed -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@xs4all.nl>
;; Created: 2018-05-23
;; Version: 0.1
;; Keywords: pubmed
;; Package-Requires: ((emacs "25.1") (deferred) (pdf-tools)
;; URL: https://gitlab.com/fvdbeek/emacs-pubmed

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

;; Download fulltext PDFs of Open Access articles using Unpaywall <https://unpaywall.org/products/api>. Using Unpaywall is legal and requires you to provide your email address by setting the value of UNPAYWALL-EMAIL in your .init.el or .emacs file: (setq unpaywall-email "your_email@example.com")

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'deferred)
(require 'eww)
(require 'json)
(require 'pdf-tools)
(require 'url)

;;;; Variables

(defvar unpaywall-url "https://api.unpaywall.org"
  "Unpaywall URL.")

;; The current version of the API is Version 2, and this is the only version supported.
(defvar unpaywall-version "v2"
  "Unpaywall API version.")

;; Requests must include your email as a parameter at the end of the URL, like this: api.unpaywall.org/my/request?email=YOUR_EMAIL.
(defvar unpaywall-email ""
  "E-mail address to authenticate Unpaywall requests.")

;;;; Commands

(defun pubmed-get-unpaywall ()
  (interactive)
  "In Pubmed, fetch the fulltext PDF of the current entry from Unpaywall or return nil if none is found."
  (if pubmed-uid
      (pubmed--unpaywall pubmed-uid)
    (error "No entry selected")))

;;;; Functions

(defun pubmed--unpaywall (pmid)
  "Deferred chain to retrieve the fulltext PDF of the PMID."
  (let ((url_for_pdf))
    (deferred:$
      ;; try
      (deferred:$
	(deferred:timeout 5000 "Time-out"
	  (let* ((doi (pubmed-convert-id pmid))
    		 (url (concat unpaywall-url "/" unpaywall-version "/" doi))
		 (parameters (list (cons "email" unpaywall-email))))
	    (deferred:url-get url parameters)))

	(deferred:nextc it
	  (lambda (buffer)
	    "Parse the JSON object in BUFFER. Return the url of the Open Access fulltext article or nil if none is found."
	    ;; FIXME: look for `url_for_pdf' in all `oa_locations'
	    (let* ((json (with-current-buffer buffer (decode-coding-string (buffer-string) 'utf-8)))
		   (json-object-type 'plist)
		   (json-array-type 'list)
		   (json-key-type nil)
		   (json-object (json-read-from-string json))
		   (msg (plist-get json-object :msg))
		   (is_error (plist-get json-object :error))
		   (error_message (plist-get json-object :message))
		   (best_oa_location (plist-get json-object :best_oa_location))
		   (url_for_pdf (plist-get best_oa_location :url_for_pdf)))
	      (cond
	       ((equal is_error t)
		(signal is_error error_message))
	       (msg
		(error "Unpaywall message: %s" msg))
	       (url_for_pdf
		(progn
		  (setq url_for_pdf url_for_pdf)
		  url_for_pdf))
	       (t
		(error "Unpaywall found no fulltext article"))))))

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
		url_for_pdf)))))

	(deferred:nextc it
	  (lambda (url)
	    (deferred:timeout 5000 "Time-out"
	      (deferred:url-retrieve url))))
	
	(deferred:nextc it
	  (lambda (buffer)
	    "Parse the HTML object in BUFFER and show the PDF."
	    (let* ((headers (with-current-buffer buffer (eww-parse-headers)))
		   (content-type (cdr (assoc "content-type" headers))))
	      (message "Content-type: %s" content-type)
	      (cond
	       ;; Show the PDF if the iframe contains a pdf file
	       ((equal content-type "application/pdf")
		(deferred:call 'pubmed--unpaywall-view-pdf buffer))
	       (t
		(error "Unknown content-type: %s" content-type)))))))

      ;; catch
      (deferred:error it
	(lambda (deferred-error)
	  "Catch any errors that occur during the deferred chain and return nil."
	  (message "%S: %S" (car deferred-error) (cdr deferred-error))
	  nil))
      
      ;; finally
      (deferred:nextc it
	(lambda (result)
	  "Return non-nil if a fulltext article is found, otherwise nil."
	  result)))))

(defun pubmed--unpaywall-view-pdf (&optional buffer)
  "View PDF in BUFFER with `pdf-tools'."
  (let ((data (with-current-buffer buffer (buffer-substring (1+ url-http-end-of-headers) (point-max))))
	(pdf-buffer (generate-new-buffer "*Unpaywall PDF*")))
    (unwind-protect
    	(with-current-buffer pdf-buffer
    	  (set-buffer-file-coding-system 'binary)
    	  (erase-buffer)
    	  (insert data)
    	  (pdf-view-mode)
    	  (switch-to-buffer-other-frame pdf-buffer))
      (kill-buffer buffer))))

;;;; Footer

(provide 'pubmed-unpaywall)

;;; pubmed-unpaywall.el ends here
