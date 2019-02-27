;;; pubmed-pmc.el --- Deferred functions to PubMed Central (PMC) -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@xs4all.nl>
;; Created: 2018-05-23
;; Version: 0.1
;; Keywords: pubmed
;; Package-Requires: ((emacs "25.1") (deferred) (esxml) (pdf-tools)
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

;; Download fulltext PDFs of Open Access articles using PubMed Central® (PMC) <https://www.ncbi.nlm.nih.gov/pmc/>. PubMed Central® (PMC) is a free full-text archive of biomedical and life sciences journal literature at the U.S. National Institutes of Health's National Library of Medicine (NIH/NLM).

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'deferred)
(require 'eww)
(require 'esxml)
(require 'esxml-query)
(require 'pdf-tools)
(require 'url)

;;;; Variables
(defvar pubmed-elink-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi"
  "ELink base URL.")

;;;; Commands

(defun pubmed-get-pmc ()
  (interactive)
  "In Pubmed, fetch the fulltext PDF of the current entry from PMC or return nil if none is found."
  (if pubmed-uid
      (pubmed--pmc pubmed-uid)
    (error "No entry selected")))

;;;; Functions

(defun pubmed--pmc (pmid)
  "Deferred chain to retrieve the fulltext PDF of the PMID."
  (deferred:$
    ;; try
    (deferred:$
      (deferred:next
	(lambda ()
	  (let ((url-request-method "GET")
		(arguments (concat "?"
				   "dbfrom=" pubmed-db
				   "&cmd=prlinks"
				   "&retmode=ref"
				   "&id=" pmid
				   (when (not (string-empty-p pubmed-api_key))
				     (concat "&api_key=" pubmed-api_key)))))
	    (concat pubmed-elink-url arguments))))

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
	      (error "PMC found no fulltext article"))))))
      
      (deferred:nextc it
	(lambda (url)
	  (deferred:timeout 5000 "Time-out"
	    (deferred:url-retrieve url))))
      
      (deferred:nextc it
	(lambda (buffer)
	  "Parse the HTML in BUFFER. Return the url of the iframe or nil if none is found."
	  ;; Extract the url of the pdf. The url is marked up as follows:
	  ;; <meta name="citation_pdf_url" content="URL_FOR_PDF"/>
	  ;; TODO: Some journals require a captcha, e.g. Indian Journal of Psychological Medicine.
	  (let* ((dom (with-current-buffer buffer (libxml-parse-html-region (point-min) (point-max))))
		 (url (esxml-node-attribute 'content (esxml-query "meta[name=citation_pdf_url]" dom))))
	    (if url
	    	(progn
	    	  (message "URL for PDF: %s" url)
    	    	  url)
	      (error "PMC found no fulltext article")))))

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
	      (deferred:call 'pubmed--pmc-view-pdf buffer))
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
	result))))

(defun pubmed--pmc-view-pdf (&optional buffer)
  "View PDF in BUFFER with `pdf-tools'."
  (let ((data (with-current-buffer buffer (buffer-substring (1+ url-http-end-of-headers) (point-max))))
	(pdf-buffer (generate-new-buffer "*PMC PDF*")))
    (unwind-protect
    	(with-current-buffer pdf-buffer
    	  (set-buffer-file-coding-system 'binary)
    	  (erase-buffer)
    	  (insert data)
    	  (pdf-view-mode)
    	  (switch-to-buffer-other-frame pdf-buffer))
      (kill-buffer buffer))))

;;;; Footer

(provide 'pubmed-pmc)

;;; pubmed-pmc.el ends here
