;;; pubmed-pmc.el --- Fetch fulltext PDFs from PubMed Central (PMC) -*- lexical-binding: t; -*-

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

;; Download fulltext PDFs of Open Access articles using PubMed Central® (PMC)
;; <https://www.ncbi.nlm.nih.gov/pmc/>. PubMed Central® (PMC) is a free
;; full-text archive of biomedical and life sciences journal literature at the
;; U.S. National Institutes of Health's National Library of Medicine (NIH/NLM).

;;; Code:

;;;; Requirements

(require 'deferred)
(require 'eww)
(require 'esxml)
(require 'esxml-query)
(require 'url)

;;;; Variables
(defvar pubmed-pmc-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi"
  "PMC base URL.")

;;;; Customization

(defgroup pubmed-pmc nil
  "Fetch fulltext PDFs from PubMed Central (PMC)."
  :group 'pubmed)

(defcustom pubmed-pmc-timeout 5000
  "PMC timeout in milliseconds."
  :group 'pubmed-pmc
  :type 'integer)

;;;; Commands

;;;###autoload
(defun pubmed-get-pmc (&optional entries)
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
        (setq pubmed-uid (tabulated-list-get-id))
	(when (eq mark ?*)
          (push pubmed-uid mark-list))
	(forward-line)))
    (cond
     (entries
      (mapcar #'pubmed--get-pmc entries))
     (mark-list
      (mapcar #'pubmed--get-pmc mark-list))
     ((tabulated-list-get-id)
      (pubmed--get-pmc (tabulated-list-get-id)))
     (t
      (error "No entry selected")))))

;;;; Functions

(defun pubmed--get-pmc (uid)
  "Try to fetch the fulltext PDF of UID, using PMC."
  (deferred:$
    (deferred:call #'pubmed-pmc uid)

    (deferred:nextc it
      (lambda (result)
	(when (bufferp result)
	  (pubmed--view-pdf result))))))

(defun pubmed-pmc (uid)
  "Deferred chain to retrieve the fulltext PDF of the UID."
  (deferred:$
    ;; try
    (deferred:$
      (deferred:next
	(lambda ()
	  (let ((url-request-method "GET")
		(arguments (concat "?"
				   "dbfrom=pubmed"
				   "&cmd=prlinks"
				   "&retmode=ref"
				   "&id=" uid
				   (when (not (string-empty-p pubmed-api-key))
				     (concat "&api_key=" pubmed-api-key)))))
	    (concat pubmed-pmc-url arguments))))

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
	  (deferred:timeout pubmed-pmc-timeout (error "Timeout")
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
    	    	url
	      (error "PMC found no fulltext article")))))

      (deferred:nextc it
	(lambda (url)
	  (deferred:timeout pubmed-pmc-timeout (error "Timeout")
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
	result))))

;;;; Footer

(provide 'pubmed-pmc)

;;; pubmed-pmc.el ends here
