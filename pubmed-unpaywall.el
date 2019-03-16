;;; pubmed-unpaywall.el --- Fetch fulltext PDFs from Unpaywall -*- lexical-binding: t; -*-

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

;; Download fulltext PDFs of Open Access articles using Unpaywall
;; <https://unpaywall.org/products/api>. Using Unpaywall is legal and requires
;; you to provide your email address by customizing `pubmed-unpaywall-email' or
;; setting the value in your .init.el or .emacs file: (setq
;; pubmed-unpaywall-email "your_email@example.com")

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'deferred)
(require 'eww)
(require 'json)
(require 'url)

;;;; Variables

(defvar pubmed-unpaywall-url "https://api.unpaywall.org/v2/"
  "Unpaywall URL.")

;;;; Customization

(defgroup pubmed-unpaywall nil
  "Fetch fulltext PDFs from Unpaywall."
  :group 'pubmed)

(defcustom pubmed-unpaywall-email ""
  "Email address to authenticate Unpaywall requests.
Requests must include your email as a parameter at the end of the
  URL, like this: api.unpaywall.org/my/request?email=YOUR_EMAIL."
  :group 'pubmed-unpaywall
  :type 'string)

(defcustom pubmed-unpaywall-timeout 5000
  "Unpaywall timeout in milliseconds."
  :group 'pubmed-unpaywall
  :type 'integer)

;;;; Commands

;;;###autoload
(defun pubmed-get-unpaywall (&optional entries)
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
      (mapcar #'pubmed--get-unpaywall entries))
     (mark-list
      (mapcar #'pubmed--get-unpaywall mark-list))
     ((tabulated-list-get-id)
      (pubmed--get-unpaywall (tabulated-list-get-id)))
     (t
      (error "No entry selected")))))

;;;; Functions

(defun pubmed--get-unpaywall (uid)
  "Try to fetch the fulltext PDF of UID, using UNPAYWALL."
  (deferred:$
    (deferred:call #'pubmed-unpaywall uid)

    (deferred:nextc it
      (lambda (result)
	(when (bufferp result)
	  (pubmed--view-pdf result))))))

(defun pubmed-unpaywall (uid)
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
	    (deferred:timeout pubmed-unpaywall-timeout (error "Timeout")
	      (let* ((url (concat pubmed-unpaywall-url doi))
		     (parameters (list (cons "email" pubmed-unpaywall-email))))
		(if doi
		    (deferred:url-get url parameters)
		  (error "Article has no doi"))))))

	(deferred:nextc it
	  (lambda (buffer)
	    "Parse the JSON object in BUFFER. Return the url of the Open Access fulltext article or nil if none is found."
	    (let* ((json (with-current-buffer buffer (decode-coding-string (buffer-string) 'utf-8)))
		   (json-object-type 'plist)
		   (json-array-type 'list)
		   (json-key-type nil)
		   (json-object (json-read-from-string json))
		   (msg (plist-get json-object :msg))
		   (is_error (plist-get json-object :error))
		   (error_message (plist-get json-object :message))
		   (best_oa_location (plist-get json-object :best_oa_location))
		   (url_for_pdf (plist-get best_oa_location :url_for_pdf))
		   (oa_locations (plist-get json-object :oa_locations)))
	      (cond
	       ((equal is_error t)
		(signal is_error error_message))
	       (msg
		(error "Unpaywall message: %s" msg))
	       ;; Return url_for_pdf if it is found in :best_oa_location
	       (url_for_pdf
		(setq pdf-url url_for_pdf))
	       ;; Loop through oa_locations to find an url_for_pdf and return the first one found
	       (oa_locations
		(let ((i 0))
		  (while (not (or (>= i (length oa_locations))
				  (plist-get (nth i oa_locations) :url_for_pdf)))
		    (setq i (1+ i)))
		  (if (>= i (length oa_locations))
		      (error "Unpaywall found no fulltext article")
		    (setq pdf-url (plist-get (nth i oa_locations) :url_for_pdf)))))
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
		pdf-url)))))

	(deferred:nextc it
	  (lambda (url)
	    (deferred:timeout pubmed-unpaywall-timeout (error "Timeout")
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

(provide 'pubmed-unpaywall)

;;; pubmed-unpaywall.el ends here
