;;; pubmed-openaccessbutton.el --- Deferred functions to PubMed -*- lexical-binding: t; -*-

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

;; Download fulltext PDFs of Open Access articles using Open Access Button
;; <https://openaccessbutton.org/api>. Using Open Access Button is legal.

;; There are no enforced rate limits, but requests should be limited to a
;; maximum of one request per second. Although most API operations do not
;; require authorisation, obtaining your own API key is encouraged. To create
;; the key, register at <https://openaccessbutton.org/account?next=/api>.

;; Use the key by customizing the variable `pubmed-openaccessbutton-api-key' or
;; setting the value in your .emacs: (setq pubmed-openaccessbutton-api-key
;; "1234567890abcdefghijklmnopqrstuvwxyz")

;;; Code:

;;;; Requirements

(require 'deferred)
(require 'eww)
(require 'json)
(require 'url)

;;;; Variables

(defvar pubmed-openaccessbutton-url "https://api.openaccessbutton.org/availability/"
  "Open Access Button URL.")

;;;; Customization

(defgroup pubmed-openaccessbutton nil
  "Fetch fulltext PDFs from Open Access Button."
  :group 'pubmed)

(defcustom pubmed-openaccessbutton-api-key ""
  "Open Access Button API key.
Although Open Access Button API doesn't require authorisation, it
  is encouraged to obtain your own API key. To create the key,
  register at <https://openaccessbutton.org/account?next=/api>."
  :link
  '(url-link "https://openaccessbutton.org/account?next=/api")
  :group 'pubmed-openaccessbutton :type 'string)

(defcustom pubmed-openaccessbutton-timeout 5000
  "Open Access Button timeout in milliseconds."
  :group 'pubmed-openaccessbutton
  :type 'integer)

;;;; Commands

(defun pubmed-get-openaccessbutton (&optional entries)
  "In PubMed, try to fetch the fulltext PDF of the marked
entries, the current entry or the optional argument ENTRIES."
  ;; TODO: optional argument NOQUERY non-nil means do not ask the user to
  ;; confirm.
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
      (mapcar #'pubmed--get-openaccessbutton entries))
     (mark-list
      (mapcar #'pubmed--get-openaccessbutton mark-list))
     ((tabulated-list-get-id)
      (pubmed--get-openaccessbutton (tabulated-list-get-id)))
     (t
      (error "No entry selected")))))

;;;; Functions

(defun pubmed--get-openaccessbutton (uid)
  "Try to fetch the fulltext PDF of UID, using Open Access Button."
  (deferred:$
    (deferred:call #'pubmed-openaccessbutton uid)

    (deferred:nextc it
      (lambda (result)
	(when (bufferp result)
	  (pubmed--view-pdf result))))))

(defun pubmed-openaccessbutton (uid)
  "Deferred chain to retrieve the fulltext PDF of the UID."
  (let (pdf-url)
    (deferred:$
      ;; try
      (deferred:$
	(deferred:timeout pubmed-openaccessbutton-timeout (error "Timeout")
	  (let* ((parameters (list (cons "pmid" uid))))
	    (deferred:url-get pubmed-openaccessbutton-url parameters)))

	(deferred:nextc it
	  (lambda (buffer)
	    "Parse the JSON object in BUFFER. Return the url of
the Open Access fulltext article or nil if none is found."
	    (let* ((json (with-current-buffer buffer (decode-coding-string (buffer-string) 'utf-8)))
		   (json-object-type 'plist)
		   (json-array-type 'list)
		   (json-key-type nil)
		   (json-object (json-read-from-string json))
		   (data (plist-get json-object :data))
		   (availability (plist-get data :availability))
		   (type (plist-get (car availability) :type))
		   (url (plist-get (car availability) :url)))
	      (cond
	       ;; Workaround of bug in Open Access Button service, where any
	       ;; article that isn't available returns the same wrong url
	       ((and (equal type "article") (equal url "https://core.ac.uk/download/pdf/38142439.pdf"))
		(error "Open Access Button found no fulltext article"))
	       ((and url (equal type "article"))
		(setq pdf-url url))
	       (t
		(error "Open Access Button found no fulltext article"))))))

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
	    (deferred:timeout pubmed-openaccessbutton-timeout (error "Timeout")
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

(provide 'pubmed-openaccessbutton)

;;; pubmed-openaccessbutton.el ends here
