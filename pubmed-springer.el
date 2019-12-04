;;; pubmed-springer.el --- Fetch fulltext PDFs from Springer -*-
;;; lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Download fulltext PDFs of Open Access articles using Springer
;; Nature <http://api.springernature.com/>. Using Springer Nature is
;; legal and requires you to provide an API key. To create the key,
;; register at <https://dev.springernature.com/signup>. The API key is
;; automatically generated. To get the key, click on the
;; "Applications" link in the top navigation bar after signing in or
;; navigate to <https://dev.springernature.com/admin/applications>.

;; Use the key by customizing the variable `pubmed-springer-api-key'
;; or setting the value in your .emacs: (setq pubmed-springer-api-key
;; "1234567890abcdefghijklmnopqrstuvwxyz")

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'deferred)
(require 'eww)
(require 'json)
(require 'url)

;;;; Variables

(defvar pubmed-springer-url "http://api.springernature.com/meta/v2/json"
  "Springer URL.")

;;;; Customization

(defgroup pubmed-springer nil
  "Fetch fulltext PDFs from Springer Nature."
  :group 'pubmed)

(defcustom pubmed-springer-timeout 5000
  "Springer timeout in milliseconds."
  :group 'pubmed-springer
  :type 'integer)

(defcustom pubmed-springer-api-key ""
  "Springer Nature API key.
To create the key, register at
  <https://dev.springernature.com/signup>. The API key is
  automatically generated. To get the key, click on the
  \"Applications\" link in the top navigation bar after signing
  in or navigate to
  <https://dev.springernature.com/admin/applications>."
  :link '(url-link "https://dev.springernature.com/signup")
  :group 'pubmed-springer
  :type 'string)

;;;; Commands

;;;###autoload
(defun pubmed-get-springer (&optional entries)
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
      (mapcar #'pubmed--get-springer entries))
     (mark-list
      (mapcar #'pubmed--get-springer mark-list))
     ((tabulated-list-get-id)
      (pubmed--get-springer (tabulated-list-get-id)))
     (t
      (error "No entry selected")))))

;;;; Functions

(defun pubmed--get-springer (uid)
  "Try to fetch the fulltext PDF of UID, using SPRINGER."
  (deferred:$
    (deferred:call #'pubmed-springer uid)

    (deferred:nextc it
      (lambda (result)
	(when (bufferp result)
	  (pubmed--view-pdf result))))))

(defun pubmed-springer (uid)
  "Deferred chain to retrieve the fulltext PDF of the UID."
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
	  (deferred:timeout pubmed-springer-timeout (error "Timeout")
	    (let* ((url pubmed-springer-url)
		   (parameters (list (cons "q" doi) (cons "api_key" pubmed-springer-api-key))))
              (cond
               ((and doi pubmed-springer-api-key)
		(deferred:url-get url parameters))
               ((not pubmed-springer-api-key)
                (error "Using Springer Nature requires an API key."))
               ((not doi)
		(error "Article has no doi")))))))

      (deferred:nextc it
	(lambda (buffer)
	  "Parse the JSON object in BUFFER. Return the url of the Open Access fulltext article or nil if none is found."
	  (let* ((json (with-current-buffer buffer (decode-coding-string (buffer-string) 'utf-8)))
		 (json-object-type 'plist)
		 (json-array-type 'list)
		 (json-key-type nil)
		 (json-object (json-read-from-string json))
		 (records (car (plist-get json-object :records)))
                 (openaccess (plist-get records :openaccess))
                 (urls (plist-get records :url)))
	    (cond
	     ;; Loop through urls to find an url_for_pdf and return the first one found
             ((and (equal openaccess "true") urls)
	      (let ((i 0))
                (while (not (or (>= i (length urls))
				(equal (plist-get (nth i urls) :format) "pdf")))
		  (setq i (1+ i)))
		(if (>= i (length urls))
		    (error "Springer found no fulltext article")
                  (setq pdf-url (plist-get (nth i urls) :value)))))
	     (t
	      (error "Springer found no fulltext article"))))))

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
	  (deferred:timeout pubmed-springer-timeout (error "Timeout")
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

(provide 'pubmed-springer)

;;; pubmed-springer.el ends here
