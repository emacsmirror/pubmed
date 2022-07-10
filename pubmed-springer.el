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
(require 'json)
(require 'seq)
(require 'url)

;;;; Variables

(defvar pubmed-springer-url "http://api.springernature.com/meta/v2/json"
  "Springer URL.")

;;;; Customization

(defgroup pubmed-springer nil
  "Fetch fulltext PDFs from Springer Nature."
  :group 'pubmed)

(defcustom pubmed-springer-timeout 5
  "Springer timeout in seconds."
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
(defun pubmed-get-springer (&optional uids)
  "Try to fetch the fulltext PDF articles using  Springer Nature.
The entries in the optional argument UIDS are used. If no uids
are supplied, the marked entries, the entries in the active
region, or the current entry are used.

The variable `pubmed-fulltext-action' says which function to
use."
  (interactive "P")
  (if-let ((uids (or uids (pubmed-get-uids))))
      (dolist (uid uids)
        (when-let ((url (pubmed-springer uid)))
          (save-current-buffer
            (funcall pubmed-fulltext-action url))))
    (error "No entry selected")))

;;;; Functions

(defun pubmed-springer (uid)
  "Try to fetch the fulltext PDF of UID, using Springer Nature.
Return the url or nil if none is found. See URL
`https://dev.springernature.com/'."
  (message "Find fulltext link for UID %s using Springer Nature..." uid)
  (unless pubmed-springer-api-key
    (error "Using Springer Nature requires an API key"))
  (catch 'result
    (let* ((entry (seq-find (lambda (entry) (equal (plist-get entry :uid) uid)) pubmed-entries))
	   (articleids (plist-get entry :articleids))
           (articleid (seq-find (lambda (articleid) (equal (plist-get articleid :idtype) "doi")) articleids))
           (doi (plist-get articleid :value)))
      (unless doi
        (message "Find fulltext link for UID %s using Springer Nature...failed" uid)
        (throw 'result nil))
      (let* ((arguments (concat "?"
			        "q=" doi
			        "&api_key=" pubmed-springer-api-key))
             (url (concat pubmed-springer-url arguments))
             (buffer (url-retrieve-synchronously url nil nil pubmed-springer-timeout))
             (json (with-current-buffer buffer (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8)))
	     (json-object-type 'plist)
	     (json-array-type 'list)
	     (json-key-type nil)
	     (json-object (json-read-from-string json))
	     (records (plist-get json-object :records))
             (record (car records))
             (openaccess (plist-get record :openaccess))
             (urls (plist-get record :url))
             (has-pdf-p (lambda (url) (equal (plist-get url :format) "pdf")))
             (url (when (and (equal openaccess "true") urls)
                    (thread-first
                        (seq-find has-pdf-p urls)
                      (plist-get :value)))))
        (if url
            (progn
              (message "Find fulltext link for UID %s using Springer Nature...done" uid)
              (throw 'result url))
          (message "Find fulltext link for UID %s using Springer Nature...failed" uid)
          (throw 'result nil))))))

;;;; Footer

(provide 'pubmed-springer)

;;; pubmed-springer.el ends here
