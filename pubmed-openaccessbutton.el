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

(defcustom pubmed-openaccessbutton-timeout 5
  "Open Access Button timeout in seconds."
  :group 'pubmed-openaccessbutton
  :type 'integer)

;;;; Commands

;;;###autoload
(defun pubmed-get-openaccessbutton (&optional uids)
  "Try to fetch the fulltext PDF articles using Open Access Button.
The entries in the optional argument UIDS are used. If no uids
are supplied, the marked entries, the entries in the active
region, or the current entry are used.

The variable `pubmed-fulltext-action' says which function to
use."
  (interactive "P")
  (if-let ((uids (or uids (pubmed-get-uids))))
      (dolist (uid uids)
        (when-let ((url (pubmed-openaccessbutton uid)))
          (funcall pubmed-fulltext-action url)))
    (error "No entry selected")))

;;;; Functions

(defun pubmed-openaccessbutton (uid)
  "Try to fetch the fulltext PDF of UID, using Open Access Button.
Return the url or nil if none is found. See URL
`https://openaccessbutton.org/api'."
  (message "Find fulltext link for UID %s using Open Access Button..." uid)
  (let* ((url (concat pubmed-openaccessbutton-url "?pmid=" uid))
         (buffer (url-retrieve-synchronously url nil nil pubmed-openaccessbutton-timeout))
         (json (with-current-buffer buffer (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8)))
	 (json-object-type 'plist)
	 (json-array-type 'list)
	 (json-key-type nil)
	 (json-object (json-read-from-string json))
	 (data (plist-get json-object :data))
	 (availability (plist-get data :availability))
	 (type (plist-get (car availability) :type))
	 (url (plist-get (car availability) :url)))
    (if (and url (equal type "article"))
        (progn
          (let* ((parsed-url (url-generic-parse-url url))
                 (path (car (url-path-and-query parsed-url))))
            ;; Remove query part
            (setf (url-filename parsed-url) path)
            (setq url (url-recreate-url parsed-url))
            (message "Find fulltext link for UID %s using Open Access Button...done" uid)
            url))
      (message "Find fulltext link for UID %s using Open Access Button...failed" uid)
      nil)))

;;;; Footer

(provide 'pubmed-openaccessbutton)

;;; pubmed-openaccessbutton.el ends here
