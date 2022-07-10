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
(require 'json)
(require 'seq)
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

(defcustom pubmed-unpaywall-timeout 5
  "Unpaywall timeout in seconds."
  :group 'pubmed-unpaywall
  :type 'integer)

;;;; Commands

;;;###autoload
(defun pubmed-get-unpaywall (&optional uids)
  "Try to fetch the fulltext PDF articles using Unpaywall.
The entries in the optional argument UIDS are used. If no uids
are supplied, the marked entries, the entries in the active
region, or the current entry are used.

The variable `pubmed-fulltext-action' says which function to
use."
  (interactive "P")
  (if-let ((uids (or uids (pubmed-get-uids))))
      (dolist (uid uids)
        (when-let ((url (pubmed-unpaywall uid)))
          (funcall pubmed-fulltext-action url)))
    (error "No entry selected")))

;;;; Functions

(defun pubmed-unpaywall (uid)
  "Try to fetch the fulltext PDF of UID, using Unpaywall.
Return the url or nil if none is found. See URL
`https://unpaywall.org/products/api'."
  (message "Find fulltext link for UID %s using Unpaywall..." uid)
  (unless pubmed-unpaywall-email
    (error "Using Unpaywall requires an email address"))
  (catch 'result
    (let* ((entry (seq-find (lambda (entry) (equal (plist-get entry :uid) uid)) pubmed-entries))
	   (articleids (plist-get entry :articleids))
           (articleid (seq-find (lambda (articleid) (equal (plist-get articleid :idtype) "doi")) articleids))
           (doi (plist-get articleid :value)))
      (unless doi
        (message "Find fulltext link for UID %s using Unpaywall...failed" uid)
        (throw 'result nil))
      (let* ((arguments (concat "?"
                                "email=" pubmed-unpaywall-email))
	     (url (concat pubmed-unpaywall-url doi arguments))
             (buffer (url-retrieve-synchronously url nil nil pubmed-unpaywall-timeout))
             (json (with-current-buffer buffer (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8)))
	     (json-object-type 'plist)
	     (json-array-type 'list)
	     (json-key-type nil)
	     (json-object (json-read-from-string json))
	     (best-oa-location (plist-get json-object :best_oa_location))
	     (oa-locations (plist-get json-object :oa_locations))
	     (has-pdf-p (lambda (oa-location) (plist-get oa-location :url_for_pdf)))
             url)
        (if (funcall has-pdf-p best-oa-location)
            ;; Return url_for_pdf if it is found in :best_oa_location
            (setq url (plist-get best-oa-location :url_for_pdf))
          ;; Else loop through oa_locations to find an url_for_pdf and return the first one found
          (setq url (thread-first
                        (seq-find has-pdf-p oa-locations)
                      (plist-get :url_for_pdf))))
        (if url
            (progn
              (message "Find fulltext link for UID %s using Unpaywall...done" uid)
              (throw 'result url))
          (message "Find fulltext link for UID %s using Unpaywall...failed" uid)
          (throw 'result nil))))))

;;;; Footer

(provide 'pubmed-unpaywall)

;;; pubmed-unpaywall.el ends here
