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
;; <https://dissemin.readthedocs.io/en/latest/api.html>. Dissemin gathers
;; metadata from multiple sources and links to full text publications in
;; third-party repositories (Zenodo <https://zenodo.org/>, HAL
;; <https://hal.archives-ouvertes.fr/>, and OSF preprints
;; <https://osf.io/preprints/>). Using Dissemin is legal.

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'json)
(require 'seq)
(require 'url)

;;;; Variables

(defvar pubmed-dissemin-url "https://dissem.in/api/"
  "Dissemin URL.")

;;;; Customization

(defgroup pubmed-dissemin nil
  "Fetch fulltext PDFs from Dissemin."
  :group 'pubmed)

(defcustom pubmed-dissemin-timeout 5
  "Dissemin timeout in seconds."
  :group 'pubmed-dissemin
  :type 'integer)

;;;; Commands

;;;###autoload
(defun pubmed-get-dissemin (&optional uids)
  "Try to fetch the fulltext PDF articles using Dissemin.
The entries in the optional argument UIDS are used. If no uids
are supplied, the marked entries, the entries in the active
region, or the current entry are used.

The variable `pubmed-fulltext-action' says which function to
use."
  (interactive "P")
  (if-let ((uids (or uids (pubmed-get-uids))))
      (dolist (uid uids)
        (when-let ((url (pubmed-dissemin uid)))
          (funcall pubmed-fulltext-action url)))
    (error "No entry selected")))

;;;; Functions

(defun pubmed-dissemin (uid)
  "Try to fetch the fulltext PDF of UID, using Dissemin.
Return the url or nil if none is found. See URL
`https://dissemin.readthedocs.io/en/latest/api.html'."
  (message "Find fulltext link for UID %s using Dissemin..." uid)
  (catch 'result
    (let* ((entry (seq-find (lambda (entry) (equal (plist-get entry :uid) uid)) pubmed-entries))
	   (articleids (plist-get entry :articleids))
           (articleid (seq-find (lambda (articleid) (equal (plist-get articleid :idtype) "doi")) articleids))
           (doi (plist-get articleid :value)))
      (unless doi
        (message "Find fulltext link for UID %s using Dissemin...failed" uid)
        (throw 'result nil))
      (let* ((url (concat pubmed-dissemin-url doi))
             (buffer (url-retrieve-synchronously url nil nil pubmed-dissemin-timeout))
             (json (with-current-buffer buffer (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8)))
	     (json-object-type 'plist)
	     (json-array-type 'list)
	     (json-key-type nil)
	     (json-object (json-read-from-string json))
             (status (plist-get json-object :status))
             (paper (plist-get json-object :paper))
             (records (plist-get paper :records))
             (has-pdf-p (lambda (record) (not (string-empty-p (plist-get record :pdf_url)))))
             (url (thread-first
                      (seq-find has-pdf-p records)
                    (plist-get :pdf_url))))
        (if url
            (progn
              (message "Find fulltext link for UID %s using Dissemin...done" uid)
              (throw 'result url)))
        (message "Find fulltext link for UID %s using Dissemin...failed" uid)
        (throw 'result nil)))))

;;;; Footer

(provide 'pubmed-dissemin)

;;; pubmed-dissemin.el ends here
