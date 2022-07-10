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

(require 'esxml)
(require 'esxml-query)
(require 'url)

;;;; Variables
(defvar pubmed-pmc-url "https://www.ncbi.nlm.nih.gov/pmc/utils/oa/oa.fcgi"
  "PMC base URL.")

;;;; Customization

(defgroup pubmed-pmc nil
  "Fetch fulltext PDFs from PubMed Central (PMC)."
  :group 'pubmed)

(defcustom pubmed-pmc-timeout 5
  "PMC timeout in seconds."
  :group 'pubmed-pmc
  :type 'integer)

;;;; Commands

;;;###autoload
(defun pubmed-get-pmc (&optional uids)
  "Try to fetch the fulltext PDF articles using using PubMed Central (PMC).
The entries in the optional argument UIDS are used. If no uids
are supplied, the marked entries, the entries in the active
region, or the current entry are used.

The variable `pubmed-fulltext-action' says which function to
use."
  (interactive "P")
  (if-let ((uids (or uids (pubmed-get-uids))))
      (dolist (uid uids)
        (when-let ((url (pubmed-pmc uid)))
          (funcall pubmed-fulltext-action url)))
    (error "No entry selected")))

;;;; Functions

(defun pubmed-pmc (uid)
  "Try to fetch the fulltext PDF of UID, using PubMed Central (PMC).
Return the url or nil if none is found. See URL
`https://www.ncbi.nlm.nih.gov/pmc/tools/oa-service/'."
  (message "Find fulltext link for UID %s using PMC..." uid)
  (if-let ((entry (seq-find (lambda (entry) (equal (plist-get entry :uid) uid)) pubmed-entries))
           (articleids (plist-get entry :articleids))
           (articleid (seq-find (lambda (articleid) (equal (plist-get articleid :idtype) "pmc")) articleids))
           (pmc (plist-get articleid :value))
           (url-request-method "GET")
	   (arguments (concat "?"
                              "&id=" pmc
			      (when (not (string-empty-p pubmed-api-key))
			        (concat "&api_key=" pubmed-api-key))))
           (url (concat pubmed-pmc-url arguments))
           (buffer (url-retrieve-synchronously url nil nil pubmed-pmc-timeout))
           (dom (with-current-buffer buffer (libxml-parse-html-region (point-min) (point-max))))
           (url (esxml-node-attribute 'href (esxml-query "link[format=pdf]" dom)))
           (parsed-url (url-generic-parse-url url)))
      (progn
        ;; The PDF articles are available for download with FTP, but can
        ;; be retrieved with HTTP as well.
        (when (equal (url-type parsed-url) "ftp")
          (setf (url-type parsed-url) "https")
          (setq url (url-recreate-url parsed-url)))
        (message "Find fulltext link for UID %s using PMC...done" uid)
        url)
    (message "Find fulltext link for UID %s using PMC...failed" uid)
    nil))

;;;; Footer

(provide 'pubmed-pmc)

;;; pubmed-pmc.el ends here
