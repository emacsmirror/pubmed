;;; pubmed-scihub.el --- Fetch fulltext PDFs from Sci-Hub -*- lexical-binding: t; -*-

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

;; Download fulltext PDFs of articles using the Sci-Hub database. You need to
;; provide a Sci-Hub URL by customizing the variable `pubmed-scihub-url' or
;; setting the value in your .init.el or .emacs file: (setq pubmed-scihub-url
;; "https://url-of-sci-hub.com/")

;; Sci-Hub doesn't provide an API, so the PDF is found by parsing the HTML. This
;; is probably more subject to change than an API, so expect this to be broken
;; easily.

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'esxml)
(require 'esxml-query)
(require 'url)

;;;; Customization

(defgroup pubmed-scihub nil
  "Fetch fulltext PDFs from Sci-Hub."
  :group 'pubmed)

(defcustom pubmed-scihub-timeout 5
  "Sci-Hub timeout in seconds."
  :group 'pubmed-scihub
  :type 'integer)

(defcustom pubmed-scihub-url ""
  "Sci-Hub URL.
You need to provide a an URL to use the Sci-Hub database."
  :group 'pubmed-scihub
  :type 'string)

;;;; Commands

;;;###autoload
(defun pubmed-get-scihub (&optional uids)
  "Try to fetch the fulltext PDF articles using Sci-Hub.
The entries in the optional argument UIDS are used. If no uids
are supplied, the marked entries, the entries in the active
region, or the current entry are used.

The variable `pubmed-fulltext-action' says which function to
use."
  (interactive "P")
  (if-let ((uids (or uids (pubmed-get-uids))))
      (dolist (uid uids)
        (when-let ((url (pubmed-scihub uid)))
          (save-current-buffer
            (funcall pubmed-fulltext-action url))))
    (error "No entry selected")))

;;;; Functions

(defun pubmed-scihub (uid)
  "Try to fetch the fulltext PDF of UID, using Sci-Hub.
Return the url or nil if none is found. See URL
`https://en.wikipedia.org/wiki/Sci-Hub'."
  (message "Find fulltext link for UID %s using Sci-Hub..." uid)
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (concat "request=" uid))
         (buffer (url-retrieve-synchronously pubmed-scihub-url nil nil pubmed-scihub-timeout))
         (dom (with-current-buffer buffer (libxml-parse-html-region (1+ url-http-end-of-headers) (point-max))))
         (node (esxml-query "div[id=article] embed[id=pdf]" dom)))
    (if node
        (let* ((url (esxml-node-attribute 'src node))
               (parsed-url (url-generic-parse-url url))
               (parsed-base (url-generic-parse-url pubmed-scihub-url)))
          ;; Make relative urls absolute
          (unless (url-type parsed-url)
            (setf (url-type parsed-url) (url-type parsed-base)))
          (unless (url-host parsed-url)
            (setf (url-host parsed-url) (url-host parsed-base)
                  (url-fullness parsed-url) t))
          (unless (string-prefix-p "/" (url-filename parsed-url))
            (setf (url-filename parsed-url) (concat (url-filename parsed-base) (url-filename parsed-url))))
          ;; Chop off anchors
          (setf (url-target parsed-url) nil)
          (setq url (url-recreate-url parsed-url))
          (message "Find fulltext link for UID %s using Sci-Hub...done" uid)
          url)
      (message "Find fulltext link for UID %s using Sci-Hub...failed" uid)
      nil)))

;;;; Footer

(provide 'pubmed-scihub)

;;; pubmed-scihub.el ends here
