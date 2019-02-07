;;; pubmed-scihub.el --- Interface to PubMed -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@xs4all.nl>
;; Created: 2018-05-23
;; Version: 0.1
;; Keywords: pubmed
;; Package-Requires: ((emacs "25.1") (esxml) (pdf-tools))
;; URL: https://gitlab.com/fvdbeek/emacs-pubmed

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Download PDFs using the Sci-Hub database. You need to provide a Sci-Hub url by setting the value of SCIHUB-URL in your .emacs: (setq scihub-url "http://url-of-sci-hub.com/")

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'esxml)
(require 'esxml-query)
(require 'eww)
(require 'pdf-tools)
(require 's)
(require 'url)

;;;; Variables

(defvar scihub-url ""
  "Sci-Hub URL")

(add-hook 'pubmed-mode-hook
          (lambda ()
            (define-key pubmed-mode-map "f"
              'pubmed-get-scihub)))

;;;; Commands

(defun pubmed-get-scihub ()
  "Fetch fulltext article from Sci-Hub"
  (interactive)
  (if uid
      (pubmed--scihub uid)
    (message "No entry selected")))

;;;; Functions

(defun pubmed--scihub (uid)
  "Return buffer of Sci-Hub request.
Retrieve the response of a POST request with the UID of the currently selected PubMed entry and call `pubmed--parse-scihub' with the current buffer containing the reponse."
  ;; Sci-Hub provides academic papers for direct download. The Sci-Hub website accepts HTTP POST requests with a key-value pair, where the key is `request' and the value can be one of:
  ;; - a search string
  ;; - an URL of a scholarly article
  ;; - a DOI
  ;; - a PMID
  (interactive)
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (concat "request=" uid)))
    (url-retrieve scihub-url 'pubmed--check-scihub)))

(defun pubmed--check-scihub (status)
  "Callback function of `pubmed--scihub'. Check the STATUS and HTTP status of the response, and signal an error if an HTTP error occurred. If we are redirected, the article is Open Access and ask a WWW browser to open the URL. Call `pubmed--parse-scihub' when no error occurred and if we are not redirected."
  (let ((url-error (plist-get status :error))
	(url-redirect (plist-get status :redirect))
	(first-header-line (buffer-substring (point-min) (line-end-position))))
    (cond
     (url-error
      (signal (car url-error) (cdr url-error)))
     ((pubmed--header-error-p (pubmed--parse-header first-header-line))
      (error "HTTP error: %s" (pubmed--get-header-error (pubmed--parse-header first-header-line))))
     (url-redirect
      (message "Redirected to: %s" url-redirect)
      ;; The redirect URL is a concatenation of the SCIHUB-URL and the URL of the article source.
      (let ((buffer (generate-new-buffer "*Sci-Hub redirect*")))
	(with-current-buffer buffer
	  (browse-url (s-chop-prefix scihub-url url-redirect)))
	(switch-to-buffer buffer)))
     (t
      (pubmed--parse-scihub)))))

(defun pubmed--parse-scihub ()
  "Parse the buffer returned by `pubmed--check-scihub'.
Extract the link to the PDF and call `pubmed--scihub-iframe' with the current buffer containing the reponse."
  (let* ((headers (eww-parse-headers))
	 (content-type
	  (mail-header-parse-content-type
	   (if (zerop (length (cdr (assoc "content-type" headers))))
	       "text/plain"
	     (cdr (assoc "content-type" headers)))))
	 (dom (libxml-parse-html-region (1+ url-http-end-of-headers) (point-max))))
    (cond
     ;; If the article is not found, the HTTP response contains the message in Russian and English
     ((equal (esxml-query "p *" dom) "статья не найдена / article not found")
      (with-current-buffer
	  (generate-new-buffer "*Sci-Hub*")
       	(erase-buffer)
	(insert "Article not found")))
     ;; If the article is found, the HTTP response contains an inline frame marked up as follows:
     ;; <iframe src = "pdflink#view=FitH" id = "pdf"></iframe>
     ((esxml-node-attribute 'src (esxml-query "iframe[id=pdf]" dom))
      (let* ((url (esxml-node-attribute 'src (esxml-query "iframe[id=pdf]" dom)))
	     (parsed-url (url-generic-parse-url url)))
	;; Sometimes, the URL is not valid and misses the `http' type.
	(when (not (url-type parsed-url))
	  (setf (url-type parsed-url) "http")
	  (setq url (url-recreate-url parsed-url)))
	;; Always chop off anchors.
	(when (string-match "#.*" url)
	  (setq url (substring url 0 (match-beginning 0))))
	(message "Found article: %s" url)
	(pubmed--scihub-iframe url)))
     (t
      (with-current-buffer
	  (generate-new-buffer "*Sci-Hub*")
       	(erase-buffer)
	(shr-insert-document dom) ; render the parsed document DOM into the current buffer.
	)))))

(defun pubmed--scihub-iframe (url)
  "Retrieve URL of the iframe and call `pubmed--parse-scihub-iframe' with the current buffer containing the reponse."
  (let ((buffer (generate-new-buffer "*Sci-Hub*"))
	(inhibit-read-only t))
    (with-current-buffer buffer
      (read-only-mode)
      (erase-buffer)
      (insert (format "Loading %s..." url))
      (goto-char (point-min))
      (url-retrieve url 'pubmed--check-scihub-iframe (list url (current-buffer))))
    (save-selected-window
      (switch-to-buffer-other-frame buffer))))

(defun pubmed--check-scihub-iframe (status url buffer)
  "Callback function of `pubmed--scihub'. Check the STATUS and HTTP status of the response, and signal an error if an HTTP error occurred. Call `pubmed--parse-scihub' when no error occurred."
  (let ((url-error (plist-get status :error))
	(first-header-line (buffer-substring (point-min) (line-end-position))))
    (cond
     (url-error
      (signal (car url-error) (cdr url-error)))
     ((pubmed--header-error-p (pubmed--parse-header first-header-line))
      (error "HTTP error: %s" (pubmed--get-header-error (pubmed--parse-header first-header-line))))
     (t
      (pubmed--parse-scihub-iframe url buffer)))))

(defun pubmed--parse-scihub-iframe (url buffer)
  "Parse the buffer returned by `pubmed--scihub-frame'.
Check whether the buffer contains a PDF file or an HTML file with a captcha, and open the PDF file or show the captcha."
  (let* ((headers (eww-parse-headers))
	 (content-type
	  (mail-header-parse-content-type
           (if (zerop (length (cdr (assoc "content-type" headers))))
	       "text/plain"
             (cdr (assoc "content-type" headers))))))
    ;; The buffer contains either a PDF file or a HTML file with a captcha image.
    (cond
     ;; Extract the captcha if the buffer contains a HTML file
     ;; The captcha image is marked up as follows:
     ;; <img id="captcha" src="CAPTCHA" />
     ((eww-html-p (car content-type))
      (message "Content-type: %s" (car content-type))
      (let* ((base-url (car (shr-parse-base url)))
	     (dom (libxml-parse-html-region (1+ url-http-end-of-headers) (point-max))) ; create a DOM parse tree
	     (img-tag (esxml-query "img[id=captcha]" dom)) ; extract the img tag of the DOM parse tree
	     (captcha-url-rel (dom-attr img-tag 'src)) ; extract the image url
	     (captcha-url-abs (concat base-url captcha-url-rel))
	     (captcha-data (with-current-buffer (url-retrieve-synchronously captcha-url-abs)
	     		    (goto-char (point-min))
	     		    (search-forward "\n\n")
	     		    (buffer-substring (point) (point-max))))
	     (inhibit-read-only t))
	(with-current-buffer buffer
	  (read-only-mode)
       	  (erase-buffer)
	  (insert-image (create-image captcha-data nil t)))
	(let* ((answer (read-from-minibuffer "Captcha: "))
	       (id (esxml-node-attribute 'value (esxml-query "input[name=id]" dom)))
	       (url-request-method "POST")
	       (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
	       (url-request-data (concat "id=" id "&answer=" answer)))
	  (url-retrieve url 'pubmed--solve-captcha (list url buffer)))))
     ;; Show the PDF if the iframe contains a pdf file
     ((equal (car content-type) "application/pdf")
      (message "Content-type: %s" (car content-type))
      (let ((data (buffer-substring (1+ url-http-end-of-headers) (point-max)))
	    (inhibit-read-only t)) ; remove the HTTP headers
	(with-current-buffer buffer
	  (read-only-mode)
       	  (set-buffer-file-coding-system 'binary)
	  (erase-buffer)
	  (insert data)
      	  (pdf-view-mode))))
     (t
      (eww-display-raw buffer)))))

(defun pubmed--solve-captcha (status url buffer)
  "Callback function."
  ;; Somehow, the postback keeps returning redirect responses. The page is retrieved again with HTTP GET to avoid an endless redirect loop.
  (let ((url-request-method "GET")
	url-request-extra-headers
	url-request-data)
    (url-retrieve url 'pubmed--check-scihub-iframe (list url buffer))))

;;;; Footer

(provide 'pubmed-scihub)

;;; pubmed.el ends here
