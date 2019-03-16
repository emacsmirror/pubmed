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
;; "http://url-of-sci-hub.com/")

;; Sci-Hub doesn't provide an API, so the PDF is found by parsing the HTML. This
;; is probably more subject to change than an API, so expect this to be broken
;; easily.

;;; Code:

;;;; Requirements

(require 'pubmed)
(require 'deferred)
(require 'esxml)
(require 'esxml-query)
(require 'eww)
(require 'url)

;;;; Customization

(defgroup pubmed-scihub nil
  "Fetch fulltext PDFs from Sci-Hub."
  :group 'pubmed)

(defcustom pubmed-scihub-timeout 5000
  "Unpaywall timeout in milliseconds."
  :group 'pubmed-scihub
  :type 'integer)

(defcustom pubmed-scihub-url ""
  "Sci-Hub URL.
You need to provide a an URL to use the Sci-Hub database."
  :group 'pubmed-scihub
  :type 'string)

;;;; Commands

;;;###autoload
(defun pubmed-get-scihub (&optional entries)
  "In PubMed, try to fetch the fulltext PDF of the marked entries, the current entry or the optional argument ENTRIES."
  ;; TODO: optional argument NOQUERY non-nil means do not ask the user
  ;; to confirm. FIXME: Loading of Sci-Hub can be quite slow, so the
  ;; user is tempted to invoke `pubmed-get-scihub' multiple times if
  ;; it doesn't seem to respond immediately. Therefore, consider a
  ;; locking mechanism to prevent multiple parallel processes.
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
      (mapcar #'pubmed--get-scihub entries))
     (mark-list
      (mapcar #'pubmed--get-scihub mark-list))
     ((tabulated-list-get-id)
      (pubmed--get-scihub (tabulated-list-get-id)))
     (t
      (error "No entry selected")))))

;;;; Functions

(defun pubmed--get-scihub (uid)
  "Try to fetch the fulltext PDF of UID, using SCIHUB."
  (deferred:$
    (deferred:call #'pubmed-scihub uid)

    (deferred:nextc it
      (lambda (result)
	(when (bufferp result)
	  (pubmed--view-pdf result))))))

(defun pubmed-scihub (uid)
  "Deferred chain to retrieve the fulltext PDF of the UID."
  ;; FIXME: Every captcha and PDF opens a new frame. Consider reusing the same frame.
  (let ((iframe-url))
    (deferred:$
      ;; try
      (deferred:$

	(deferred:timeout pubmed-scihub-timeout (error "Timeout")
	  ;; Sci-Hub provides academic papers for direct download. The Sci-Hub website accepts HTTP POST requests with a key-value pair, where the key is `request' and the value can be one of:
	  ;; - a search string
	  ;; - an URL of a scholarly article
	  ;; - a DOI
	  ;; - a PMID
	  (let ((parameters (list (cons "request" uid))))
	    (deferred:url-post pubmed-scihub-url parameters)))

	(deferred:nextc it
	  (lambda (buffer)
	    "Parse the HTML in BUFFER. Return the url of the iframe or nil if none is found."
	    (let ((dom (with-current-buffer buffer (libxml-parse-html-region (point-min) (point-max)))))
	      (cond
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
    		  url))
	       (t
		(error "Sci-Hub found no fulltext article"))))))

	(deferred:nextc it
	  (lambda (url)
	    "Retrieve the URL of the iframe."
	    (setq iframe-url url)
	    (deferred:timeout pubmed-scihub-timeout (error "Timeout")
	      (deferred:url-retrieve iframe-url))))

	(deferred:nextc it
	  (deferred:lambda (buffer)
	    "Parse the HTML object in BUFFER and extract the PDF or captcha."
	    ;; The buffer contains either a PDF file or a HTML file with a captcha image.
	    (let* ((headers (with-current-buffer buffer (eww-parse-headers)))
		   (content-type (cdr (assoc "content-type" headers))))
	      ;; Extract the captcha if the buffer contains a HTML file. The captcha image is marked up as follows:
	      ;; <img id="captcha" src="CAPTCHA" />
	      (cond
	       ((equal content-type "text/html; charset=UTF-8")
		(let (captcha-id
		      captcha-url)
		  (deferred:$
		    (deferred:next
		      (lambda ()
			(let* ((base-url (car (shr-parse-base iframe-url)))
      			       (dom (with-current-buffer buffer
				      (libxml-parse-html-region (1+ url-http-end-of-headers) (point-max)))) ; create a DOM parse tree
      			       (img-tag (esxml-query "img[id=captcha]" dom)) ; extract the img tag of the DOM parse tree
      			       (rel-url (dom-attr img-tag 'src)) ; extract the image url
      			       (url (concat base-url rel-url))
			       (id (esxml-node-attribute 'value (esxml-query "input[name=id]" dom))))
			  (if (and url id)
			      ;; Retrieve captcha image.
			      (progn
				(setq captcha-id id
				      captcha-url url)
				(deferred:url-get captcha-url))
			    (error "No captcha found")))))
		    
		    (deferred:nextc it
		      (lambda (image-buffer)
			"Show captcha image in a new buffer."
			(let ((image-data (with-current-buffer image-buffer (buffer-string)))
		      	      (captcha-buffer (generate-new-buffer "*Sci-Hub Captcha*")))
			  (unwind-protect
		      	      (with-current-buffer captcha-buffer
		      		(erase-buffer)
		      		(insert-image (create-image image-data nil t))
				(read-only-mode)
		      		(switch-to-buffer-other-frame captcha-buffer))
		            (kill-buffer image-buffer)))))
		    
		    (deferred:nextc it
		      (lambda ()
			"Read the text displayed by the CAPTCHA from the minibuffer."
			;; FIXME: when the minibuffer is left, the prompt disappears so the captcha cannot be solved anymore
			(read-from-minibuffer "Captcha: ")))
		    
		    (deferred:nextc it
		      (lambda (answer)
			"Send the answer to the CAPTCHA in an HTTP POST request to IFRAME-URL"
			(deferred:timeout pubmed-scihub-timeout (error "Timeout")
			  (let ((url-request-method "POST")
				(url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
				(url-request-data (concat "id=" captcha-id "&answer=" answer)))
			    (deferred:url-retrieve iframe-url)))))

		    (deferred:nextc it
		      (lambda ()
			"After the postback, retrieve the URL again with HTTP GET."
			(deferred:timeout pubmed-scihub-timeout (error "Timeout")
	  		  (deferred:url-retrieve iframe-url))))

		    ;; Return the deferred to parse the HTML object again
		    (deferred:nextc it self))))

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

(provide 'pubmed-scihub)

;;; pubmed-scihub.el ends here
