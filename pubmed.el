;;; pubmed.el --- Interface to PubMed -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@xs4all.nl>
;; Created: 2018-05-23
;; Version: 0.1
;; Keywords: pubmed
;; Package-Requires: ((emacs "25.1") (esxml) (s "1.10"))
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

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a GNU Emacs interface to the PubMed database of references on life sciences and biomedical topics.

;; Since May 1, 2018, NCBI limits access to the E-utilities unless you have an API key. See <https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/>. If you don't have an API key, E-utilities will still work, but you may be limited to fewer requests than allowed with an API key. Any computer (IP address) that submits more than three E-utility requests per second will receive an error message. This limit applies to any combination of requests to EInfo, ESearch, ESummary, EFetch, ELink, EPost, ESpell, and EGquery. 

;; First, you will need an NCBI account. If you don't have one already, register at <https://www.ncbi.nlm.nih.gov/account/>.

;; To create the key, go to the "Settings" page of your NCBI account. (Hint: after signing in, simply click on your NCBI username in the upper right corner of any NCBI page.) You'll see a new "API Key Management" area. Click the "Create an API Key" button, and copy the resulting key.

;; Use the key by setting the value of PUBMED-API-KEY in your .emacs: (setq pubmed-api-key "1234567890abcdefghijklmnopqrstuvwxyz")

;;; Code:

;;;; Requirements

(require 'pubmed-bibtex)
(require 'pubmed-completion-at-point)
(require 'pubmed-pmc)
(require 'esxml)
(require 'esxml-query)
(require 'eww)
(require 'json)
(require 's)
(require 'url)

;;;; Variables
(defvar pubmed-search-completion t
  "When non-NIL use completion using PubMed suggestions.")

(defvar pubmed-history-list nil
  "The PubMed history list.")

(defvar pubmed-api-key ""
  "E-utilities API key.")

(defvar pubmed-limit-with-api-key 10
  "Maximum amount of E-utilities requests/second with API key.")

(defvar pubmed-limit-without-api-key 3
  "Maximum amount of E-utilities requests/second without API key.")

(defvar pubmed-efetch-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
  "EFetch base URL.")

(defvar pubmed-esearch-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  "ESearch base URL.")

(defvar pubmed-espell-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/espell.fcgi"
  "ESpell base URL.")

(defvar pubmed-esummary-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
  "ESummary base URL.")

(defvar pubmed-db "pubmed"
  "Entrez database.")

(defvar pubmed-usehistory "y"
  "Store search results on the Entrez history server for later use.")

;; Integer query key returned by a previous ESearch call. When provided, ESearch will find the intersection of the set specified by query_key and the set retrieved by the query in term (i.e. joins the two with AND). For query_key to function, WebEnv must be assigned an existing WebEnv string and PUBMED-USE-HISTORY must be set to "y".
(defvar pubmed-query_key ""
  "QueryKey, only present if PUBMED-USEHISTORY is \"y\".")

;; Web environment string returned from a previous ESearch, EPost or ELink call. When provided, ESearch will post the results of the search operation to this pre-existing WebEnv, thereby appending the results to the existing environment. In addition, providing WebEnv allows query keys to be used in term so that previous search sets can be combined or limited. As described above, if WebEnv is used, usehistory must be set to "y".
(defvar pubmed-webenv ""
  "WebEnv; only present if PUBMED-USEHISTORY is \"y\".")

;; Sequential index of the first record to be retrieved (default=0, corresponding to the first record of the entire set). This parameter can be used in conjunction with retmax to download an arbitrary subset of records from the input set.
(defvar pubmed-retstart 0
  "Sequential index of the first UID; default=0.")

;; Total number of UIDs from the retrieved set to be shown in the output. If PUBMED-USEHISTORY is set to "y", the remainder of the retrieved set will be stored on the History server; otherwise these UIDs are lost. Increasing retmax allows more of the retrieved UIDs to be included in the output, up to a maximum of 100,000 records. To retrieve more than 100,000 UIDs, submit multiple esearch requests while incrementing the value of retstart.
(defvar pubmed-retmax 500
  "Number of UIDs returned; default=500.")

;; Specifies the method used to sort UIDs in the ESearch output. If PUBMED-USEHISTORY is set to "y", the UIDs are loaded onto the History Server in the specified sort order and will be retrieved in that order by ESummary or EFetch. For PubMed, the default sort order is "most+recent". Valid sort values include:
;; "journal": Records are sorted alphabetically by journal title, and then by publication date.
;; "pub+date": Records are sorted chronologically by publication date (with most recent first), and then alphabetically by journal title.
;; "most+recent": Records are sorted chronologically by date added to PubMed (with the most recent additions first).
;; "relevance": Records are sorted based on relevance to your search. For more information about PubMed's relevance ranking, see the PubMed Help section on Computation of Weighted Relevance Order in PubMed.
;; "title": Records are sorted alphabetically by article title.
;; "author": Records are sorted alphabetically by author name, and then by publication date.
(defvar pubmed-sort ""
  "Method used to sort UIDs in the ESearch output.")

;; There are two allowed values for ESearch: "uilist" (default), which displays the standard output, and "count", which displays only the <Count> tag.
(defvar pubmed-rettype "uilist"
  "Retrieval type.")

;; Determines the format of the returned output. The default value for the E-Utilities is "xml" for ESearch XML, but "json" is also supported to return output in JSON format. Emacs-pubmed uses JSON for Esearch and Esummary calls, but XML for EFetch calls because it doesn't support JSON.
(defvar pubmed-retmode "json"
  "Retrieval mode.")

(defvar pubmed-time-format-string "%Y-%m-%d"
  "The format-string that is used by `format-time-string' to convert time values. Default is the ISO 8601 date format, i.e., \"%Y-%m-%d\".")

(defvar pubmed-entries nil
  "The plist of citations retrieved by the last PubMed search.")

(defvar pubmed-uid nil
  "The UID of the entry currently selected in the PubMed buffer.")

(defvar pubmed-entry-timer nil
  "The timer that is set to delay EFetch calls.")

(defvar pubmed-entry-delay 0.5
  "Delay in seconds before fetching the PubMed entry; default=0.5. Seconds may be an integer or floating point number. Purpose of the delay is to prevent frequent EFetch calls and exceeding the E-utilities rate limit when walking fast through the PubMed entries.")

(defvar pubmed-months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "Abbreviated months.")

(defvar pubmed-fulltext-functions '(pubmed-pmc)
  "The list of functions tried in order by `pubmed-fulltext' to fetch fulltext articles. To change the behavior of ‘pubmed-get-fulltext’, remove, change the order of, or insert functions in this list.")

(defvar pubmed-temp-prefix "pubmed"
  "Prefix for temporary files created by pubmed.")

;;;; Keymap

(defvar pubmed-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "a") 'pubmed-append-bibtex)
    (define-key map (kbd "RET") 'pubmed-show-current-entry)
    (define-key map (kbd "g") 'pubmed-get-fulltext)
    (define-key map (kbd "m") 'pubmed-mark)
    (define-key map (kbd "n") 'pubmed-show-next)
    (define-key map (kbd "p") 'pubmed-show-prev)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "s") 'pubmed-search)
    (define-key map (kbd "u") 'pubmed-unmark)
    (define-key map (kbd "U") 'pubmed-unmark-all)
    (define-key map (kbd "w") 'pubmed-write-bibtex)
    (define-key map (kbd "TAB") 'pubmed-show-bibtex)
    map)
  "Local keymap for `pubmed-mode'.")

(defvar pubmed-search-mode-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "TAB") 'completion-at-point)
    map)
  "Local keymap for `pubmed-search-mode'.")

;;;; Mode

(define-derived-mode pubmed-mode tabulated-list-mode "pubmed"
  "Major mode for PubMed."
  :group 'pubmed
  (setq tabulated-list-format [("Author" 15 t)
                               ("Title"  75 t)
                               ("Journal" 30 t)
			       ("Pubdate" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

;;;;; Commands
(defun pubmed-show-mode ()
  "Mode for displaying PubMed entries."
  (interactive)
  (use-local-map pubmed-mode-map)
  (setq major-mode 'pubmed-show-mode
        mode-name "pubmed-show"
        buffer-read-only t)
  (buffer-disable-undo))

(defun pubmed-search (query)
  "Search PubMed with QUERY."
  (interactive
   (let* ((minibuffer-setup-hook (lambda () (add-hook 'completion-at-point-functions 'pubmed-completion-at-point nil t)))
	  (query (read-from-minibuffer "Query: " nil pubmed-search-mode-map nil 'pubmed-history-list)))
     (list query)))
  (setq pubmed-entries nil)
  (pubmed--esearch query))

(defun pubmed-show-entry (uid)
  "Display ENTRY in the current buffer."
  (interactive)
  ;; "Return the parsed summary for an UID"
  ;; TODO: Only the summary of the first UID is returned. Consider returning multiple summaries at once when multiple UIDs are passed as argument.
  (let ((url-request-method "POST")
	(url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (concat "db=" pubmed-db
				  "&retmode=xml"
				  "&rettype=abstract"
				  "&id=" uid
				  (when (not (string-empty-p pubmed-api-key))
				    (concat "&api_key=" pubmed-api-key)))))
    (url-retrieve pubmed-efetch-url 'pubmed--parse-efetch)))

(defun pubmed-show-current-entry ()
  "Show the current entry in the \"pubmed-show\" buffer."
  (interactive)
  (setq pubmed-uid (tabulated-list-get-id))
  (when (timerp pubmed-entry-timer)
    (cancel-timer pubmed-entry-timer))
  (setq pubmed-entry-timer (run-with-timer pubmed-entry-delay nil 'pubmed-show-entry pubmed-uid)))

(defun pubmed-show-next ()
  "Show the next item in the \"pubmed-show\" buffer."
  (interactive)
  (with-current-buffer "*PubMed*"
    (forward-line)
    (setq pubmed-uid (tabulated-list-get-id))
    (when (get-buffer-window "*PubMed-entry*" "visible")
      (when (timerp pubmed-entry-timer)
	(cancel-timer pubmed-entry-timer))
      (setq pubmed-entry-timer (run-with-timer pubmed-entry-delay nil 'pubmed-show-entry pubmed-uid)))))

(defun pubmed-show-prev ()
  "Show the previous entry in the \"pubmed-show\" buffer."
  (interactive)
  (with-current-buffer "*PubMed*"
    (forward-line -1)
    (setq pubmed-uid (tabulated-list-get-id))
    (when (get-buffer-window "*PubMed-entry*" "visible")
      (when (timerp pubmed-entry-timer)
	(cancel-timer pubmed-entry-timer))
      (setq pubmed-entry-timer (run-with-timer pubmed-entry-delay nil 'pubmed-show-entry pubmed-uid)))))

(defun pubmed-mark (&optional _num)
  "Mark an entry and move to the next line."
  (interactive "p")
  (tabulated-list-put-tag "*" t))

(defun pubmed-unmark (&optional _num)
  "Unmark an entry and move to the next line."
  (interactive "p")
  (tabulated-list-put-tag " " t))

(defun pubmed-mark-all (&optional _num)
  "Unmark all entries."
  ;; TODO: mark all entries in active region
  (interactive "p")
  (pubmed--guard)
  (let (mark-list
	pubmed-uid)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(setq pubmed-uid (tabulated-list-get-id))
        (push pubmed-uid mark-list)
        (tabulated-list-put-tag "*" t)))))

(defun pubmed-unmark-all (&optional _num)
  "Unmark all entries."
  (interactive "p")
  (pubmed--guard)
  (let (mark-list)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(tabulated-list-put-tag " " t)))
    (setq mark-list nil)))

(defun pubmed-get-fulltext (&optional entries)
  "Try to fetch the fulltext PDF of the marked entries or current entry."
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
     (mark-list
      (mapcar 'pubmed--fulltext mark-list))
     ((tabulated-list-get-id)
      (pubmed--fulltext (tabulated-list-get-id)))
     (t
      (error "No entry selected")))))

(defun pubmed--fulltext (uid)
  "Try to fetch the fulltext PDF of UID, using multiple methods. The functions in `pubmed-fulltext-functions' are tried in order, until a fulltext PDF is found."
  (let ((i 0))
    (deferred:$
      (deferred:next
	(deferred:lambda ()
	  (cond
	   ((eq 0 (length pubmed-fulltext-functions))
	    (error "No functions in the list `pubmed-fulltext-functions'"))
	   ((>= i (length pubmed-fulltext-functions))
	    (error "No fulltext PDF found"))
	   ((< i (length pubmed-fulltext-functions))
	    (progn
	      (message "Trying %S..." (nth i pubmed-fulltext-functions))
	      
	      (deferred:$
		(deferred:call (nth i pubmed-fulltext-functions) uid)

		(deferred:nextc it
		  (lambda (result)
		    (setq i (1+ i))
		    (if result
			(when (bufferp result)
			  (pubmed--view-pdf result))
		      (deferred:next self)))))))))))))

;;;; Functions

(defun pubmed--guard ()
  (unless (eq major-mode 'pubmed-mode)
    (error "The current buffer is not in PubMed mode")))

(defun pubmed--get-url-error (status)
  "Given a HTTP STATUS code, return a human readable response. Return nil if none is found."
  (case status
    ;; 1xx Informational response
    ((100) "Continue")
    ((101) "Switching Protocols")
    ((102) "Processing (WebDAV; RFC 2518)")
    ((103) "Early Hints (RFC 8297)")
    ;; 2xx Success
    ((200) "OK") ;; Standard response for successful HTTP requests
    ((201) "Created")
    ((202) "Accepted")
    ((203) "Non-Authoritative Information (since HTTP/1.1)")
    ((204) "No Content")
    ((205) "Reset Content")
    ((206) "Partial Content (RFC 7233)")
    ((207) "Multi-Status (WebDAV; RFC 4918)")
    ((208) "Already Reported (WebDAV; RFC 5842)")
    ((226) "IM Used (RFC 3229)")
    ;; 3xx Redirection
    ((300) "Multiple Choices")
    ((301) "Moved Permanently")
    ((302) "Found")
    ((303) "See Other (since HTTP/1.1)")
    ((304) "Not Modified (RFC 7232)")
    ((305) "Use Proxy (since HTTP/1.1)")
    ((306) "Switch Proxy")
    ((307) "Temporary Redirect (since HTTP/1.1)")
    ((308) "Permanent Redirect (RFC 7538)")
    ;; 4xx Client errors
    ((400) "Bad Request")
    ((401) "Unauthorized (RFC 7235)")
    ((402) "Payment Required")
    ((403) "Forbidden")
    ((404) "Not Found")
    ((405) "Method Not Allowed")
    ((406) "Not Acceptable")
    ((407) "Proxy Authentication Required (RFC 7235)")
    ((408) "Request Timeout")
    ((409) "Conflict")
    ((410) "Gone")
    ((411) "Length Required")
    ((412) "Precondition Failed (RFC 7232)")
    ((413) "Payload Too Large (RFC 7231)")
    ((414) "URI Too Long (RFC 7231)")
    ((415) "Unsupported Media Type")
    ((416) "Range Not Satisfiable (RFC 7233)")
    ((417) "Expectation Failed")
    ((418) "I'm a teapot (RFC 2324, RFC 7168)")
    ((421) "Misdirected Request (RFC 7540)")
    ((422) "Unprocessable Entity (WebDAV; RFC 4918)")
    ((423) "Locked (WebDAV; RFC 4918)")
    ((424) "Failed Dependency (WebDAV; RFC 4918)")
    ((426) "Upgrade Required")
    ((428) "Precondition Required (RFC 6585)")
    ((429) "Too Many Requests (RFC 6585)")
    ((431) "Request Header Fields Too Large (RFC 6585)")
    ((451) "Unavailable For Legal Reasons (RFC 7725)")
    ;; 5xx Server errors
    ((500) "Internal Server Error")
    ((501) "Not Implemented")
    ((502) "Bad Gateway")
    ((503) "Service Unavailable")
    ((504) "Gateway Timeout")
    ((505) "HTTP Version Not Supported")
    ((506) "Variant Also Negotiates (RFC 2295)")
    ((507) "Insufficient Storage (WebDAV; RFC 4918)")
    ((508) "Loop Detected (WebDAV; RFC 5842)")
    ((510) "Not Extended (RFC 2774)")
    ((511) "Network Authentication Required (RFC 6585)")))

(defun pubmed--parse-time-string (time-string)
  "Convert TIME-STRING to a string formatted according to PUBMED-TIME-FORMAT-STRING. TIME-STRING should be formatted as \"yyyy/mm/dd HH:MM\"."
  (let* ((regexp "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\) \\([0-9][0-9]\\):\\([0-9][0-9]\\)")
	 (parsed-time (string-match regexp time-string))
	 (sec 0)
	 (min (string-to-number (match-string 5 time-string)))
	 (hour (string-to-number (match-string 4 time-string)))
	 (day (string-to-number (match-string 3 time-string)))
	 (mon (string-to-number (match-string 2 time-string)))
	 (year (string-to-number (match-string 1 time-string)))
	 (encoded-time (encode-time sec min hour day mon year)))
    (format-time-string pubmed-time-format-string encoded-time)))

(defun pubmed--html-to-unicode (string)
  (let* ((html-entities '(Aacute "Á" aacute "á" Acirc "Â" acirc "â" acute "´" AElig "Æ" aelig "æ" Agrave "À" agrave "à" alefsym "ℵ" Alpha "Α" alpha "α" amp "&" and "∧" ang "∠" apos "'" aring "å" Aring "Å" asymp "≈" atilde "ã" Atilde "Ã" auml "ä" Auml "Ä" bdquo "„" Beta "Β" beta "β" brvbar "¦" bull "•" cap "∩" ccedil "ç" Ccedil "Ç" cedil "¸" cent "¢" Chi "Χ" chi "χ" circ "ˆ" clubs "♣" cong "≅" copy "©" crarr "↵" cup "∪" curren "¤" Dagger "‡" dagger "†" darr "↓" dArr "⇓" deg "°" Delta "Δ" delta "δ" diams "♦" divide "÷" eacute "é" Eacute "É" ecirc "ê" Ecirc "Ê" egrave "è" Egrave "È" empty "∅" emsp " " ensp " " Epsilon "Ε" epsilon "ε" equiv "≡" Eta "Η" eta "η" eth "ð" ETH "Ð" euml "ë" Euml "Ë" euro "€" exist "∃" fnof "ƒ" forall "∀" frac12 "½" frac14 "¼" frac34 "¾" frasl "⁄" Gamma "Γ" gamma "γ" ge "≥" gt ">" harr "↔" hArr "⇔" hearts "♥" hellip "…" iacute "í" Iacute "Í" icirc "î" Icirc "Î" iexcl "¡" igrave "ì" Igrave "Ì" image "ℑ" infin "∞" int "∫" Iota "Ι" iota "ι" iquest "¿" isin "∈" iuml "ï" Iuml "Ï" Kappa "Κ" kappa "κ" Lambda "Λ" lambda "λ" lang "〈" laquo "«" larr "←" lArr "⇐" lceil "⌈" ldquo "“" le "≤" lfloor "⌊" lowast "∗" loz "◊" lrm "" lsaquo "‹" lsquo "‘" lt "<" macr "¯" mdash "—" micro "µ" middot "·" minus "−" Mu "Μ" mu "μ" nabla "∇" nbsp "" ndash "–" ne "≠" ni "∋" not "¬" notin "∉" nsub "⊄" ntilde "ñ" Ntilde "Ñ" Nu "Ν" nu "ν" oacute "ó" Oacute "Ó" ocirc "ô" Ocirc "Ô" OElig "Œ" oelig "œ" ograve "ò" Ograve "Ò" oline "‾" omega "ω" Omega "Ω" Omicron "Ο" omicron "ο" oplus "⊕" or "∨" ordf "ª" ordm "º" oslash "ø" Oslash "Ø" otilde "õ" Otilde "Õ" otimes "⊗" ouml "ö" Ouml "Ö" para "¶" part "∂" permil "‰" perp "⊥" Phi "Φ" phi "φ" Pi "Π" pi "π" piv "ϖ" plusmn "±" pound "£" Prime "″" prime "′" prod "∏" prop "∝" Psi "Ψ" psi "ψ" quot "\"" radic "√" rang "〉" raquo "»" rarr "→" rArr "⇒" rceil "⌉" rdquo "”" real "ℜ" reg "®" rfloor "⌋" Rho "Ρ" rho "ρ" rlm "" rsaquo "›" rsquo "’" sbquo "‚" scaron "š" Scaron "Š" sdot "⋅" sect "§" shy "" Sigma "Σ" sigma "σ" sigmaf "ς" sim "∼" spades "♠" sub "⊂" sube "⊆" sum "∑" sup "⊃" sup1 "¹" sup2 "²" sup3 "³" supe "⊇" szlig "ß" Tau "Τ" tau "τ" there4 "∴" Theta "Θ" theta "θ" thetasym "ϑ" thinsp " " thorn "þ" THORN "Þ" tilde "˜" times "×" trade "™" uacute "ú" Uacute "Ú" uarr "↑" uArr "⇑" ucirc "û" Ucirc "Û" ugrave "ù" Ugrave "Ù" uml "¨" upsih "ϒ" Upsilon "Υ" upsilon "υ" uuml "ü" Uuml "Ü" weierp "℘" Xi "Ξ" xi "ξ" yacute "ý" Yacute "Ý" yen "¥" yuml "ÿ" Yuml "Ÿ" Zeta "Ζ" zeta "ζ" zwj "" zwnj ""))
         (pubmed--html-to-unicode (lambda (s) (or (plist-get html-entities (intern (substring s 1 -1))) s))))
    (replace-regexp-in-string "&[^; ]*;" pubmed--html-to-unicode string)))

(defun pubmed--remove-html-tags (string)
  "Remove all HTML tags from STRING."
  (let ((regexp "<[[:alnum:][:blank:]/=\"-]*?>"))
    (replace-regexp-in-string regexp "" string)))

(defun pubmed---html-cleanup (string)
  "Cleanup all HTML from STRING."
  (pubmed--remove-html-tags
   (pubmed--html-to-unicode string)))

(defun pubmed--list (entries)
  "Populate the tabulated list mode buffer."
  (let ((pubmed-buffer (get-buffer-create "*PubMed*"))
	(inhibit-read-only t))
    (with-current-buffer pubmed-buffer
      (pubmed-mode)
      (setq tabulated-list-entries (append entries tabulated-list-entries))
      (tabulated-list-print nil t)
      (setq pubmed-uid (tabulated-list-get-id)))
    (switch-to-buffer pubmed-buffer)))

(defun pubmed--esearch (query)
  "Search PubMed with QUERY. Use ESearch to retrieve the UIDs and post them on the History server."
  (let* ((hexified-query (url-hexify-string query)) ;  All special characters are URL encoded. 
	 (encoded-query (s-replace "%20" "+" hexified-query)) ; All (hexified) spaces are replaced by '+' signs
	 (url-request-method "POST")
	 (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data (concat "db=" pubmed-db
				   "&retmode=json"
				   "&sort=" pubmed-sort
				   "&term=" encoded-query
				   "&usehistory=" pubmed-usehistory
				   (when (not (string-empty-p pubmed-webenv))
				     (concat "&webenv=" pubmed-webenv))
				   (when (not (string-empty-p pubmed-api-key))
				     (concat "&api_key=" pubmed-api-key)))))
    (message "Searching...")
    (url-retrieve pubmed-esearch-url 'pubmed--parse-esearch)))

(defun pubmed--parse-esearch (status)
  "Parse the JSON object in the data retrieved by `pubmed--esearch'. First use ESearch to retrieve the UIDs and post them on the History server, then use multiple ESummary calls to retrieve the data in batches of 500."
  (let* ((url-error (plist-get status :error))
	 (json (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8))
	 (json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type nil)
	 (json-object (json-read-from-string json))
	 (esearchresult (plist-get json-object :esearchresult))
	 (error-message (plist-get esearchresult :ERROR))
	 (count (string-to-number (plist-get esearchresult :count)))
	 (retstart (string-to-number (plist-get esearchresult :retstart)))
	 (retmax (string-to-number (plist-get esearchresult :retmax)))
	 (querykey (plist-get esearchresult :querykey))
	 (webenv (plist-get esearchresult :webenv)))
    (cond
     (url-error
      (if (eq (cadr url-error) 'http)
      	  (error "HTTP Error %i: %s" (caddr url-error) (pubmed--get-url-error (caddr url-error)))
      	(signal (car url-error) (cdr url-error))))
     (error-message
      (error "ESearch returned %s: " error-message))
     ((eq count 0)
      (message "No items found for query"))
     (t
      (progn
	(setq pubmed-webenv webenv)
	(pubmed--get-docsums querykey webenv count retstart retmax))))))

(defun pubmed--get-docsums (querykey webenv count &optional retstart retmax)
  "Use multiple ESummary calls to retrieve the document summaries (DocSums) for a set of UIDs stored on the Entrez History server in batches of 500. The QUERYKEY specifies which of the UID lists attached to the given WEBENV will be used as input to ESummary."
  (interactive)
  (let ((start (if (boundp 'retstart)
		   retstart
		 pubmed-retstart))
	(max (if (boundp 'retmax)
		 retmax
	       pubmed-retmax))
	(counter 0)
	(pubmed-buffer (get-buffer-create "*PubMed*")))
    ;; Workaround to prevent 400 Bad Request Error: sleep for 0.5 seconds after posting to the Entrez History server
    (sleep-for 0.5) 
    (with-current-buffer pubmed-buffer
      ;; Remove previous entries from the `tabulated-list-entries' variable.
      (setq tabulated-list-entries nil))
    (while (< start count)
      ;; Limit the amount of requests to prevent errors like "Too Many Requests (RFC 6585)" and "Bad Request". NCBI mentions a limit of 3 requests/second without an API key and 10 requests/second with an API key (see <https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/>).
      (if (string-empty-p pubmed-api-key)
	  (progn
	    (if (<= counter pubmed-limit-without-api-key)
		(progn
		  (pubmed--esummary querykey webenv start max)
		  (setq counter (1+ counter)))
	      (progn
		(run-with-timer "1 sec" nil 'pubmed--esummary querykey webenv start max)
		(setq counter 0))))
	(progn
	  (if (<= counter pubmed-limit-with-api-key)
	      (progn
		(pubmed--esummary querykey webenv start max)
		(setq counter (1+ counter)))
	    (progn
	      (run-with-timer "1 sec" nil 'pubmed--esummary querykey webenv start max)
	      (setq counter 0)))))
      (setq start (+ start max)))))

(defun pubmed--esummary (querykey webenv retstart retmax)
  "Retrieve the document summaries (DocSums) of a set of UIDs stored on the Entrez History server."
  (let*((url-request-method "POST")
	(url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (concat "db=" pubmed-db
				  "&retmode=" pubmed-retmode
				  "&retstart=" (number-to-string retstart)
				  "&retmax=" (number-to-string retmax)
				  "&query_key=" querykey
				  "&webenv=" webenv
				  (when (not (string-empty-p pubmed-api-key))
				    (concat "&api_key=" pubmed-api-key)))))
    (url-retrieve pubmed-esummary-url 'pubmed--parse-esummary)))

(defun pubmed--parse-esummary (status)
  "Parse the JSON object in the data retrieved by `pubmed--esummary'."
  (let ((url-error (plist-get status :error)))
    (cond
     (url-error
      (if (eq (cadr url-error) 'http)
      	  (error "HTTP Error %i: %s" (caddr url-error) (pubmed--get-url-error (caddr url-error)))
      	(signal (car url-error) (cdr url-error))))
     (t
      (let* ((json (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8))
	     (json-object-type 'plist)
             (json-array-type 'list)
             (json-key-type nil)
  	     (json-object (json-read-from-string json))
  	     (result (plist-get json-object :result))
	     (uids (plist-get result :uids))
	     entries)
	;; The JSON object is converted to a plist. The first keyword is ":uids", with a list of all uids as value. The rest of the keywords are named ":<uid>", with a plist containing the document summary (DocSum) as value.
	;; Add the plist containing the DocSums to `pubmed-entries'
	(setq pubmed-entries (append (cddr (plist-member result :uids)) pubmed-entries))
	;; Iterate over the list of UIDs, convert them to a keyword, and get the value.
	;; The  `tabulated-list-entries' variable specifies the entries displayed in the Tabulated List buffer. Each list element corresponds to one entry, and has the form "(ID CONTENTS)", where "ID" is UID and "CONTENTS" is a vector with the same number of elements as `tabulated-list-format'.
	(dolist (uid uids entries)
	  (let* ((keyword (intern (concat ":" uid)))
		 (value (plist-get result keyword))
		 (entry (list (plist-get value :uid)
	    		      (vector (plist-get value :sortfirstauthor)
				      (pubmed---html-cleanup (plist-get value :title))
	    			      (plist-get value :source)
				      (pubmed--parse-time-string (plist-get value :sortpubdate))))))
	    (push entry entries)))
	(pubmed--list (nreverse entries)))))))

(defun pubmed--parse-efetch (status)
  "Parse the XML object in the data retrieved by `pubmed-show-entry' and show the result in the \"*PubMed-entry*\" buffer."
  (let ((url-error (plist-get status :error)))
    (cond
     (url-error
      (if (eq (cadr url-error) 'http)
      	  (error "HTTP Error %i: %s" (caddr url-error) (pubmed--get-url-error (caddr url-error)))
      	(signal (car url-error) (cdr url-error))))
     (t
      (let* ((dom (libxml-parse-xml-region (1+ url-http-end-of-headers) (point-max)))
	     (summary (esxml-query "PubmedArticle" dom))
	     (pubmed-entry-buffer (get-buffer-create "*PubMed-entry*"))
	     (inhibit-read-only t))
	(with-current-buffer pubmed-entry-buffer
	  (pubmed-show-mode)
	  (erase-buffer)
	  (cond
	   (summary
	    (progn
	      (insert (pubmed--summary-journal-isoabbreviation summary))
	      (if (s-suffix-p "." (pubmed--summary-journal-isoabbreviation summary))
		  (insert " ")
		(insert ". "))
	      (insert (pubmed--summary-journal-pubdate summary))

	      (if (equal (pubmed--summary-publicationstatus summary) "aheadofprint")
		  (progn
		    (when (pubmed--summary-elocation summary)
      		      (let ((elocationlist (pubmed--summary-elocation summary))
      			    elocations)
      			(dolist (elocation elocationlist)
      			  (push (concat (plist-get elocation 'type) ": " (plist-get elocation 'id)) elocations))
			(insert ". ")
      			(insert (s-join ". " (nreverse elocations)))
			(insert ". ")))
		    (insert "[Epub ahead of print]"))
		(when (plist-get (pubmed--summary-journal-issue summary) 'volume)
		  (insert ";")
		  (insert (plist-get (pubmed--summary-journal-issue summary) 'volume)))
		(when (plist-get (pubmed--summary-journal-issue summary) 'issue)
      		  (insert "(" (plist-get (pubmed--summary-journal-issue summary) 'issue) ")" ))
		(when (pubmed--summary-pagination summary)
		  (insert ":")
		  (insert (pubmed--summary-pagination summary))
		  (insert ". ")))
	      (insert "\n\n")
	      (let ((article-title (pubmed--summary-article-title summary)))
		(put-text-property 0 (length article-title) 'face 'bold article-title)
		(insert article-title))
	      (insert "\n")
	      (when (pubmed--summary-authors summary)
      		(let ((authorlist (pubmed--summary-authors summary))
      		      authors)
      		  (dolist (author authorlist)
      		    (cond
      		     ((and (plist-get author 'lastname) (plist-get author 'initials))
      		      (push (concat (plist-get author 'lastname) " " (plist-get author 'initials)) authors))
      		     ((plist-get author 'collectivename)
      		      (push (plist-get author 'collectivename) authors))))
      		  (insert (s-join ", " (nreverse authors)))
      		  (insert "\n\n")))
	      (when (pubmed--summary-investigators summary)
      		(let ((investigatorlist (pubmed--summary-investigators summary))
      		      investigators)
      		  (dolist (investigator investigatorlist)
      		    (push (concat (plist-get investigator 'lastname) " " (plist-get investigator 'initials)) investigators))
      		  (insert "Collaborators (" (number-to-string (length investigatorlist)) ")\n")
      		  (insert (s-join ", " (nreverse investigators)))
      		  (insert "\n\n")))
	      (when (pubmed--summary-abstract summary)
		(let ((heading "ABSTRACT"))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert "\n")
      		(insert (pubmed--summary-abstract summary))
      		(insert "\n\n"))
	      (when (pubmed--summary-keywords summary)
      		(insert "\n")
		(let ((heading "KEYWORDS: "))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert (s-join "; " (pubmed--summary-keywords summary)))
      		(insert "\n\n"))
	      (when  (plist-get (pubmed--summary-articleid summary) 'pubmed)
		(let ((heading "PMID: "))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert (plist-get (pubmed--summary-articleid summary) 'pubmed) "\n"))
	      (when (plist-get (pubmed--summary-articleid summary) 'doi)
		(let ((heading "DOI: "))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert (plist-get (pubmed--summary-articleid summary) 'doi) "\n"))
	      ;; (when (plist-get (pubmed--summary-articleid summary) 'pii)
	      ;; 	(insert "PII: " (plist-get (pubmed--summary-articleid summary) 'pii) "\n"))
	      (when (plist-get (pubmed--summary-articleid summary) 'pmc)
		(let ((heading "PMCID: "))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert (plist-get (pubmed--summary-articleid summary) 'pmc) "\n"))
	      (when (pubmed--summary-commentscorrections summary)
      		(insert "\n")
		(let ((heading "Comment in:"))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert "\n")
      		(let ((commentslist (pubmed--summary-commentscorrections summary)))
      		  (dolist (comment commentslist)
      		    ;; (insert (plist-get comment 'reftype))
      		    (insert (plist-get comment 'refsource))
      		    ;; TODO: make refsource a link
      		    ;; (insert (plist-get comment 'pmid))
      		    (insert "\n"))))
	      (when (pubmed--summary-references summary)
      		(insert "\n")
		(let ((heading "References in:"))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert "\n")
      		(let ((referencelist (pubmed--summary-references summary)))
      		  (dolist (reference referencelist)
      		    (insert (plist-get reference 'citation))
      		    ;; TODO: make reference a link
      		    ;; (insert (plist-get reference 'pubmed))
      		    (insert "\n"))))
	      (when (pubmed--summary-publicationtype summary)
      		(insert "\n")
		(let ((heading "Publication types:"))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert "\n")
      		(let ((publicationtypelist (pubmed--summary-publicationtype summary)))
      		  (dolist (publicationtype publicationtypelist)
      		    (insert (plist-get publicationtype 'type))
      		    (insert "\n"))))
	      (when (pubmed--summary-mesh summary)
      		(insert "\n")
		(let ((heading "MeSH terms:"))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert "\n")
      		(let ((meshheadinglist (pubmed--summary-mesh summary)))
      		  ;; Iterate over the meshheadings
      		  (dolist (meshheading meshheadinglist)
      		    (let ((qualifiers (plist-get meshheading 'qualifiers)))
      		      ;; If the descriptor (or subject heading) has qualifiers (or subheadings)
      		      (if qualifiers
      			  ;; Iterate over the qualifiers
      			  (dolist (qualifier qualifiers)
      			    ;; Insert "descriptor/qualifier"
      			    (insert (plist-get meshheading 'descriptor))
      			    (insert "/")
      			    (insert (plist-get qualifier 'qualifier))
      			    (insert "\n"))
      			;; If the descriptor (or subject heading) has no qualifiers (or subheadings)
      			;; Insert "descriptor"
      			(insert (plist-get meshheading 'descriptor))
      			(insert "\n"))))))
	      (when (pubmed--summary-grant summary)
      		(insert "\n")
		(let ((heading "Grant support:"))
		  (put-text-property 0 (length heading) 'face 'bold heading)
		  (insert heading))
      		(insert "\n")
      		(let ((grantlist (pubmed--summary-grant summary)))
      		  (dolist (grant grantlist)
      		    (insert (plist-get grant 'grantid))
      		    (insert "/")
      		    (insert (plist-get grant 'agency))
		    (when (plist-get grant 'country)
      		      (insert "/")
      		      (insert (plist-get grant 'country)))
      		    (insert "\n"))))
	      (goto-char (point-min))))
	   (t
	    ;; FIXME: add support for books
	    (insert "No article"))))
	(save-selected-window
	  (display-buffer pubmed-entry-buffer)))))))

(defun pubmed--summary-pmid (summary)
  "Return the PMID of the article SUMMARY"
  (esxml-query "PMID *" summary))

(defun pubmed--summary-datecompleted (summary)
  "Return the completed date of the article SUMMARY. The time value of the date can be converted by `format-time-string' to a string according to FORMAT-STRING."
  (let* ((datecompleted (encode-time 0
				     0
				     0
				     (string-to-number (esxml-query "DateCompleted Day *" summary))
				     (string-to-number (esxml-query "DateCompleted Month *" summary))
				     (string-to-number (esxml-query "DateCompleted Year *" summary)))))
    datecompleted))

(defun pubmed--summary-daterevised (summary)
  "Return the revised date value of the article SUMMARY. The time value of the date can be converted by `format-time-string' to a string according to FORMAT-STRING."
  (let* ((daterevised (encode-time 0
				   0
				   0
				   (string-to-number (esxml-query "DateRevised Day *" summary))
				   (string-to-number (esxml-query "DateRevised Month *" summary))
				   (string-to-number (esxml-query "DateRevised Year *" summary)))))
    daterevised))

(defun pubmed--summary-pubmodel (summary)
  "Return the publication model of the article SUMMARY"
  (esxml-node-attribute 'PubModel (esxml-query "Article" summary)))

(defun pubmed--summary-issn (summary)
  "Return a plist with the journal ISSN and ISSN type of the article SUMMARY. The plist has the form \"('issn ISSN 'type TYPE)\"."
  "Return the ISSN of the article SUMMARY"
  (let ((issn (esxml-query "Journal ISSN *" summary))
	(type (esxml-node-attribute 'IssnType (esxml-query "Journal ISSN" summary))))
    (list 'issn issn 'type type)))

(defun pubmed--summary-journal-issue (summary)
  "Return a plist with the journal year, season, issue, volume, and cited medium of the article SUMMARY. The plist has the form \"('year YEAR 'season SEASON 'issue ISSUE 'volume VOLUME 'citedmedium CITEDMEDIUM)\"."
  (let* ((year (esxml-query "Journal JournalIssue Year *" summary))
	 (season (esxml-query "Journal JournalIssue Season *" summary))
	 (issue (esxml-query "Journal JournalIssue Issue *" summary))
	 (volume (esxml-query "Journal JournalIssue Volume *" summary))
	 (citedmedium (esxml-node-attribute 'CitedMedium (esxml-query "Journal JournalIssue" summary))))
    (list 'year year 'season season 'issue issue 'volume volume 'citedmedium citedmedium)))

(defun pubmed--summary-journal-pubdate (summary)
  "Return a string with the journal publication date of the article SUMMARY."
  (let* ((day (esxml-query "Article Journal JournalIssue PubDate Day *" summary))
	 (month (esxml-query "Article Journal JournalIssue PubDate Month *" summary))
	 (year (esxml-query "Article Journal JournalIssue PubDate Year *" summary))
	 (medlinedate (esxml-query "Article Journal JournalIssue PubDate MedlineDate *" summary)))
    ;; If MONTH is a number
    (when (and month (string-match "[[:digit:]]+" month))
      ;; Convert the month number to the abbreviated month name
      (setq month (nth (1- (string-to-number month)) pubmed-months)))
    (cond
     ;; Non-standard date formats are stored in MedlineDate fields
     (medlinedate
      medlinedate)
     ((and month day)
      (concat year " " month " " day))
     (month
      (concat year " " month))
     (t
      year))))

(defun pubmed--summary-journal-title (summary)
  "Return the journal title of the article SUMMARY."
  (esxml-query "Journal Title *" summary))

(defun pubmed--summary-journal-isoabbreviation (summary)
  "Return the journal ISO abbreviation of the article SUMMARY."
  (esxml-query "Journal ISOAbbreviation *" summary))

(defun pubmed--summary-article-title (summary)
  "Return the title of the article SUMMARY."
  (esxml-query "Article ArticleTitle *" summary))

(defun pubmed--summary-pagination (summary)
  "Return the pagination of the article SUMMARY."
  (esxml-query "Article Pagination MedlinePgn *" summary))

(defun pubmed--summary-elocation (summary)
  "Return an plist of Elocation IDs of the article SUMMARY.  The plist has the form \"('type TYPE 'id ID)\"."
  (let* ((elocationidlist (esxml-query-all "ELocationID" (esxml-query "Article" summary)))
	 elocationids)
    (dolist (elocationid elocationidlist elocationids)
      (let* ((type (esxml-node-attribute 'EIdType elocationid))
	     (id (car (esxml-node-children elocationid))))
    	(push (list 'type type 'id id) elocationids)))
    (nreverse elocationids)))

(defun pubmed--summary-abstract (summary)
  "Return the abstract of the article SUMMARY. Return nil if no abstract is available."
  (let ((textlist (esxml-query-all "AbstractText" (esxml-query "Article Abstract" summary)))
	texts)
    (when textlist
      ;; Iterate through AbstractText nodes, where structure is like: (AbstractText ((Label . "LABEL") (NlmCategory . "CATEGORY")) "ABSTRACTTEXT")
      (dolist (text textlist)
	(let ((label (esxml-node-attribute 'Label text))
	      (nlmcategory (esxml-node-attribute 'NlmCategory text)) ; NlmCategory attribute is ignored
	      (abstracttext (car (esxml-node-children text))))
	  (if
	      (and label abstracttext)
	      (push (concat label ": " abstracttext) texts)
	    (push abstracttext texts))))
      (s-join "\n\n" (nreverse texts)))))

(defun pubmed--summary-authors (summary)
  "Return an plist with the authors of the article SUMMARY. Each list element corresponds to one author, and is a plist with the form \"('lastname LASTNAME 'forename FORENAME 'initials INITIALS 'affiliationinfo AFFILIATIONINFO 'collectivename COLLECTIVENAME)\"."
  (let ((authorlist (esxml-query-all "Author" (esxml-query "Article AuthorList" summary)))
	authors)
    (dolist (author authorlist)
      (let ((lastname (esxml-query "LastName *" author))
	    (forename (esxml-query "ForeName *" author))
    	    (initials (esxml-query "Initials *" author))
	    (affiliationinfo (esxml-query "AffiliationInfo Affiliation *" author))
    	    (collectivename (esxml-query "CollectiveName *" author)))
    	(push (list 'lastname lastname 'forename forename 'initials initials 'affiliationinfo affiliationinfo 'collectivename collectivename) authors)))
    (nreverse authors)))

(defun pubmed--summary-language (summary)
  "Return the language of the article SUMMARY."
  (esxml-query "Article Language *" summary))

(defun pubmed--summary-grant (summary)
  "Return a list of the grants of the article SUMMARY. Each list element corresponds to one grant, and is a plist with the form \"('grantid GRANTID 'agency AGENCY 'country COUNTRY)\"."
  (let ((grantlist (esxml-query-all "Grant" (esxml-query "Article GrantList" summary)))
	grants) ;; make sure list starts empty
    (dolist (grant grantlist)
      (let ((grantid (esxml-query "GrantID *" grant))
	    (agency (esxml-query "Agency *" grant))
	    (country (esxml-query "Country *" grant)))
	(push (list 'grantid grantid 'agency agency 'country country) grants)))
    (nreverse grants)))

(defun pubmed--summary-publicationtype (summary)
  "Return a plist of the publication types and unique identifiers of the article SUMMARY. The plist has the form \"('type TYPE 'ui UI)\"."
  ;; Iterate through PublicationType nodes, where structure is like: (PublicationType ((UI . "UI")) "PUBLICATIONTYPE")
  (let ((publicationtypelist (esxml-query-all "PublicationType" (esxml-query "Article PublicationTypeList" summary)))
	publicationtypes) ;; make sure list starts empty
    (dolist (publicationtype publicationtypelist)
      (let ((type (car (esxml-node-children publicationtype)))
    	    (ui (esxml-node-attribute 'UI publicationtype)))
	;; For each `publicationtype' push the type to the list `publicationtypes'
	;; Ignore the UI
	(push (list 'type type 'ui ui) publicationtypes)))
    (nreverse publicationtypes)))

(defun pubmed--summary-articledate (summary)
  "Return a plist of article date and date type of the article SUMMARY. The plist has the form \"('type TYPE 'date date)\". The time value of the date can be converted by `format-time-string' to a string according to FORMAT-STRING."
  (let ((type (esxml-node-attribute 'DateType (esxml-query "Article ArticleDate" summary)))
	(date (encode-time 0
			   0
			   0
			   (string-to-number (esxml-query "Article ArticleDate Day *" summary))
			   (string-to-number  (esxml-query "Article ArticleDate Month *" summary))
			   (string-to-number (esxml-query "Article ArticleDate Year *" summary)))))
    (list 'type type 'date date)))

(defun pubmed--summary-medlinejournalinfo (summary)
  "Return a plist with the country, journal title abbreviation (MedlineTA), LocatorPlus accession number (NlmUniqueID) and ISSNLinking element of the article SUMMARY. The plist has the form \"('country COUNTRY 'medlineta MEDLINETA 'nlmuniqueid NLMUNIQUEID 'issnlinking ISSNLINKING)\"."
  (let ((country (esxml-query "MedlineJournalInfo Country *" summary))
	(medlineta (esxml-query "MedlineJournalInfo MedlineTA *" summary))
	(nlmuniqueid (esxml-query "MedlineJournalInfo NlmUniqueID *" summary))
	(issnlinking (esxml-query "MedlineJournalInfo ISSNLinking *" summary)))
    (list 'country country 'medlineta medlineta 'nlmuniqueid nlmuniqueid 'issnlinking issnlinking)))

(defun pubmed--summary-substances (summary)
  "Return a plist of the chemical substances and unique identifiers of the article SUMMARY. Each list element corresponds to one substance, and is a plist with the form \"('registrynumber REGISTRYNUMBER 'substance SUBSTANCE 'ui UI)\"."
  (let ((chemicallist (esxml-query-all "Chemical" (esxml-query "ChemicalList" summary)))
	chemicals)
    (dolist (chemical chemicallist)
      (let ((registrynumber (esxml-query "RegistryNumber *" chemical)) ; RegistryNumber is ignored
	    (substance (esxml-query "NameOfSubstance *" chemical))
	    (ui (esxml-node-attribute 'UI (esxml-query "NameOfSubstance" chemical))))
	(push (list 'registrynumber registrynumber 'substance substance 'ui ui) chemicals)))
    (nreverse chemicals)))

(defun pubmed--summary-mesh (summary)
  "Return an list of the MeSH terms  of the article SUMMARY. Each list element corresponds to one descriptor (or subject heading) and its qualifiers (or subheadings), and is a plist with the form \"('descriptor DESCRIPTOR 'ui UI 'qualifiers (('qualifier QUALIFIER 'ui UI) ('qualifier QUALIFIER 'ui UI) (...)))\"."
  (let ((meshheadinglist (esxml-query-all "MeshHeading" (esxml-query "MeshHeadingList" summary)))
	meshheadings)
    (dolist (meshheading meshheadinglist)
      (let ((descriptorname (esxml-query "DescriptorName *" meshheading))
      	    (descriptorui (esxml-node-attribute 'UI (esxml-query "DescriptorName" meshheading)))
      	    (qualifierlist (esxml-query-all "QualifierName" meshheading))
	    qualifiers)
	(dolist (qualifier qualifierlist)
	  (let ((qualifiername (esxml-query "QualifierName *" qualifier))
      		(qualifierui (esxml-node-attribute 'UI (esxml-query "QualifierName" qualifier))))
	    (push (list 'qualifier qualifiername 'ui qualifierui) qualifiers)))
	(push (list 'descriptor descriptorname 'ui descriptorui 'qualifiers qualifiers) meshheadings)))
    (nreverse meshheadings)))

(defun pubmed--summary-commentscorrections (summary)
  "Return the correction of the article SUMMARY. The plist has the form \"('reftype REFTYPE 'refsource REFSOURCE 'pmid PMID)\"."
  (let ((commentscorrectionslist (esxml-query-all "CommentsCorrections" (esxml-query "CommentsCorrectionsList" summary)))
	commentscorrections)
    (dolist (commentscorrection commentscorrectionslist)
      (let ((reftype (esxml-node-attribute 'RefType commentscorrection))
	    (refsource (esxml-query "RefSource *" commentscorrection))
	    (pmid (esxml-query "PMID *" commentscorrection)))
	;; For each `commentscorrection' push the reftype, refsource and pmid to the list `commentscorrections'
    	(push (list 'reftype reftype 'refsource refsource 'pmid pmid) commentscorrections)))
    (nreverse commentscorrections)))

(defun pubmed--summary-publicationstatus (summary)
  "Return the publication status of the article SUMMARY."
  (esxml-query "PubmedData PublicationStatus *" summary))

(defun pubmed--summary-articleid (summary)
  "Return an plist of the article IDs. The plist has the form \"('pubmed pubmed 'doi DOI 'pii PII 'pmc PMC 'mid MID)\"."
  (let ((articleidlist (esxml-query-all "ArticleId" (esxml-query "PubmedData ArticleIdList" summary)))
	articleids)
    (dolist (articleid articleidlist)
      (let ((idtype (intern (esxml-node-attribute 'IdType articleid)))
	    (id (car (esxml-node-children articleid))))
	(push idtype articleids)
	(push id articleids)))
    (nreverse articleids)))

(defun pubmed--summary-keywords (summary)
  "Return an alist of the article keywords."
  (let ((keywordlist (esxml-query-all "Keyword" (esxml-query "KeywordList" summary)))
	keywords)
    (dolist (keyword keywordlist)
      (push (car (esxml-node-children keyword)) keywords))
    (nreverse keywords)))

(defun pubmed--summary-investigators (summary)
  "Return an plist with the investigators of the article SUMMARY. Each list element corresponds to one investigator, and is a plist with the form \"('lastname LASTNAME 'forename FORENAME 'initials INITIALS)\"."
  (let ((investigatorlist (esxml-query-all "Investigator" (esxml-query "InvestigatorList" summary)))
	investigators)
    (dolist (investigator investigatorlist)
      (let ((lastname (esxml-query "LastName *" investigator))
	    (forename (esxml-query "ForeName *" investigator))
    	    (initials (esxml-query "Initials *" investigator)))
    	(push (list 'lastname lastname 'forename forename 'initials initials) investigators)))
    (nreverse investigators)))

(defun pubmed--summary-references (summary)
  "Return a plist of the references of the article SUMMARY. Each list element corresponds to one reference, The has the form \"('citation CITATION 'pubmed PMID)\"."
  (let ((referencelist (esxml-query-all "Reference" (esxml-query "ReferenceList" summary)))
	references)
    (dolist (reference referencelist)
      (let ((citation (esxml-query "Citation *" reference))
	    (articleidlist (esxml-query-all "ArticleId" (esxml-query "ArticleIdList" reference))))
	(dolist (articleid articleidlist)
	  (let ((idtype (intern (esxml-node-attribute 'IdType articleid)))
		(id (car (esxml-node-children articleid))))
	    (push (list 'citation citation idtype id) references)))))
    (nreverse references)))

(defun pubmed--view-pdf (buffer)
  "Create a temporary pdf file containing BUFFER and open it with the default pdf viewer."
  (let ((data (with-current-buffer buffer (buffer-substring (1+ url-http-end-of-headers) (point-max))))
	(tempfile (make-nearby-temp-file "pubmed-" nil ".pdf")))
    (with-temp-file tempfile
      (set-buffer-file-coding-system 'binary)
      (insert data))
    (find-file tempfile)))

;;;; Footer

(provide 'pubmed)

;;; pubmed.el ends here
