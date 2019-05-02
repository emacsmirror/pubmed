;;; pubmed.el --- Interface to PubMed -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>
;; Created: 2018-05-23
;; Version: 0.2.1
;; Keywords: pubmed, hypermedia
;; Package-Requires: ((emacs "25.1") (deferred "0.5.1") (esxml "0.3.4") (s "1.12.0"))
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

;; This is a GNU Emacs interface to the PubMed database of references on life
;; sciences and biomedical topics.

;;; Code:

;;;; Requirements

(require 'pubmed-bibtex)
(require 'pubmed-openaccessbutton)
(require 'pubmed-pmc)
(require 'esxml)
(require 'esxml-query)
(require 'eww)
(require 'json)
(require 's)
(require 'url)

;;;; Variables

(defvar pubmed-efetch-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
  "EFetch base URL.")

(defvar pubmed-esearch-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  "ESearch base URL.")

(defvar pubmed-espell-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/espell.fcgi"
  "ESpell base URL.")

(defvar pubmed-esummary-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
  "ESummary base URL.")

(defvar pubmed-completion-url
  "https://www.ncbi.nlm.nih.gov/portal/utils/autocomp.fcgi?dict=pm_related_queries_2&callback=?&q=%s")

(defvar pubmed-author-completion-url
  "https://www.ncbi.nlm.nih.gov/portal/utils/autocomp.fcgi?dict=auf&q=%s")

(defvar pubmed-journal-completion-url
  "https://www.ncbi.nlm.nih.gov/portal/utils/autocomp.fcgi?dict=jrz&q=%s")

;; Web environment string returned from a previous ESearch, EPost or
;; ELink call. When provided, ESearch will post the results of the
;; search operation to this pre-existing WebEnv, thereby appending the
;; results to the existing environment. In addition, providing WebEnv
;; allows query keys to be used in term so that previous search sets
;; can be combined or limited."
(defvar pubmed-webenv ""
  "Web environment string.")

(defvar pubmed-uid nil
  "The UID of the entry currently selected in the PubMed buffer.")

(defvar pubmed-entries nil
  "The plist of citations retrieved by the last PubMed search.")

(defvar pubmed-history-list nil
  "The PubMed history list.")

(defvar pubmed-entry-timer nil
  "The timer that is set to delay EFetch calls.")

(defvar pubmed-entry-delay 0.5
  "Delay in seconds before fetching the PubMed entry; default=0.5.
Seconds may be an integer or floating point number. Purpose of
the delay is to prevent frequent EFetch calls and exceeding the
E-utilities rate limit when walking fast through the PubMed
entries.")

(defvar pubmed-limit-with-api-key 10
  "Maximum amount of E-utilities requests/second with API key.")

(defvar pubmed-limit-without-api-key 3
  "Maximum amount of E-utilities requests/second without API key.")

;; Sequential index of the first record to be retrieved (default=0,
;; corresponding to the first record of the entire set). This
;; parameter can be used in conjunction with retmax to download an
;; arbitrary subset of records from the input set.
(defvar pubmed-retstart 0
  "Sequential index of the first record; default=0.")

;; Total number of records from the retrieved set to be shown in the
;; output. The remainder of the retrieved set will be stored on the
;; History server. Increasing retmax allows more of the retrieved
;; records to be included in the output, up to a maximum of 100,000
;; records. To retrieve more than 100,000 records, submit multiple
;; esearch requests while incrementing the value of retstart.
(defvar pubmed-retmax 500
  "Number of records returned; default=500.")

(defvar pubmed-months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "Abbreviated months.")

;;;; Keymap

(defvar pubmed-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'pubmed-show-current-entry)
    (define-key map (kbd "f") #'pubmed-get-fulltext)
    (define-key map (kbd "m") #'pubmed-mark)
    (define-key map (kbd "n") #'pubmed-show-next)
    (define-key map (kbd "p") #'pubmed-show-prev)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "s") #'pubmed-search)
    (define-key map (kbd "u") #'pubmed-unmark)
    (define-key map (kbd "U") #'pubmed-unmark-all)
    (define-key map (kbd "w") #'pubmed-bibtex-write)
    (define-key map (kbd "TAB") #'pubmed-bibtex-show)
    map)
  "Local keymap for `pubmed-mode'.")

(defvar pubmed-search-mode-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "Local keymap for `pubmed-search-mode'.")

;;;; Customization

(defgroup pubmed nil
  "Interface to PubMed."
  :group 'external)

(defcustom pubmed-api-key ""
  "E-utilities API key."
  :group 'pubmed
  :type 'string)

(defcustom pubmed-search-completion t
  "If non-nil, use completion using PubMed suggestions."
  :group 'pubmed
  :type 'boolean)

(defcustom pubmed-max-results 10000
  "Maximum number of search results.
A warning is issued if the count of search results exceeds this
  number."
  :group 'pubmed
  :type 'integer)

(defcustom pubmed-sort "most+recent"
  "Method used to sort records in the ESearch output.
The records are loaded onto the History Server in the specified
sort order and will be retrieved in that order by ESummary or
EFetch. The default sort order is \"most+recent\".

Valid sort values include: \"journal\": Records are sorted
alphabetically by journal title, and then by publication date.

\"pub+date\": Records are sorted chronologically by publication
date \(with most recent first\), and then alphabetically by
journal title. \"most+recent\": Records are sorted
chronologically by date added to PubMed \(with the most recent
additions first\). \"relevance\": Records are sorted based on
relevance to your search. For more information about PubMed's
relevance ranking, see the PubMed Help section on Computation of
Weighted Relevance Order in PubMed. \"title\": Records are sorted
alphabetically by article title. \"author\": Records are sorted
alphabetically by author name, and then by publication date."
  :group 'pubmed
  :type 'string)

(defcustom pubmed-time-format-string "%Y-%m-%d"
  "The format-string to convert time values.
Default is the ISO 8601 date format, i.e., \"%Y-%m-%d\"."
  :link '(info-link "(elisp) Time Parsing")
  :group 'pubmed
  :type 'string)

(defcustom pubmed-fulltext-functions '(pubmed-pmc pubmed-openaccessbutton)
  "The list of functions tried in order by `pubmed-fulltext'.
To change the behavior of ‘pubmed-get-fulltext’, remove, change
  the order of, or insert functions in this list. Each function
  should accept no arguments, and return a string or nil."
  :group 'pubmed
  :type '(repeat function)
  :options '(pubmed-pmc
	     pubmed-openaccessbutton
	     pubmed-unpaywall
	     pubmed-scihub))

(defcustom pubmed-temp-prefix "pubmed-"
  "Prefix for temporary files created by pubmed."
  :group 'pubmed
  :type 'string)

;;;; Mode

;;;###autoload
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

;;;; Commands

;;;###autoload
(defun pubmed-show-mode ()
  "Mode for displaying PubMed entries."
  (interactive)
  (use-local-map pubmed-mode-map)
  (setq major-mode 'pubmed-show-mode
        mode-name "pubmed-show"
        buffer-read-only t)
  (buffer-disable-undo))

;;;###autoload
(defun pubmed-search (query)
  "Search PubMed with QUERY."
  (interactive
   (let* ((minibuffer-setup-hook (when (eq pubmed-search-completion t)
				   (lambda () (add-hook 'completion-at-point-functions #'pubmed-completion-at-point nil t))))
	  (query (read-from-minibuffer "Query: " nil pubmed-search-mode-map nil 'pubmed-history-list)))
     (list query)))
  (setq pubmed-entries nil)
  (pubmed--esearch query))

(defun pubmed-show-entry (uid)
  "Display entry UID in the current buffer."
  (interactive)
  ;; "Return the parsed summary for an UID"
  ;; TODO: Only the summary of the first UID is returned. Consider returning multiple summaries at once when multiple UIDs are passed as argument.
  (let ((url-request-method "POST")
	(url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (concat "db=pubmed"
				  "&retmode=xml"
				  "&rettype=abstract"
				  "&id=" uid
				  (when (not (string-empty-p pubmed-api-key))
				    (concat "&api_key=" pubmed-api-key)))))
    (url-retrieve pubmed-efetch-url #'pubmed--parse-efetch)))

(defun pubmed-show-current-entry ()
  "Show the current entry in the \"pubmed-show\" buffer."
  (interactive)
  (setq pubmed-uid (tabulated-list-get-id))
  (when (timerp pubmed-entry-timer)
    (cancel-timer pubmed-entry-timer))
  (setq pubmed-entry-timer (run-with-timer pubmed-entry-delay nil #'pubmed-show-entry pubmed-uid)))

(defun pubmed-show-next ()
  "Show the next item in the \"pubmed-show\" buffer."
  (interactive)
  (with-current-buffer "*PubMed*"
    (forward-line)
    (setq pubmed-uid (tabulated-list-get-id))
    (when (get-buffer-window "*PubMed-entry*" "visible")
      (when (timerp pubmed-entry-timer)
	(cancel-timer pubmed-entry-timer))
      (setq pubmed-entry-timer (run-with-timer pubmed-entry-delay nil #'pubmed-show-entry pubmed-uid)))))

(defun pubmed-show-prev ()
  "Show the previous entry in the \"pubmed-show\" buffer."
  (interactive)
  (with-current-buffer "*PubMed*"
    (forward-line -1)
    (setq pubmed-uid (tabulated-list-get-id))
    (when (get-buffer-window "*PubMed-entry*" "visible")
      (when (timerp pubmed-entry-timer)
	(cancel-timer pubmed-entry-timer))
      (setq pubmed-entry-timer (run-with-timer pubmed-entry-delay nil #'pubmed-show-entry pubmed-uid)))))

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
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag "*" t))))

(defun pubmed-unmark-all (&optional _num)
  "Unmark all entries."
  (interactive "p")
  (pubmed--guard)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag " " t))))

(defun pubmed-get-fulltext (&optional entries)
  "Try to fetch the fulltext PDF of the marked entries, the current entry or the optional argument ENTRIES."
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
      (mapcar #'pubmed--fulltext entries))
     (mark-list
      (mapcar #'pubmed--fulltext mark-list))
     ((tabulated-list-get-id)
      (pubmed--fulltext (tabulated-list-get-id)))
     (t
      (error "No entry selected")))))

;;;###autoload
(defun pubmed-complete (&optional _predicate)
  "Perform completion using PubMed suggestions preceding point."
  (interactive)
  (let* ((data (pubmed-completion-at-point))
         (plist (nthcdr 3 data)))
    (if (null data)
        (minibuffer-message "Nothing to complete")
      (let ((completion-extra-properties plist))
        (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)
                              (plist-get plist :predicate))))))

;;;###autoload
(defun pubmed-author-complete (&optional _predicate)
  "Perform completion using PubMed suggestions preceding point."
  (interactive)
  (let* ((data (pubmed-author-completion-at-point))
         (plist (nthcdr 3 data)))
    (if (null data)
        (minibuffer-message "Nothing to complete")
      (let ((completion-extra-properties plist))
	(completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)
                              (plist-get plist :predicate))))))

;;;###autoload
(defun pubmed-journal-complete (&optional _predicate)
  "Perform completion using PubMed suggestions preceding point."
  (interactive)
  (let* ((data (pubmed-journal-completion-at-point))
         (plist (nthcdr 3 data)))
    (if (null data)
        (minibuffer-message "Nothing to complete")
      (let ((completion-extra-properties plist))
        (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)
                              (plist-get plist :predicate))))))

;;;###autoload
(defun pubmed-completion-at-point ()
  "Function used for `completion-at-point-functions'."
  (interactive)
  (let* ((start (line-beginning-position))
	 (end (line-end-position))
	 (text (buffer-substring-no-properties start end)))
    (list start
	  end
	  (completion-table-with-cache
           (lambda (_)
	     (pubmed--completion-candidates text)))
	  :exclusive 'no)))

;;;###autoload
(defun pubmed-author-completion-at-point ()
  "Function used for `completion-at-point-functions'."
  (interactive)
  (let* ((start (line-beginning-position))
	 (end (line-end-position))
	 (text (buffer-substring-no-properties start end)))
    (list start
	  end
	  (completion-table-with-cache
           (lambda (_)
	     (pubmed--author-completion-candidates text)))
	  :exclusive 'no)))

;;;###autoload
(defun pubmed-journal-completion-at-point ()
  "Function used for `completion-at-point-functions'."
  (interactive)
  (let* ((start (line-beginning-position))
	 (end (line-end-position))
	 (text (buffer-substring-no-properties start end)))
    (list start
	  end
	  (completion-table-with-cache
           (lambda (_)
	     (pubmed--journal-completion-candidates text)))
	  :exclusive 'no)))

;;;; Functions

(defun pubmed--guard ()
  "Signal an error when the current buffer is not in `pubmed-mode'."
  (unless (eq major-mode 'pubmed-mode)
    (error "The current buffer is not in PubMed mode")))

(defun pubmed--get-url-error (status)
  "Convert integer STATUS to a human readable response. Return nil if none is found."
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
  "Format TIME-STRING according to `pubmed-time-format-string'.
TIME-STRING should be formatted as \"yyyy/mm/dd HH:MM\"."
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
  "Replace HTML entities with unicode in STRING."
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
  "Populate the tabulated list mode buffer with ENTRIES."
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
	 (url-request-data (concat "db=pubmed"
				   "&retmode=json"
				   "&sort=" pubmed-sort
				   "&term=" encoded-query
				   "&usehistory=y"
				   (when (not (string-empty-p pubmed-webenv))
				     (concat "&webenv=" pubmed-webenv))
				   (when (not (string-empty-p pubmed-api-key))
				     (concat "&api_key=" pubmed-api-key)))))
    (message "Searching...")
    (url-retrieve pubmed-esearch-url #'pubmed--parse-esearch)))

(defun pubmed--parse-esearch (status)
  "Check STATUS and parse the JSON object in the current buffer.
First use ESearch to retrieve the UIDs and post them on the
History server, then use multiple ESummary calls to retrieve the
data in batches of 500."
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
  "Retrieve the document summaries (DocSums) from the Entrez History server.
Use multiple ESummary calls in batches of 500. The QUERYKEY
specifies which of the stored sets to the given WEBENV will be
used as input to ESummary. COUNT is the total number of records
in the stored set. Optional argument RETSTART is the sequential
index of the first record to be retrieved, optional argument
RETMAX is the total number of records to be retrieved."
  (catch 'cancel
    (when (> count pubmed-max-results)
      (unless (y-or-n-p (format "There are %i results. Are you sure you want to continue? " count))
	(message "Searching...cancelled")
	(throw 'cancel t)))
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
		  (run-with-timer "1 sec" nil #'pubmed--esummary querykey webenv start max)
		  (setq counter 0))))
	  (progn
	    (if (<= counter pubmed-limit-with-api-key)
		(progn
		  (pubmed--esummary querykey webenv start max)
		  (setq counter (1+ counter)))
	      (progn
		(run-with-timer "1 sec" nil #'pubmed--esummary querykey webenv start max)
		(setq counter 0)))))
	(setq start (+ start max)))
      (message "Searching...done"))))

(defun pubmed--esummary (querykey webenv retstart retmax)
  "Retrieve the document summaries (DocSums) from the Entrez History server.
The QUERYKEY specifies which of the stored sets to the given
WEBENV will be used as input to ESummary. RETSTART is the
sequential index of the first record to be retrieved, RETMAX is
the total number of records to be retrieved."
  (let*((url-request-method "POST")
	(url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (concat "db=pubmed"
				  "&retmode=json"
				  "&retstart=" (number-to-string retstart)
				  "&retmax=" (number-to-string retmax)
				  "&query_key=" querykey
				  "&webenv=" webenv
				  (when (not (string-empty-p pubmed-api-key))
				    (concat "&api_key=" pubmed-api-key)))))
    (url-retrieve pubmed-esummary-url #'pubmed--parse-esummary)))

(defun pubmed--parse-esummary (status)
  "Check STATUS and parse the JSON object in the current buffer."
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
		 (articletitle (plist-get value :title))
		 (booktitle (plist-get value :booktitle))
		 (title (cond
		 	 ((not (string-empty-p articletitle))
		 	  articletitle)
		 	 ((not (string-empty-p booktitle))
		 	  booktitle)))
		 (entry (list (plist-get value :uid)
	    		      (vector (plist-get value :sortfirstauthor)
				      (pubmed---html-cleanup title)
	    			      (plist-get value :source)
				      (pubmed--parse-time-string (plist-get value :sortpubdate))))))
	    (push entry entries)))
	(pubmed--list (nreverse entries)))))))

(defun pubmed--parse-efetch (status)
  "Check STATUS and parse the XML object in the current buffer.
Show the result in the \"*PubMed-entry*\" buffer."
  (let ((url-error (plist-get status :error)))
    (cond
     (url-error
      (if (eq (cadr url-error) 'http)
      	  (error "HTTP Error %i: %s" (caddr url-error) (pubmed--get-url-error (caddr url-error)))
      	(signal (car url-error) (cdr url-error))))
     (t
      (let ((dom (libxml-parse-xml-region (1+ url-http-end-of-headers) (point-max)))
	    (pubmed-entry-buffer (get-buffer-create "*PubMed-entry*"))
	    (inhibit-read-only t)
	    summary)
	(with-current-buffer pubmed-entry-buffer
	  (pubmed-show-mode)
	  (erase-buffer)
	  (cond
	   ;; metadata associated with an article
	   ((setq summary (esxml-query "PubmedArticle" dom))
	    (insert (pubmed--summary-journal-isoabbreviation summary))
	    (if (s-suffix-p "." (pubmed--summary-journal-isoabbreviation summary))
		(insert " ")
	      (insert ". "))
	    (insert (pubmed--summary-pubdate summary))
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
	    (when (plist-get (pubmed--summary-articleid summary) 'pubmed)
	      (let ((heading "PMID: "))
	      	(put-text-property 0 (length heading) 'face 'bold heading)
	      	(insert heading))
      	      (insert (plist-get (pubmed--summary-articleid summary) 'pubmed) "\n"))
	    (when (plist-get (pubmed--summary-articleid summary) 'doi)
	      (let ((heading "DOI: "))
	      	(put-text-property 0 (length heading) 'face 'bold heading)
	      	(insert heading))
      	      (insert (plist-get (pubmed--summary-articleid summary) 'doi) "\n"))
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
		  (when (plist-get grant 'grantid)
      	      	    (insert (plist-get grant 'grantid))
      	      	    (insert "/"))
      	      	  (insert (plist-get grant 'agency))
	      	  (when (plist-get grant 'country)
      	      	    (insert "/")
      	      	    (insert (plist-get grant 'country)))
      	      	  (insert "\n"))))
	    (goto-char (point-min)))
	   ;; metadata associated with a bookchapter
	   ((and (setq summary (esxml-query "PubmedBookArticle" dom)) (setq articletitle (pubmed--summary-article-title summary)))
	    ;; (insert (pubmed--summary-pubdate summary))
	    ;; (insert "\n\n")
	    (put-text-property 0 (length articletitle) 'face 'bold articletitle)
	    (insert articletitle)
	    (insert "\n")
	    (when (setq authorlist (pubmed--summary-authors summary))
      	      (let (authors)
      		(dolist (author authorlist)
      	      	  (cond
      	      	   ((and (plist-get author 'lastname) (plist-get author 'initials))
      	      	    (push (concat (plist-get author 'lastname) " " (plist-get author 'initials)) authors))
      	      	   ((plist-get author 'collectivename)
      	      	    (push (plist-get author 'collectivename) authors))))
      		(insert (s-join ", " (nreverse authors)))
      		(insert "\n\n")))
	    (let* ((booktitle (pubmed--summary-book-title summary))
		   (publisher (pubmed--summary-publisher summary))
		   (medium (pubmed--summary-book-medium summary))
		   (reportnumber (pubmed--summary-book-reportnumber summary))
		   (location (plist-get publisher 'location))
		   (name (plist-get publisher 'name))
		   (pubdate (pubmed--summary-pubdate summary))
		   (beginningdate (format-time-string "%Y" (pubmed--summary-beginningdate summary)))
		   (endingdate (format-time-string "%Y" (pubmed--summary-endingdate summary))))
	      (when booktitle
		(insert booktitle))
	      (when medium
		(insert  " [" medium "]"))
	      (insert ". ")
	      (when location
		(insert location ": "))
	      (when name
		(insert name))
	      (cond
	       (beginningdate
		(insert  "; " beginningdate "."))
	       ((and beginningdate endingdate)
		(insert  "; " beginningdate "-" endingdate "."))
	       (pubdate
		(insert "; " pubdate "."))
	       (t
		(insert ".")))
	      (when reportnumber
		(insert " " reportnumber "."))
	      (insert "\n\n"))
	    (when (setq collectiontitle (pubmed--summary-book-collectiontitle summary))
	      (insert collectiontitle "\n\n"))
	    ;; (when (pubmed--summary-authors summary)
      	    ;;   (let ((authorlist (pubmed--summary-authors summary))
      	    ;;   	    authors)
      	    ;;   	(dolist (author authorlist)
      	    ;;   	  (cond
      	    ;;   	   ((and (plist-get author 'lastname) (plist-get author 'initials))
      	    ;;   	    (push (concat (plist-get author 'lastname) " " (plist-get author 'initials)) authors))
      	    ;;   	   ((plist-get author 'collectivename)
      	    ;;   	    (push (plist-get author 'collectivename) authors))))
      	    ;;   	(insert (s-join ", " (nreverse authors)))
      	    ;;   	(insert "\n\n")))
	    ;; (when (pubmed--summary-investigators summary)
      	    ;;   (let ((investigatorlist (pubmed--summary-investigators summary))
      	    ;;   	    investigators)
      	    ;;   	(dolist (investigator investigatorlist)
      	    ;;   	  (push (concat (plist-get investigator 'lastname) " " (plist-get investigator 'initials)) investigators))
      	    ;;   	(insert "Collaborators (" (number-to-string (length investigatorlist)) ")\n")
      	    ;;   	(insert (s-join ", " (nreverse investigators)))
      	    ;;   	(insert "\n\n")))
	    (when (pubmed--summary-abstract summary)
	      (let ((heading "EXCERPT:"))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert "\n")
      	      (insert (pubmed--summary-abstract summary))
      	      (insert "\n\n"))
	    (when (pubmed--summary-sections summary)
	      (let ((heading "SECTIONS:"))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert "\n")
      	      (insert (s-join "\n" (pubmed--summary-sections summary)))
      	      (insert "\n\n"))
	    (when (pubmed--summary-keywords summary)
	      (let ((heading "KEYWORDS: "))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert (s-join "; " (pubmed--summary-keywords summary)))
      	      (insert "\n\n"))
	    (when (pubmed--summary-book-isbn summary)
	      (let ((heading "ISBN: "))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert (pubmed--summary-book-isbn summary))
	      (insert "\n\n"))
	    (when (plist-get (pubmed--summary-articleid summary) 'pubmed)
	      (let ((heading "PMID: "))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert (plist-get (pubmed--summary-articleid summary) 'pubmed))
	      (insert "\n\n"))
	    ;; (when (pubmed--summary-mesh summary)
	    ;;   (let ((heading "MeSH terms:"))
	    ;; 	(put-text-property 0 (length heading) 'face 'bold heading)
	    ;; 	(insert heading))
      	    ;;   (insert "\n")
      	    ;;   (let ((meshheadinglist (pubmed--summary-mesh summary)))
      	    ;; 	;; Iterate over the meshheadings
      	    ;; 	(dolist (meshheading meshheadinglist)
      	    ;;   	  (let ((qualifiers (plist-get meshheading 'qualifiers)))
      	    ;;   	    ;; If the descriptor (or subject heading) has qualifiers (or subheadings)
      	    ;;   	    (if qualifiers
      	    ;;   		;; Iterate over the qualifiers
      	    ;;   		(dolist (qualifier qualifiers)
      	    ;;   		  ;; Insert "descriptor/qualifier"
      	    ;;   		  (insert (plist-get meshheading 'descriptor))
      	    ;;   		  (insert "/")
      	    ;;   		  (insert (plist-get qualifier 'qualifier))
      	    ;;   		  (insert "\n"))
      	    ;;   	      ;; If the descriptor (or subject heading) has no qualifiers (or subheadings)
      	    ;;   	      ;; Insert "descriptor"
      	    ;;   	      (insert (plist-get meshheading 'descriptor))
      	    ;;   	      (insert "\n\n"))))))
	    (when (pubmed--summary-publicationtype summary)
	      (let ((heading "Publication types:"))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading)
		(insert "\n"))
	      (mapcar (lambda (x) (insert (plist-get x 'type) "\n")) (pubmed--summary-publicationtype summary))
	      (insert "\n"))
	    (goto-char (point-min)))
	   ;; metadata associated with a book
	   ((setq summary (esxml-query "PubmedBookArticle" dom))
	    ;; (insert (pubmed--summary-pubdate summary))
	    ;; (insert "\n\n")
	    (let* ((booktitle (pubmed--summary-book-title summary))
		   (medium (pubmed--summary-book-medium summary)))
	      (put-text-property 0 (length booktitle) 'face 'bold booktitle)
	      (insert booktitle)
	      (when medium
		(insert  " [" medium "]"))
	      (insert ". ")
	      (insert "\n"))
	    (when (setq authorlist (pubmed--summary-authors summary))
      	      (let (authors)
      		(dolist (author authorlist)
      		  (cond
      	      	   ((and (plist-get author 'lastname) (plist-get author 'initials))
      	      	    (push (concat (plist-get author 'lastname) " " (plist-get author 'initials)) authors))
      	      	   ((plist-get author 'collectivename)
      	      	    (push (plist-get author 'collectivename) authors))))
      		(insert (s-join ", " (nreverse authors)))
      		(insert "\n\n")))
	    (let* ((publisher (pubmed--summary-publisher summary))
		   (reportnumber (pubmed--summary-book-reportnumber summary))
		   (location (plist-get publisher 'location))
		   (name (plist-get publisher 'name))
		   (pubdate (pubmed--summary-pubdate summary))
		   (beginningdate (format-time-string "%Y" (pubmed--summary-beginningdate summary)))
		   (endingdate (format-time-string "%Y" (pubmed--summary-endingdate summary))))
	      (when location
		(insert location ": "))
	      (when name
		(insert name))
	      (cond
	       (beginningdate
		(insert  "; " beginningdate "."))
	       ((and beginningdate endingdate)
		(insert  "; " beginningdate "-" endingdate "."))
	       (pubdate
		(insert "; " pubdate "."))
	       (t
		(insert ".")))
	      (when reportnumber
		(insert " " reportnumber "."))
	      (insert "\n\n"))
	    (when (setq collectiontitle (pubmed--summary-book-collectiontitle summary))
	      (insert collectiontitle "\n\n"))
	    ;; (when (pubmed--summary-authors summary)
      	    ;;   (let ((authorlist (pubmed--summary-authors summary))
      	    ;;   	    authors)
      	    ;;   	(dolist (author authorlist)
      	    ;;   	  (cond
      	    ;;   	   ((and (plist-get author 'lastname) (plist-get author 'initials))
      	    ;;   	    (push (concat (plist-get author 'lastname) " " (plist-get author 'initials)) authors))
      	    ;;   	   ((plist-get author 'collectivename)
      	    ;;   	    (push (plist-get author 'collectivename) authors))))
      	    ;;   	(insert (s-join ", " (nreverse authors)))
      	    ;;   	(insert "\n\n")))
	    ;; (when (pubmed--summary-investigators summary)
      	    ;;   (let ((investigatorlist (pubmed--summary-investigators summary))
      	    ;;   	    investigators)
      	    ;;   	(dolist (investigator investigatorlist)
      	    ;;   	  (push (concat (plist-get investigator 'lastname) " " (plist-get investigator 'initials)) investigators))
      	    ;;   	(insert "Collaborators (" (number-to-string (length investigatorlist)) ")\n")
      	    ;;   	(insert (s-join ", " (nreverse investigators)))
      	    ;;   	(insert "\n\n")))
	    (when (pubmed--summary-abstract summary)
	      (let ((heading "EXCERPT:"))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert "\n")
      	      (insert (pubmed--summary-abstract summary))
      	      (insert "\n\n"))
	    (when (pubmed--summary-sections summary)
	      (let ((heading "SECTIONS:"))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert "\n")
      	      (insert (s-join "\n" (pubmed--summary-sections summary)))
      	      (insert "\n\n"))
	    (when (pubmed--summary-keywords summary)
	      (let ((heading "KEYWORDS: "))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert (s-join "; " (pubmed--summary-keywords summary)))
      	      (insert "\n\n"))
	    (when (pubmed--summary-book-isbn summary)
	      (let ((heading "ISBN: "))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert (pubmed--summary-book-isbn summary))
	      (insert "\n\n"))
	    (when (plist-get (pubmed--summary-articleid summary) 'pubmed)
	      (let ((heading "PMID: "))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading))
      	      (insert (plist-get (pubmed--summary-articleid summary) 'pubmed))
	      (insert "\n\n"))
	    ;; (when (pubmed--summary-mesh summary)
	    ;;   (let ((heading "MeSH terms:"))
	    ;; 	(put-text-property 0 (length heading) 'face 'bold heading)
	    ;; 	(insert heading))
      	    ;;   (insert "\n")
      	    ;;   (let ((meshheadinglist (pubmed--summary-mesh summary)))
      	    ;; 	;; Iterate over the meshheadings
      	    ;; 	(dolist (meshheading meshheadinglist)
      	    ;;   	  (let ((qualifiers (plist-get meshheading 'qualifiers)))
      	    ;;   	    ;; If the descriptor (or subject heading) has qualifiers (or subheadings)
      	    ;;   	    (if qualifiers
      	    ;;   		;; Iterate over the qualifiers
      	    ;;   		(dolist (qualifier qualifiers)
      	    ;;   		  ;; Insert "descriptor/qualifier"
      	    ;;   		  (insert (plist-get meshheading 'descriptor))
      	    ;;   		  (insert "/")
      	    ;;   		  (insert (plist-get qualifier 'qualifier))
      	    ;;   		  (insert "\n"))
      	    ;;   	      ;; If the descriptor (or subject heading) has no qualifiers (or subheadings)
      	    ;;   	      ;; Insert "descriptor"
      	    ;;   	      (insert (plist-get meshheading 'descriptor))
      	    ;;   	      (insert "\n\n"))))))
	    (when (pubmed--summary-publicationtype summary)
	      (let ((heading "Publication types:"))
		(put-text-property 0 (length heading) 'face 'bold heading)
		(insert heading)
		(insert "\n"))
	      (mapcar (lambda (x) (insert (plist-get x 'type) "\n")) (pubmed--summary-publicationtype summary))
	      (insert "\n"))
	    (goto-char (point-min)))
	   (t
	    (insert "No summary available")))
	  (save-selected-window
	    (display-buffer pubmed-entry-buffer))))))))

(defun pubmed--summary-pmid (summary)
  "Return the PMID of the article SUMMARY."
  (esxml-query "PMID *" summary))

(defun pubmed--summary-datecompleted (summary)
  "Return the completed date of the article SUMMARY.
The time value of the date can be converted by `format-time-string' to a string according to FORMAT-STRING."
  (let* ((datecompleted (encode-time 0
				     0
				     0
				     (string-to-number (or (esxml-query "DateCompleted Day *" summary) "0"))
				     (string-to-number (or (esxml-query "DateCompleted Month *" summary) "0"))
				     (string-to-number (or (esxml-query "DateCompleted Year *" summary) "0")))))
    datecompleted))

(defun pubmed--summary-daterevised (summary)
  "Return the revised date value of the article SUMMARY.
The time value of the date can be converted by `format-time-string' to a string according to FORMAT-STRING."
  (let* ((daterevised (encode-time 0
				   0
				   0
				   (string-to-number (or (esxml-query "DateRevised Day *" summary) "0"))
				   (string-to-number (or (esxml-query "DateRevised Month *" summary) "0"))
				   (string-to-number (or (esxml-query "DateRevised Year *" summary) "0")))))
    daterevised))

(defun pubmed--summary-pubmodel (summary)
  "Return the publication model of the article SUMMARY."
  (esxml-node-attribute 'PubModel (esxml-query "Article" summary)))

(defun pubmed--summary-issn (summary)
  "Return a plist with the journal ISSN of the article SUMMARY.
The plist has the form \"('issn ISSN 'type TYPE)\"."
  (let ((issn (esxml-query "Journal ISSN *" summary))
	(type (esxml-node-attribute 'IssnType (esxml-query "Journal ISSN" summary))))
    (list 'issn issn 'type type)))

(defun pubmed--summary-journal-issue (summary)
  "Return a plist of the journal issue of the article SUMMARY.
The plist has the form \"('year YEAR 'season SEASON 'issue ISSUE
'volume VOLUME 'citedmedium CITEDMEDIUM)\"."
  (let* ((year (esxml-query "JournalIssue Year *" summary))
	 (season (esxml-query "JournalIssue Season *" summary))
	 (issue (esxml-query "JournalIssue Issue *" summary))
	 (volume (esxml-query "JournalIssue Volume *" summary))
	 (citedmedium (esxml-node-attribute 'CitedMedium (esxml-query "JournalIssue" summary))))
    (list 'year year 'season season 'issue issue 'volume volume 'citedmedium citedmedium)))

(defun pubmed--summary-pubdate (summary)
  "Return a string with the publication date of the article SUMMARY."
  (let* ((day (esxml-query "PubDate Day *" summary))
	 (month (esxml-query "PubDate Month *" summary))
	 (year (esxml-query "PubDate Year *" summary))
	 (medlinedate (esxml-query "PubDate MedlineDate *" summary)))
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
  (esxml-query "ArticleTitle *" summary))

(defun pubmed--summary-book-title (summary)
  "Return the title of the book SUMMARY."
  (esxml-query "BookTitle *" summary))

(defun pubmed--summary-pagination (summary)
  "Return the pagination of the article SUMMARY."
  (cond
   ((and (esxml-query "Pagination StartPage *" summary) (esxml-query "Pagination EndPage *" summary))
    (concat (esxml-query "Pagination StartPage *" summary) "--" (esxml-query "Pagination EndPage *" summary)))
   ((esxml-query "Pagination MedlinePgn *" summary)
    (esxml-query "Pagination MedlinePgn *" summary))
   (t
    nil)))

(defun pubmed--summary-elocation (summary)
  "Return an plist of Elocation IDs of the article SUMMARY.
The plist has the form \"('type TYPE 'id ID)\"."
  (let* ((elocationidlist (esxml-query-all "ELocationID" (esxml-query "Article" summary)))
	 elocationids)
    (dolist (elocationid elocationidlist elocationids)
      (let* ((type (esxml-node-attribute 'EIdType elocationid))
	     (id (car (esxml-node-children elocationid))))
    	(push (list 'type type 'id id) elocationids)))
    (nreverse elocationids)))

(defun pubmed--summary-abstract (summary)
  "Return the abstract of the article SUMMARY.
Return nil if no abstract is available."
  (let (texts)
    (when (setq abstract (esxml-query-all "AbstractText" (esxml-query "Abstract" summary)))
      ;; Iterate through AbstractText nodes, where structure is like: (AbstractText ((Label . "LABEL") (NlmCategory . "CATEGORY")) "ABSTRACTTEXT")
      (dolist (node abstract)
	(when (setq label (esxml-node-attribute 'Label node))
	  (push (s-trim label) texts))
	;; Abstracts of books contain headings in <b> or <i> tags in stead of a Label or NlmCategory attributes
	(dolist (child (esxml-node-children node))
	  (cond
	   ;; Treat as a paragraph if child is a string
	   ((stringp child)
	    (push (s-trim child) texts))
	   ;; Treat as a heading if child is a cons
	   ((consp child)
	    (cond
	     ((setq heading (esxml-query "b *" child))
	      (push (s-trim heading) texts))
	     ((setq heading (esxml-query "i *" child))
	      (push (s-trim heading) texts)))))))
      (s-join "\n" (nreverse texts)))))

(defun pubmed--summary-authors (summary)
  "Return an plist with the authors of the article SUMMARY.
Each list element corresponds to one author, and is a plist with the form \"('lastname LASTNAME 'forename FORENAME 'initials INITIALS 'affiliationinfo AFFILIATIONINFO 'collectivename COLLECTIVENAME)\"."
  (let ((authorlist (esxml-query-all "Author" (esxml-query "AuthorList" summary)))
	authors)
    (dolist (author authorlist)
      (let ((lastname (esxml-query "LastName *" author))
	    (forename (esxml-query "ForeName *" author))
    	    (initials (esxml-query "Initials *" author))
	    (affiliationinfo (esxml-query "AffiliationInfo Affiliation *" author))
    	    (collectivename (esxml-query "CollectiveName *" author)))
    	(push (list 'lastname lastname 'forename forename 'initials initials 'affiliationinfo affiliationinfo 'collectivename collectivename) authors)))
    (nreverse authors)))

(defun pubmed--summary-editors (summary)
  "Return an plist with the editors of the article SUMMARY.
Each list element corresponds to one editor, and is a plist with the form \"('lastname LASTNAME 'forename FORENAME 'initials INITIALS 'affiliationinfo AFFILIATIONINFO 'collectivename COLLECTIVENAME)\"."
  (let ((editorlist (esxml-query-all "Editor" (esxml-query "EditorList" summary)))
	editors)
    (dolist (editor editorlist)
      (let ((lastname (esxml-query "LastName *" editor))
	    (forename (esxml-query "ForeName *" editor))
    	    (initials (esxml-query "Initials *" editor))
	    (affiliationinfo (esxml-query "AffiliationInfo Affiliation *" editor))
    	    (collectivename (esxml-query "CollectiveName *" editor)))
    	(push (list 'lastname lastname 'forename forename 'initials initials 'affiliationinfo affiliationinfo 'collectivename collectivename) editors)))
    (nreverse editors)))

(defun pubmed--summary-language (summary)
  "Return the language of the article SUMMARY."
  (esxml-query "Language *" summary))

(defun pubmed--summary-grant (summary)
  "Return a list of the grants of the article SUMMARY.
Each list element corresponds to one grant, and is a plist with the form \"('grantid GRANTID 'acronym ACRONYM 'agency AGENCY 'country COUNTRY)\"."
  (let ((grantlist (esxml-query-all "Grant" (esxml-query "Article GrantList" summary)))
	grants) ;; make sure list starts empty
    (dolist (grant grantlist)
      (let ((grantid (esxml-query "GrantID *" grant))
	    (acronym (esxml-query "Acronym *" grant))
	    (agency (esxml-query "Agency *" grant))
	    (country (esxml-query "Country *" grant)))
	(push (list 'grantid grantid 'acronym acronym 'agency agency 'country country) grants)))
    (nreverse grants)))

(defun pubmed--summary-publicationtype (summary)
  "Return a plist of the publication types of the article SUMMARY.
The plist has the form \"('type TYPE 'ui UI)\"."
  ;; Iterate through PublicationType nodes, where structure is like: (PublicationType ((UI . "UI")) "PUBLICATIONTYPE")
  (let ((publicationtypelist (esxml-query-all "PublicationType" summary))
	publicationtypes) ;; make sure list starts empty
    (dolist (publicationtype publicationtypelist)
      (let ((type (car (esxml-node-children publicationtype)))
    	    (ui (esxml-node-attribute 'UI publicationtype)))
	;; For each `publicationtype' push the type to the list `publicationtypes'
	;; Ignore the UI
	(push (list 'type type 'ui ui) publicationtypes)))
    (nreverse publicationtypes)))

(defun pubmed--summary-articledate (summary)
  "Return a plist of article date of the article SUMMARY.
The plist has the form \"('type TYPE 'date date)\". The time
value of the date can be converted by `format-time-string' to a
string according to FORMAT-STRING."
  (let ((type (esxml-node-attribute 'DateType (esxml-query "ArticleDate" summary)))
	(date (encode-time 0
			   0
			   0
			   (string-to-number (or (esxml-query "ArticleDate Day *" summary) "0"))
			   (string-to-number (or (esxml-query "ArticleDate Month *" summary) "0"))
			   (string-to-number (or (esxml-query "ArticleDate Year *" summary) "0")))))
    (list 'type type 'date date)))

(defun pubmed--summary-medlinejournalinfo (summary)
  "Return a plist with Medline journal info of the article SUMMARY.
The plist has the form \"('country COUNTRY 'medlineta MEDLINETA
'nlmuniqueid NLMUNIQUEID 'issnlinking ISSNLINKING)\"."
  (let ((country (esxml-query "MedlineJournalInfo Country *" summary))
	(medlineta (esxml-query "MedlineJournalInfo MedlineTA *" summary))
	(nlmuniqueid (esxml-query "MedlineJournalInfo NlmUniqueID *" summary))
	(issnlinking (esxml-query "MedlineJournalInfo ISSNLinking *" summary)))
    (list 'country country 'medlineta medlineta 'nlmuniqueid nlmuniqueid 'issnlinking issnlinking)))

(defun pubmed--summary-substances (summary)
  "Return a plist of the chemical substances of the article SUMMARY.
Each list element corresponds to one substance, and is a plist
with the form \"('registrynumber REGISTRYNUMBER 'substance
SUBSTANCE 'ui UI)\"."
  (let ((chemicallist (esxml-query-all "Chemical" (esxml-query "ChemicalList" summary)))
	chemicals)
    (dolist (chemical chemicallist)
      (let ((registrynumber (esxml-query "RegistryNumber *" chemical)) ; RegistryNumber is ignored
	    (substance (esxml-query "NameOfSubstance *" chemical))
	    (ui (esxml-node-attribute 'UI (esxml-query "NameOfSubstance" chemical))))
	(push (list 'registrynumber registrynumber 'substance substance 'ui ui) chemicals)))
    (nreverse chemicals)))

(defun pubmed--summary-commentscorrections (summary)
  "Return the correction of the article SUMMARY.
The plist has the form \"('reftype REFTYPE 'refsource REFSOURCE 'pmid PMID)\"."
  (let ((commentscorrectionslist (esxml-query-all "CommentsCorrections" (esxml-query "CommentsCorrectionsList" summary)))
	commentscorrections)
    (dolist (commentscorrection commentscorrectionslist)
      (let ((reftype (esxml-node-attribute 'RefType commentscorrection))
	    (refsource (esxml-query "RefSource *" commentscorrection))
	    (pmid (esxml-query "PMID *" commentscorrection)))
	;; For each `commentscorrection' push the reftype, refsource and pmid to the list `commentscorrections'
    	(push (list 'reftype reftype 'refsource refsource 'pmid pmid) commentscorrections)))
    (nreverse commentscorrections)))

(defun pubmed--summary-mesh (summary)
  "Return an list of the MeSH terms  of the article SUMMARY.
Each list element corresponds to one descriptor (or subject heading) and its qualifiers (or subheadings), and is a plist with the form \"('descriptor DESCRIPTOR 'ui UI 'qualifiers (('qualifier QUALIFIER 'ui UI) ('qualifier QUALIFIER 'ui UI) (...)))\"."
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

(defun pubmed--summary-keywords (summary)
  "Return an alist of the keywords of article SUMMARY."
  (let ((keywordlist (esxml-query-all "Keyword" (esxml-query "KeywordList" summary)))
	keywords)
    (dolist (keyword keywordlist)
      (push (car (esxml-node-children keyword)) keywords))
    (nreverse keywords)))

(defun pubmed--summary-coistatement (summary)
  "Return a string with the conflict of interest statement of article SUMMARY."
  (esxml-query "CoiStatement *" summary))

(defun pubmed--summary-history (summary)
  "Return a plist of the history of article SUMMARY.
The plist contains the dates indicating the history of the article's publication, i.e. when it was received, accepted, revised, or retracted. The plist has the form \"('pubstatus PUBSTATUS 'year YEAR 'month MONTH 'day DAY)\"."
  (let ((pubdatelist (esxml-query-all "PubMedPubDate" (esxml-query "History" summary)))
	pubdates)
    (dolist (pubdate pubdatelist)
      (let ((pubstatus (esxml-node-attribute 'PubStatus pubdate))
	     (year (esxml-query "Year *" pubdate))
	 (month (esxml-query "Month *" pubdate))
	 (day (esxml-query "Day *" pubdate))
	 (hour (esxml-query "Hour *" pubdate))
	 (minute (esxml-query "Minute *" pubdate)))
	    (push (list 'pubstatus pubstatus 'year year 'month month 'day day) pubdates)))
    (nreverse pubdates)))

(defun pubmed--summary-publicationstatus (summary)
  "Return the publication status of article SUMMARY."
  (esxml-query "PublicationStatus *" summary))

(defun pubmed--summary-articleid (summary)
  "Return an plist of the article IDs of the article SUMMARY.
The plist has the form \"('pubmed pubmed 'doi DOI 'pii PII 'pmc PMC 'mid MID)\"."
  (let ((articleidlist (esxml-query-all "ArticleId" (esxml-query "ArticleIdList" summary)))
	articleids)
    (dolist (articleid articleidlist)
      (let ((idtype (intern (esxml-node-attribute 'IdType articleid)))
	    (id (car (esxml-node-children articleid))))
	(push idtype articleids)
	(push id articleids)))
    (nreverse articleids)))

(defun pubmed--summary-references (summary)
  "Return a plist of the references of the article SUMMARY.
Each list element corresponds to one reference, The has the form \"('citation CITATION 'pubmed PMID)\"."
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

(defun pubmed--summary-investigators (summary)
  "Return an plist with the investigators of the article SUMMARY.
Each list element corresponds to one investigator, and is a plist with the form \"('lastname LASTNAME 'forename FORENAME 'initials INITIALS)\"."
  (let ((investigatorlist (esxml-query-all "Investigator" (esxml-query "InvestigatorList" summary)))
	investigators)
    (dolist (investigator investigatorlist)
      (let ((lastname (esxml-query "LastName *" investigator))
	    (forename (esxml-query "ForeName *" investigator))
    	    (initials (esxml-query "Initials *" investigator)))
    	(push (list 'lastname lastname 'forename forename 'initials initials) investigators)))
    (nreverse investigators)))

(defun pubmed--summary-chapter (summary)
  "Return a string with the chapter of book SUMMARY."
  (esxml-query "LocationLabel[Type=chapter] *" summary))

(defun pubmed--summary-publisher (summary)
  "Return a plist of the publisher of book SUMMARY.
The plist has the form \"('name NAME 'location LOCATION)\"."
  (let ((name (esxml-query "Publisher PublisherName *" summary))
	(location (esxml-query "Publisher PublisherLocation *" summary)))
    (list 'name name 'location location)))

;; FIXME: return a plist instead of an encoded time
(defun pubmed--summary-beginningdate (summary)
  "Return the beginning date value of the article SUMMARY.
The time value of the date can be converted by `format-time-string' to a string according to FORMAT-STRING."
  (let* ((beginningdate (encode-time 0
				     0
				     0
				     (string-to-number (or (esxml-query "BeginningDate Day *" summary) "0"))
				     (string-to-number (or (esxml-query "BeginningDate Month *" summary) "0"))
				     (string-to-number (or (esxml-query "BeginningDate Year *" summary) "0")))))
    beginningdate))

;; FIXME: return a plist instead of an encoded time
(defun pubmed--summary-endingdate (summary)
  "Return the ending date value of the article SUMMARY.
The time value of the date can be converted by `format-time-string' to a string according to FORMAT-STRING."
  (let* ((endingdate (encode-time 0
				  0
				  0
				  (string-to-number (or (esxml-query "EndingDate Day *" summary) "0"))
				  (string-to-number (or (esxml-query "EndingDate Month *" summary) "0"))
				  (string-to-number (or (esxml-query "EndingDate Year *" summary) "0")))))
    endingdate))

(defun pubmed--summary-book-authors (summary)
  "Return an plist with the authors of the article SUMMARY.
Each list element corresponds to one author, and is a plist with the form \"('lastname LASTNAME 'forename FORENAME 'initials INITIALS 'affiliationinfo AFFILIATIONINFO 'collectivename COLLECTIVENAME)\"."
  (let ((authorlist (esxml-query-all "Author" (esxml-query "AuthorList[Type=authors]" summary)))
	authors)
    (dolist (author authorlist)
      (let ((lastname (esxml-query "LastName *" author))
	    (forename (esxml-query "ForeName *" author))
    	    (initials (esxml-query "Initials *" author))
	    (affiliationinfo (esxml-query "AffiliationInfo Affiliation *" author))
    	    (collectivename (esxml-query "CollectiveName *" author)))
    	(push (list 'lastname lastname 'forename forename 'initials initials 'affiliationinfo affiliationinfo 'collectivename collectivename) authors)))
    (nreverse authors)))

(defun pubmed--summary-book-editors (summary)
  "Return an plist with the editors of SUMMARY.
Each list element corresponds to one author, and is a plist with the form \"('lastname LASTNAME 'forename FORENAME 'initials INITIALS 'affiliationinfo AFFILIATIONINFO 'collectivename COLLECTIVENAME)\"."
  (let ((editorlist (esxml-query-all "Author" (esxml-query "AuthorList[Type=editors]" summary)))
	editors)
    (dolist (editor editorlist)
      (let ((lastname (esxml-query "LastName *" editor))
	    (forename (esxml-query "ForeName *" editor))
    	    (initials (esxml-query "Initials *" editor))
	    (affiliationinfo (esxml-query "AffiliationInfo Affiliation *" editor))
    	    (collectivename (esxml-query "CollectiveName *" editor)))
    	(push (list 'lastname lastname 'forename forename 'initials initials 'affiliationinfo affiliationinfo 'collectivename collectivename) editors)))
    (nreverse editors)))

(defun pubmed--summary-book-edition (summary)
  "Return a string with the edition of SUMMARY."
  (esxml-query "Edition *" summary))

(defun pubmed--summary-book-collectiontitle (summary)
  "Return a string with the collection title of SUMMARY."
  (esxml-query "CollectionTitle *" summary))

(defun pubmed--summary-book-isbn (summary)
  "Return a string with the isbn of SUMMARY."
  (esxml-query "Isbn *" summary))

(defun pubmed--summary-book-medium (summary)
  "Return a string with the medium of SUMMARY."
  (esxml-query "Medium *" summary))

(defun pubmed--summary-book-reportnumber (summary)
  "Return a string with the reportnumber of SUMMARY.
The reportnummber provides an identifier assigned to scientific or technical reports by the publishing organization. May be contained in: <Book>"
  (esxml-query "ReportNumber *" summary))

(defun pubmed--summary-sections (summary)
  "Return a plist of the sections of the book SUMMARY."
  (let ((sectionlist (esxml-query-all "Section" (esxml-query "Sections" summary)))
	sections)
    (dolist (section sectionlist)
      (push (esxml-query "SectionTitle *" section) sections))
    (nreverse sections)))

(defun pubmed--summary-contributiondate (summary)
  "Return the contribution date value of the article SUMMARY.
The time value of the date can be converted by `format-time-string' to a string according to FORMAT-STRING."
  (let* ((contributiondate (encode-time 0
					0
					0
					(string-to-number (or (esxml-query "ContributionDate Day *" summary) "0"))
					(string-to-number (or (esxml-query "ContributionDate Month *" summary) "0"))
					(string-to-number (or (esxml-query "ContributionDate Year *" summary) "0")))))
    contributiondate))

(defun pubmed--fulltext (uid)
  "Try to fetch the fulltext PDF of UID, using multiple methods.
The functions in `pubmed-fulltext-functions' are tried in order, until a fulltext PDF is found."
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
		    (message "Trying %S...done" (nth i pubmed-fulltext-functions))
		    (setq i (1+ i))
		    (if result
			(when (bufferp result)
			  (pubmed--view-pdf result))
		      (deferred:next self)))))))))))))

(defun pubmed--view-pdf (buffer)
  "Create a temporary pdf file containing BUFFER and open it with the default pdf viewer."
  (let ((data (with-current-buffer buffer (buffer-substring (1+ url-http-end-of-headers) (point-max))))
	(tempfile (make-nearby-temp-file pubmed-temp-prefix nil ".pdf")))
    (with-temp-file tempfile
      (set-buffer-file-coding-system 'binary)
      (insert data))
    (find-file-other-frame tempfile)))

(defun pubmed--sentence-at-point ()
  "Return the sentence at point."
  (let* ((current-line (line-number-at-pos))
	 sentence-line
	 (sentence
	  (save-excursion
	    (backward-sentence 1)
	    (setq sentence-line (line-number-at-pos))
	    ;; don't span prefix over following lines
	    (when (thing-at-point 'sentence)
	      (replace-regexp-in-string
	       ".*?\\([[:alnum:]][[:space:][:alnum:]]*\\)"
	       "\\1"
	       (replace-regexp-in-string "\\(.*\\)[ \t\n]*.*" "\\1" (thing-at-point 'sentence)))))))
    (or (if (eq sentence-line current-line) sentence) (thing-at-point 'word)))) ;fallback to word

(defun pubmed--completion-candidates (string)
  "Return a list of NCBI autocomplete suggestions matching STRING."
  (let* ((url (format pubmed-completion-url (url-hexify-string string)))
	 (response (with-temp-buffer (url-insert-file-contents url)(buffer-string)))
	 ;; Return the positions of the words delimited by double quotes
	 (matched-positions (s-matched-positions-all "\"\\([[:word:][:ascii:][:space:]]+?\\)\"" response 1))
	 ;; Return a list of words
	 (matched-words (mapcar (lambda (x) (substring response (car x) (cdr x))) matched-positions))
	 ;; Remove the prefix and keep the autocomplete suggestions
	 (candidates (cdr matched-words)))
    candidates))

(defun pubmed--author-completion-candidates (string)
  "Return a list of NCBI autocomplete suggestions matching STRING."
  (let* ((url (format pubmed-author-completion-url (url-hexify-string string)))
	 (response (with-temp-buffer (url-insert-file-contents url)(buffer-string)))
	 ;; Return the positions of the words delimited by double quotes
	 (matched-positions (s-matched-positions-all "\"\\([[:word:][:ascii:][:space:]]+?\\)\"" response 1))
	 ;; Return a list of matched items, containing matched authors bounded by an @-sign.
	 (matched-items (mapcar (lambda (x) (substring response (car x) (cdr x))) matched-positions))
	 ;; Return a list of matched authors
	 (matched-authors (mapcan (lambda (x) (s-split "@" x)) matched-items))
	 ;; Remove the prefix and keep the autocomplete suggestions
	 (candidates (cdr matched-authors)))
    candidates))

(defun pubmed--journal-completion-candidates (string)
  "Return a list of NCBI autocomplete suggestions matching STRING."
  (let* ((url (format pubmed-journal-completion-url (url-hexify-string string)))
	 (response (with-temp-buffer (url-insert-file-contents url)(buffer-string)))
	 ;; Return the positions of the words delimited by double quotes
	 (matched-positions (s-matched-positions-all "\"\\([[:word:][:ascii:][:space:]]+?\\)\"" response 1))
	 ;; Return a list of matched items, containing matched journals bounded by an @-sign.
	 (matched-items (mapcar (lambda (x) (substring response (car x) (cdr x))) matched-positions))
	 ;; Return a list of matched journals
	 (matched-journals (mapcan (lambda (x) (s-split "@" x)) matched-items))
	 ;; Remove the prefix and keep the autocomplete suggestions
	 (candidates (cdr matched-journals)))
    candidates))

;;;; Footer

(provide 'pubmed)

;;; pubmed.el ends here
