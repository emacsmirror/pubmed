;;; pubmed-bibtex.el --- Export BibTex from PubMed -*- lexical-binding: t; -*-

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

;; Translate the PubMed document summaries (DocSums) to BibTeX
;; entries. PubMed includes citations for journal articles and a
;; subset of books and book chapters available on the NCBI Bookshelf.
;; These are declared as @article, @book, and @incollection,
;; respectively.

;; Choosing BibTeX field types

;; The @article reference type is used for an article from a magazine
;; or a journal. Required fields are: author, title, journal, year.
;; Optional fields are: volume, number, pages, month, note.

;; The @book reference type is used for a published book. Required
;; fields are: author/editor, title, publisher, year. Optional fields
;; are: volume/number, series, address, edition, month, note.

;; The @incollection is used for a section of a book having its own
;; title. Required fields are: author, title, booktitle, publisher,
;; year. Optional fields are: editor, volume/number, series, type,
;; chapter, pages, address, edition, month, note.

;; Of the optional fields, the volume, number and type are not
;; included in the PubMed DocSums of the @books and @incollection
;; references. When the note field is enabled, it is set to an empty
;; value for later use. Besides the required and optional fields, the
;; following non-standard fields can be included in the BibTeX
;; entries: issn, pubmed, pii, doi, url.

;; The optional and non-standard fields can be included by setting the
;; values of the corresponding variables in your init.el or .emacs,
;; e.g. (setq pubmed-bibtex-article-number t)

;; Choosing BibTeX citation key

;; The first key of a BibTeX entry the citation key, or the BibTeX
;; key. This key must be unique for all entries in your bibliography.
;; It is this identifier that you will use within your document to
;; cross-reference it to this entry (i.e., the identifier you use in
;; the \cite{} command in your LaTeX file).

;; By default, the author-year citation style is used that follows the
;; loose standard in which the author's surname is followed by the
;; year of publication.

;; A formatter pattern language is provided that mostly follows the
;; JabRef key formatting syntax. The default author-year citation
;; style is produced by the [auth][year] key pattern. If the key is
;; not unique in the current database, it is made unique by adding one
;; of the letters a-z until a unique key is found.

;; Configuring default BibTeX file

;; If you have a master BibTeX file, e.g. bibliography.bib, and want
;; it to serve as the default file to append or write the BibTeX
;; reference to, you can set `pubmed-bibtex-default-file' in your
;; init.el or .emacs: (setq pubmed-bibtex-default-file
;; "/path/to/bibliography.bib")

;;; Code:

;;;; Requirements

(require 'bibtex)
(require 'json)
(require 's)
(require 'unidecode)

;;;; Variables

(defvar pubmed-bibtex-citation-keys nil
  "List of citation keys.")

(defvar pubmed-bibtex-key-functions
  `(("auth" . ,#'pubmed-bibtex-key--auth)
    ("authors" . ,#'pubmed-bibtex-key--authors)
    ("authorLast" . ,#'pubmed-bibtex-key--authorlast)
    ("authorsAlpha" . ,#'pubmed-bibtex-key--authorsalpha)
    ("authini" . ,#'pubmed-bibtex-key--authini)
    ("authorIni" . ,#'pubmed-bibtex-key--authorini)
    ("auth.auth.ea" . ,#'pubmed-bibtex-key--auth.auth.ea)
    ("auth.etal" . ,#'pubmed-bibtex-key--auth.etal)
    ("authEtAl" . ,#'pubmed-bibtex-key--authetal)
    ("authshort" . ,#'pubmed-bibtex-key--authshort)
    ("authForeIni" . ,#'pubmed-bibtex-key--authforeini)
    ("authorLastForeIni" . ,#'pubmed-bibtex-key--authorlastforeini)
    ("camel" . ,#'pubmed-bibtex-key--camel)
    ("chapter" . ,#'pubmed--summary-chapter)
    ("doi" . ,(lambda (summary) (plist-get (pubmed--summary-articleid summary) 'doi)))
    ("edition" . ,#'pubmed--summary-book-edition)
    ("editorIni" . ,#'pubmed-bibtex-key--editorini)
    ("editorLast" . ,#'pubmed-bibtex-key--editorlast)
    ("editorLastForeIni" . ,#'pubmed-bibtex-key--editorlastforeini)
    ("editors" . ,#'pubmed-bibtex-key--editors)
    ("editorsAlpha" . ,#'pubmed-bibtex-key--editorsalpha)
    ("edtr" . ,#'pubmed-bibtex-key--edtr)
    ("edtr.etal" . ,#'pubmed-bibtex-key--edtr.etal)
    ("edtrEtAl" . ,#'pubmed-bibtex-key--edtretal)
    ("edtr.edtr.ea" . ,#'pubmed-bibtex-key--edtr.edtr.ea)
    ("edtrForeIni" . ,#'pubmed-bibtex-key--edtrforeini)
    ("edtrIni" . ,#'pubmed-bibtex-key--edtrini)
    ("edtrshort" . ,#'pubmed-bibtex-key--edtrshort)
    ("pureauth" . ,#'pubmed-bibtex-key--pureauth)
    ("pureauth.auth.ea" . ,#'pubmed-bibtex-key--pureauth.auth.ea)
    ("pureauth.etal" . ,#'pubmed-bibtex-key--pureauth.etal)
    ("pureauthEtAl" . ,#'pubmed-bibtex-key--pureauthetal)
    ("pureauthForeIni" . ,#'pubmed-bibtex-key--pureauthforeini)
    ("pureauthIni" . ,#'pubmed-bibtex-key--pureauthini)
    ("pureauthorIni" . ,#'pubmed-bibtex-key--pureauthorini)
    ("pureauthorLast" . ,#'pubmed-bibtex-key--pureauthorlast)
    ("pureauthorLastForeIni" . ,#'pubmed-bibtex-key--pureauthorlastforeini)
    ("pureauthors" . ,#'pubmed-bibtex-key--pureauthors)
    ("pureauthorsAlpha" . ,#'pubmed-bibtex-key--pureauthorsalpha)
    ("pureauthshort" . ,#'pubmed-bibtex-key--pureauthshort)
    ("firstpage" . ,#'pubmed-bibtex-key--firstpage)
    ("isbn" . ,#'pubmed--summary-book-isbn)
    ("issn" . ,(lambda (summary) (plist-get (pubmed--summary-issn summary) 'issn)))
    ("issue" . ,(lambda (summary) (plist-get (pubmed--summary-journal-issue summary) 'issue)))
    ("journal" . ,#'pubmed--summary-journal-isoabbreviation)
    ("keyword" . ,#'pubmed-bibtex-key--keyword)
    ("language" . ,#'pubmed--summary-language)
    ("lastpage" . ,#'pubmed-bibtex-key--lastpage)
    ("medium" . ,#'pubmed--summary-book-medium)
    ("mid" . ,(lambda (summary) (plist-get (pubmed--summary-articleid summary) 'mid)))
    ("month" . ,#'pubmed-bibtex-key--month)
    ("pageprefix" . ,#'pubmed-bibtex-key--pageprefix)
    ("pages" . ,#'pubmed--summary-pagination)
    ("pii" . ,(lambda (summary) (plist-get (pubmed--summary-articleid summary) 'pii)))
    ("pmid" . ,#'pubmed--summary-pmid)
    ("publisher" . ,#'pubmed--summary-publisher)
    ("pubmed" . ,(lambda (summary) (plist-get (pubmed--summary-articleid summary) 'pubmed)))
    ("pubmodel" . ,#'pubmed--summary-pubmodel)
    ("series" . ,#'pubmed--summary-book-collectiontitle)
    ("shorttitle" . ,#'pubmed-bibtex-key--shorttitle)
    ("shortyear" . ,#'pubmed-bibtex-key--shortyear)
    ("title" . ,#'pubmed-bibtex-key--title)
    ("veryshorttitle" . ,#'pubmed-bibtex-key--veryshorttitle)
    ("volume" . ,(lambda (summary) (plist-get (pubmed--summary-journal-issue summary) 'volume)))
    ("year" . ,#'pubmed-bibtex-key--year)
    ("abbr" . ,#'pubmed-bibtex-key--abbr)
    ("alphanum" . ,#'pubmed-bibtex-key--alphanum)
    ("ascii" . ,#'pubmed-bibtex-key--ascii)
    ("capitalize" . ,#'upcase-initials)
    ("clean" . ,#'pubmed-bibtex-key--clean)
    ("condense" . ,#'pubmed-bibtex-key--condense)
    ("fold" . ,#'unidecode)
    ("lower" . ,#'downcase)
    ("nopunct" . ,#'pubmed-bibtex-key--nopunct)
    ("nopunctordash" . ,#'pubmed-bibtex-key--nopunctordash)
    ("postfix" . ,#'pubmed-bibtex-key--postfix)
    ("sentencecase" . ,#'s-capitalize)
    ("prefix" . ,#'pubmed-bibtex-key--prefix)
    ("replace" . ,#'pubmed-bibtex-key--replace)
    ("select" . ,#'pubmed-bibtex-key--select)
    ("skipwords" . ,#'pubmed-bibtex-key--skipwords)
    ("substring" . ,#'pubmed-bibtex-key--substring)
    ("titlecase" . ,#'pubmed-bibtex-key--titlecase)
    ("upper" . ,#'upcase)
    ("placeholder" . ,#'pubmed-bibtex-key--placeholder)))

;;;; Customization

(defgroup pubmed-bibtex nil
  "Export PubMed entries to BibTeX."
  :group 'pubmed)

;; TODO: add regexp
;; TODO: add support for flags
;; TODO: add support for multiple filters
(defcustom pubmed-bibtex-keypattern "[auth][year]"
  "Default BibTeX key pattern.
The formatter pattern language mostly follows the JabRef key
formatting syntax. The default key pattern is [auth][year], and
this could produce keys like e.g. Yared1998 If the key is not
unique in the current database, it is made unique by adding one
of the letters a-z until a unique key is found. Thus, the labels

Yared1998 Yared1998a Yared1998b

Special field markers

Several special field markers are offered, which extract only a
specific part of a field.

Author-related key patterns

[auth]: The last name of the first author

[authors]: The last name of all authors

[authorLast]: The last name of the last author

[authorsN]: The last name of up to N authors. If there are more
authors, \"EtAl\" is appended.

[authorsAlpha]: Corresponds to the BibTeX style \"alpha\". One
author: First three letters of the last name. Two to four
authors: First letters of last names concatenated. More than four
authors: First letters of last names of first three authors
concatenated. \"+\" at the end.

[authIniN]: The beginning of each author's last name, using no
more than N characters.

[authorIni]: The first 5 characters of the first author's last
name, and the last name initials of the remaining authors.

[authN]: The first N characters of the first author's last name

[authN_M]: The first N characters of the Mth author's last name

[auth.auth.ea]: The last name of the first two authors, and
\".ea\" if there are more than two.

[auth.etal]: The last name of the first author, and the last name
of the second author if there are two authors or \".etal\" if
there are more than two.

[authEtAl]: The last name of the first author, and the last name
of the second author if there are two authors or \"EtAl\" if
there are more than two. This is similar to auth.etal. The
difference is that the authors are not separated by \".\" and in
case of more than 2 authors \"EtAl\" instead of \".etal\" is
appended.

[authshort]: The last name if one author is given; the first
character of up to three authors' last names if more than one
author is given. A plus character is added, if there are more
than three authors.

[authForeIni]: The forename initial of the first author.

[authorLastForeIni]: The forename initial of the last author.

Note: If there is no author (as in the case of an edited book), then
all of the above [auth...] markers will use the editor(s) (if any) as
a fallback. Thus, the editor(s) of a book with no author will be
treated as the author(s) for label-generation purposes. If you do not
want this behaviour, i.e. you require a marker which expands to
nothing if there is no author, use pureauth instead of auth in the
above codes. For example, [pureauth], or [pureauthors3].

Editor-related key patterns

[edtr]: The last name of the first editor

[edtrIniN]: The beginning of each editor's last name, using no
more than N characters

[editors]: The last name of all editors

[editorLast]: The last name of the last editor

[editorIni]: The first 5 characters of the first editor's last
name, and the last name initials of the remaining editors.

[edtrN]: The first N characters of the first editor's last name

[edtrN_M]: The first N characters of the Mth editor's last name

[edtr.edtr.ea]: The last name of the first two editors, and
\".ea\" if there are more than two.

[edtrshort]: The last name if one editor is given; the first
character of up to three editors' last names if more than one
editor is given. A plus character is added, if there are more
than three editors.

[edtrForeIni]: The forename initial of the first editor.

[editorLastForeIni]: The forename initial of the last editor.

Title-related key patterns

[shorttitle]: The first 3 words of the title, ignoring any
function words (see below). For example, An awesome paper on
JabRef becomes AwesomePaperJabref.

[veryshorttitle]: The first word of the title, ignoring any
function words (see below). For example, An awesome paper on
JabRef becomes Awesome.

[camel]: Capitalize all the words of the title. For example, An
awesome paper on JabRef becomes An Awesome Paper On Jabref.

[title]: Capitalize all the significant words of the title. For
example, An awesome paper on JabRef becomes An Awesome Paper on
Jabref.

Other key patterns

[firstpage]: The number of the first page of the
publication (Caution: this will return the lowest number found in
the pages field, since BibTeX allows 7,41,73--97 or 43+.)

[pageprefix]: The non-digit prefix of pages (like \"L\" for L7)
or \"\" if no non-digit prefix exists (like \"\" for 7,41,73--97)
.

[keywordN]: Keyword number N from the \"keywords\" field,
assuming keywords are separated by commas or semicolons.

[lastpage]: The number of the last page of the publication (See
the remark on firstpage)

[shortyear]: The last 2 digits of the publication year

Note: the functions above all have the clean filter automatically
applied to them. If you want more control, you can disable this
by configuring the `pubmed-bibtex-apply-clean' variable.

Filters

A field name (or one of the above pseudo-field names) may optionally
be followed by one or more filters. Filters are applied in the order
they are specified.

abbr: Abbreviates the text produced by the field name or special
field marker. Only the first character and subsequent characters
following white space will be included. For example,
[journal:abbr] would from the journal name \"Journal of Fish
Biology\" produce \"JoFB\", [title:abbr] would from the title
\"An awesome paper on JabRef\" produce \"AAPoJ\", [camel:abbr]
would from the title \"An awesome paper on JabRef\" produce
\"AAPOJ\".

alphanum: clears out everything but unicode alphanumeric
characters (unicode character classes L and N)

ascii: removes all non-ascii characters

capitalize: Changes the first character of each word to
uppercase, all other characters are converted to lowercase. For
example, an example title will be converted to An Example Title

clean: transliterates the citation keys and removes unsafe characters

condense: this replaces spaces in the value passed in. You can specify
what to replace it with by adding it as a parameter, e.g condense=_
will replace spaces with underscores. Parameters should not contain
spaces unless you want the spaces in the value passed in to be
replaced with those spaces in the parameter

fold: tries to replace diacritics with ascii look-alikes. Removes
non-ascii characters it cannot match.

lower: Forces the text inserted by the field marker to be in
lowercase. For example, [auth:lower] expands the last name of the
first author in lowercase.

nopunct: Removes punctuation

nopunctordash: Removes punctuation and word-connecting dashes

postfix: postfixes with its parameter, so postfix=_ will add an
underscore to the end if, and only if, the value it is supposed to
postfix isn't empty

prefix: prefixes with its parameter, so prefix=_ will add an
underscore to the front if, and only if, the value it is supposed to
prefix isn't empty. If you want to use a reserved character (such as :
or \), you'll need to add a backslash (\) in front of it.

replace: replaces text, case insensitive; :replace=.etal,&etal will
replace .EtAl with &etal

select: selects words from the value passed in. The format is
select=start,number (1-based), so select=1,4 would select the first
four words. If number is not given, all words from start to the end of
the list are selected. It is important to note that `select' works
only on values that have the words separated by whitespace, so the
caveat below applies.

sentencecase: Changes the first character of the first word to
uppercase, all remaining words are converted to lowercase.
Example: an Example Title will be converted to An example title

skipwords: filters out common words like ‘of', ‘the', … the list of
words can be seen and changed by going into about:config under the key
extensions.zotero.translators.better-bibtex.skipWords as a
comma-separated, case-insensitive list of words.

substring: (substring=start,n) selects n (default: all) characters
starting at start (default: 1)

titlecase: Changes the first character of all normal words to
uppercase, all function words (see
`pubmed-bibtex-function-words') are converted to lowercase.
Example: example title with An function Word will be converted to
Example Title with an Function Word

upper: Forces the text inserted by the field marker to be in
uppercase. For example, [auth:upper] expands the last name of the
first author in uppercase.

(x): The string between the parentheses will be inserted if the
field marker preceding this filter resolves to an empty value.
The placeholder x may be any string. For instance, the marker
[volume:(unknown)] will return the entry's volume if set, and the
string unknown if the entry's volume field is not set."
  
  :link '(url-link "https://help.jabref.org/en/BibtexKeyPatterns")
  :group 'pubmed-bibtex
  :type 'string)

(defcustom pubmed-bibtex-function-words
  '("a" "an" "the" "above" "about" "across" "against" "along" "among"
    "around" "at" "before" "behind" "below" "beneath" "beside" "between"
    "beyond" "by" "down" "during" "except" "for" "from" "in" "inside"
    "into" "like" "near" "of" "off" "on" "onto" "since" "to" "toward"
    "through" "under" "until" "up" "upon" "with" "within" "without" "and"
    "but" "for" "nor" "or" "so" "yet")
  "List of function words."
  :group 'pubmed-bibtex
  :type '(repeat string))

(defcustom pubmed-bibtex-apply-clean t
  "Automatically apply the clean filter to the citekey functions.
The filter does diacritic folding, space removal, and stripping
of invalid citekey characters."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-default-file (concat (file-name-as-directory default-directory) "pubmed.bib")
  "Default BibTeX file."
  :group 'pubmed-bibtex
  :type 'file)

(defcustom pubmed-bibtex-article-volume t
  "If non-nil, include the optional \"volume\" field name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-article-number nil
  "If non-nil, include the optional \"number\" field name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-article-pages t
  "If non-nil, include the optional \"pages\" field name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-article-month nil
  "If non-nil, include the optional \"month\" field name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-article-note nil
  "If non-nil, include the optional \"note\" field name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-article-issn nil
  "If non-nil, include the non-standard \"issn\" name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-article-pubmed nil
  "If non-nil, include the non-standard \"pubmed\" name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-article-pii nil
  "If non-nil, include the non-standard \"pii\" field name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-article-doi t
  "If non-nil, include the non-standard \"doi\" field name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-article-url nil
  "If non-nil, include the non-standard \"url\" field name in the \"@article\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-book-series nil
  "If non-nil, include the optional \"series\" field name in the \"@book\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-book-address t
  "If non-nil, include the optional \"address\" field name in the \"@book\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-book-edition t
  "If non-nil, include the optional \"edition\" field name in the \"@book\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-book-month nil
  "If non-nil, include the optional \"month\" field name in the \"@book\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-book-note nil
  "If non-nil, include the optional \"note\" field name in the \"@book\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-book-pubmed nil
  "If non-nil, include the non-standard \"pubmed\" name in the \"@book\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-book-url nil
  "If non-nil, include the non-standard \"url\" field name in the \"@book\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-editor t
  "If non-nil, include the optional \"editor\" field name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-series nil
  "If non-nil, include the optional \"series\" field name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-chapter nil
  "If non-nil, include the optional \"chapter\" field name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-pages t
  "If non-nil, include the optional \"pages\" field name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-address t
  "If non-nil, include the optional \"address\" field name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-edition t
  "If non-nil, include the optional \"edition\" field name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-month nil
  "If non-nil, include the optional \"month\" field name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-note nil
  "If non-nil, include the optional \"note\" field name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-pubmed nil
  "If non-nil, include the non-standard \"pubmed\" name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

(defcustom pubmed-bibtex-incollection-url nil
  "If non-nil, include the non-standard \"url\" field name in the \"@incollection\" reference type."
  :group 'pubmed-bibtex
  :type 'boolean)

;;;; Commands

;;;###autoload
(defun pubmed-bibtex-show (&optional entries)
  "In PubMed, show the BibTeX references of the marked entries or current entry. If optional argument ENTRIES is a list of UIDs, show the BibTeX references of the entries."
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
      (let ((bibtex-entry-buffer (get-buffer-create "*BibTeX entry*"))
	    (summaries (pubmed-bibtex--summaries entries)))
	(with-current-buffer bibtex-entry-buffer
	  (erase-buffer)
	  (mapc (lambda (summary) (pubmed-bibtex--insert summary)) summaries)
	  (bibtex-mode)
	  ;; Call `bibtex-set-dialect' to set `bibtex-entry-head', or
	  ;; `bibtex-parse-buffers-stealthily' will throw an error
	  ;; because the buffer is not associated with an existing
	  ;; file
	  (bibtex-set-dialect)
	  (goto-char (point-min)))
	(save-selected-window
	  (display-buffer bibtex-entry-buffer))))
     (mark-list
      (let ((bibtex-entry-buffer (get-buffer-create "*BibTeX entry*"))
	    (summaries (pubmed-bibtex--summaries (nreverse mark-list))))
	(with-current-buffer bibtex-entry-buffer
	  (erase-buffer)
	  (mapc (lambda (summary) (pubmed-bibtex--insert summary)) summaries)
	  (bibtex-mode)
	  (bibtex-set-dialect)
	  (goto-char (point-min)))
	(save-selected-window
	  (display-buffer bibtex-entry-buffer))))
     ((tabulated-list-get-id)
      (let ((bibtex-entry-buffer (get-buffer-create "*BibTeX entry*"))
	    (summaries (pubmed-bibtex--summaries (tabulated-list-get-id))))
	(with-current-buffer bibtex-entry-buffer
	  (erase-buffer)
	  (mapc (lambda (summary) (pubmed-bibtex--insert summary)) summaries)
	  (bibtex-mode)
	  (bibtex-set-dialect)
	  (goto-char (point-min)))
	(save-selected-window
	  (display-buffer bibtex-entry-buffer))))
     (t
      (error "No entry selected")))))

;;;###autoload
(defun pubmed-bibtex-write (&optional file entries)
  "In PubMed, write the BibTeX references of the marked entries or current entry to file FILE.
If optional argument ENTRIES is a list of UIDs, write the BibTeX
references of the entries. If FILE is not empty, the references
are appended to the end of the file."
  (interactive
   (list (read-file-name "Write to BibTeX file: " nil pubmed-bibtex-default-file nil pubmed-bibtex-default-file)))
  (pubmed--guard)
  (if (not (file-writable-p file)) (error "Output file not writable")
    (if entries
	(pubmed-bibtex--write file entries)
      (pubmed-bibtex--write file))))

;;;; Functions

(defun pubmed-bibtex--summaries (uids)
  "Return list of summaries of UIDS."
  (let ((url-request-method "POST")
	(url-request-extra-headers `(("Content-Type" . ,#'"application/x-www-form-urlencoded")))
	(url-request-data (concat "db=pubmed"
				  "&retmode=xml"
				  "&rettype=abstract"
				  "&id=" (if (listp uids)
					     (s-join "," uids)
					   uids)
				  (when (not (string-empty-p pubmed-api-key))
				    (concat "&api_key=" pubmed-api-key)))))
    (with-current-buffer (url-retrieve-synchronously pubmed-efetch-url)
      (let* ((dom (libxml-parse-xml-region (1+ url-http-end-of-headers) (point-max)))
	     (summaries (esxml-node-children (esxml-query "PubmedArticleSet" dom))))
	summaries))))

(defun pubmed-bibtex--insert (summary)
  "Insert the BibTeX reference of SUMMARY in the current buffer."
  (let ((citationkey (if pubmed-bibtex-apply-clean
			 (pubmed-bibtex-key--clean (pubmed-bibtex--keypattern summary))
		       (pubmed-bibtex--keypattern summary))))
    (cond
     ((and (equal (car summary) 'PubmedBookArticle) (pubmed--summary-article-title summary))
      (progn
	(insert "@incollection{")
	(insert citationkey)
	;; BibTeX accepts names in the format "forename surname" or "surname, forename". Here, the latter is used.
	(let ((authorlist (pubmed--summary-book-authors summary))
      	      authors)
      	  (dolist (author authorlist authors)
    	    (let* ((lastname (plist-get author 'lastname))
    		   (initials (plist-get author 'initials))
    		   ;; Add periods and spaces after all initials, and then remove the space after the last initial
    		   (dotted-initials (s-trim-right (mapconcat (lambda (x) (string x ?. ?\s)) initials ""))))
    	      (push (concat lastname ", " dotted-initials) authors)))
	  (insert ",\n"
    		  "author = {"
    		  (s-join " and " (nreverse authors))
	  	  "}"))
	(let ((editorlist (pubmed--summary-book-editors summary))
      	      editors)
      	  (dolist (editor editorlist editors)
    	    (let* ((lastname (plist-get editor 'lastname))
    		   (initials (plist-get editor 'initials))
    		   ;; Add periods and spaces after all initials, and then remove the space after the last initial
    		   (dotted-initials (s-trim-right (mapconcat (lambda (x) (string x ?. ?\s)) initials ""))))
    	      (push (concat lastname ", " dotted-initials) editors)))
	  (when (and pubmed-bibtex-incollection-editor editors)
	    (insert ",\n"
	   	    "editor = {"
	   	    (s-join " and " (nreverse editors))
	   	    "}")))
	(insert ",\n"
		"title = {{"
		(pubmed--summary-article-title summary)
		"}}")
	(insert ",\n"
		"booktitle = {{"
		(pubmed--summary-book-title summary)
		"}}")
	(insert ",\n"
		"publisher = {"
		(plist-get (pubmed--summary-publisher summary) 'name)
		"}")
	(when (and pubmed-bibtex-incollection-series
		   (pubmed--summary-book-collectiontitle summary))
	  (insert ",\n"
	       	  "series = {"
		  (pubmed--summary-book-collectiontitle summary)
	       	  "}"))
	(when (and pubmed-bibtex-incollection-chapter
		   (pubmed--summary-chapter summary))
	  (insert ",\n"
	       	  "chapter = {"
		  (pubmed--summary-chapter summary)
	       	  "}"))
	(when (and pubmed-bibtex-book-series
		   (pubmed--summary-book-collectiontitle summary))
	  (insert ",\n"
	       	  "series = {"
		  (pubmed--summary-book-collectiontitle summary)
	       	  "}"))
	(when (and pubmed-bibtex-incollection-pages
		   (pubmed--summary-pagination summary))
          (insert ",\n"
      		  "pages = {"
		  (pubmed--summary-pagination summary)
      		  "}"))
	(when (and pubmed-bibtex-book-address
		   (plist-get (pubmed--summary-publisher summary) 'location))
	  (insert ",\n"
		  "address = {"
		  (plist-get (pubmed--summary-publisher summary) 'location)
		  "}"))
	(when (and pubmed-bibtex-book-edition
		   (pubmed--summary-book-edition summary))
          (insert ",\n"
         	  "edition = {"
		  (pubmed--summary-book-edition summary)
         	  "}"))
	(insert ",\n"
    		"year = {"
    		(pubmed-bibtex-key--year summary)
    		"}")
	(when (and pubmed-bibtex-book-month
		   (pubmed-bibtex-key--month summary))
    	  (insert ",\n"
    	   	  "month = {"
		  (pubmed-bibtex-key--month summary)
    	   	  "}"))
	(when (and pubmed-bibtex-book-pubmed
		   (plist-get (pubmed--summary-articleid summary) 'pubmed))
      	  (insert ",\n"
      	   	  "pubmed"
      	   	  " = {"
		  (plist-get (pubmed--summary-articleid summary) 'pubmed)
      	   	  "}"))
	(when pubmed-bibtex-book-url
      	  (insert ",\n"
      	   	  "url = {}"))
	(when pubmed-bibtex-book-note
      	  (insert ",\n"
      	   	  "note = {}"))
	(insert "\n"
		"}\n\n")))
     ((equal (car summary) 'PubmedBookArticle)
      (progn
	(insert "@book{")
	(insert citationkey)
	;; BibTeX accepts names in the format "forename/initials surname" or "surname, forename/initials".
	;; Here, the latter is used.
	(let ((authorlist (pubmed--summary-authors summary))
      	      authors)
      	  (dolist (author authorlist authors)
    	    (let* ((lastname (plist-get author 'lastname))
    		   (initials (plist-get author 'initials))
    		   ;; Add periods and spaces after all initials, and then remove the space after the last initial
    		   (dotted-initials (s-trim-right (mapconcat (lambda (x) (string x ?. ?\s)) initials ""))))
    	      (push (concat lastname ", " dotted-initials) authors)))
	  (insert ",\n"
    		  "author = {"
    		  (s-join " and " (nreverse authors))
	  	  "}"))
	(insert ",\n"
		"title = {{"
		(pubmed--summary-book-title summary)
		"}}")
	(insert ",\n"
		"publisher = {"
		(plist-get (pubmed--summary-publisher summary) 'name)
		"}")
	(when (and pubmed-bibtex-book-address
		   (plist-get (pubmed--summary-publisher summary) 'location))
	  (insert ",\n"
		  "address = {"
		  (plist-get (pubmed--summary-publisher summary) 'location)
		  "}"))
	(when (and pubmed-bibtex-book-edition
		   (pubmed--summary-book-edition summary))
          (insert ",\n"
         	  "edition = {"
		  (pubmed--summary-book-edition summary)
         	  "}"))
	(insert ",\n"
    		"year = {"
    		(pubmed-bibtex-key--year summary)
    		"}")
	(when (and pubmed-bibtex-book-month
		   (pubmed-bibtex-key--month summary))
    	  (insert ",\n"
    	   	  "month = {"
		  (pubmed-bibtex-key--month summary)
    	   	  "}"))
	(when (and pubmed-bibtex-book-pubmed
		   (plist-get (pubmed--summary-articleid summary) 'pubmed))
      	  (insert ",\n"
      	   	  "pubmed"
      	   	  " = {"
		  (plist-get (pubmed--summary-articleid summary) 'pubmed)
      	   	  "}"))
	(when pubmed-bibtex-book-url
      	  (insert ",\n"
      	   	  "url = {}"))
	(when pubmed-bibtex-book-note
      	  (insert ",\n"
      	   	  "note = {}"))
	(insert "\n"
		"}\n\n")))
     ((equal (car summary) 'PubmedArticle)
      (progn
	(insert "@article{")
	(insert citationkey)
	;; BibTeX accepts names in the format "forename surname" or "surname, forename". Here, the latter is used.
	(let ((authorlist (pubmed--summary-authors summary))
      	      authors)
      	  (dolist (author authorlist authors)
    	    (let* ((lastname (plist-get author 'lastname))
    		   (initials (plist-get author 'initials))
    		   ;; Add periods and spaces after all initials, and then remove the space after the last initial
    		   (dotted-initials (s-trim-right (mapconcat (lambda (x) (string x ?. ?\s)) initials ""))))
    	      (push (concat lastname ", " dotted-initials) authors)))
	  (insert ",\n"
    		  "author = {"
    		  (s-join " and " (nreverse authors))
		  ;; (format "%S" authorlist)
	  	  "}"))
	(insert ",\n"
    		"title = {{"
    		(pubmed--summary-article-title summary)
    		"}}")
	(insert ",\n"
    		"journal = {{"
    		(pubmed--summary-journal-isoabbreviation summary)
    		"}}")
	(when (and pubmed-bibtex-article-issn
		   (plist-get (pubmed--summary-issn summary) 'issn))
	  (insert ",\n"
    		  "issn = {"
		  (plist-get (pubmed--summary-issn summary) 'issn)
    		  "}"))
	(insert ",\n"
    		"year = {"
		(pubmed-bibtex-key--year summary)
    		"}")
	(when (and pubmed-bibtex-article-month
		   (pubmed-bibtex-key--month summary))
    	  (insert ",\n"
    	   	  "month = {"
    		  (pubmed-bibtex-key--month summary)
    	   	  "}"))
	(when (and pubmed-bibtex-article-volume
		   (plist-get (pubmed--summary-journal-issue summary) 'volume))
          (insert ",\n"
      		  "volume = {"
		  (plist-get (pubmed--summary-journal-issue summary) 'volume)
      		  "}"))
	(when (and pubmed-bibtex-article-number
      		   (plist-get (pubmed--summary-journal-issue summary) 'issue))
          (insert ",\n"
      		  "number = {"
      		  (plist-get (pubmed--summary-journal-issue summary) 'issue)
      		  "}"))
	(when (and pubmed-bibtex-article-pages
		   (pubmed--summary-pagination summary))
          (insert ",\n"
      		  "pages = {"
		  (pubmed--summary-pagination summary)
      		  "}"))
	(when (and pubmed-bibtex-article-pubmed
		   (plist-get (pubmed--summary-articleid summary) 'pubmed))
      	  (insert ",\n"
      	   	  "pubmed"
      	   	  " = {"
		  (plist-get (pubmed--summary-articleid summary) 'pubmed)
      	   	  "}"))
	(when (and pubmed-bibtex-article-pii
		   (plist-get (pubmed--summary-articleid summary) 'pii))
      	  (insert ",\n"
		  "pii"
      	   	  " = {"
		  (plist-get (pubmed--summary-articleid summary) 'pii)
      	   	  "}"))
	(when (and pubmed-bibtex-article-doi
		   (plist-get (pubmed--summary-articleid summary) 'doi))
      	  (insert ",\n"
		  "doi"
      	   	  " = {"
		  (plist-get (pubmed--summary-articleid summary) 'doi)
      	   	  "}"))
	(when pubmed-bibtex-article-url
          (insert ",\n"
      		  "url = {}"))
	(when pubmed-bibtex-article-note
          (insert ",\n"
      		  "note = {}"))
	(insert "\n"
      		"}\n\n"))))))

;; FIXME: add support for multiple filter functions.
(defun pubmed-bibtex--keypattern (summary)
  "Return the BibTeX citation key according to `pubmed-bibtex-keypattern' of SUMMARY."
  (let ((regexp "\\[\\([[:alpha:].]+\\)\\([[:digit:]]*\\)[_]?\\([[:digit:]]*\\)[:]?\\([[:alpha:]()]*\\)[=]?\\([[:alnum:]]*\\)[,]?\\([[:alnum:]]*\\)\\]")
	field-key
	filter-key
	citation-key)
    (with-temp-buffer
      (insert pubmed-bibtex-keypattern)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(let* ((field (match-string 1))
	       (arg1 (match-string 2))
	       (arg2 (match-string 3))
	       (filter (match-string 4))
	       (arg3 (match-string 5))
	       (arg4 (match-string 6))
	       (fieldfunction (cdr (assoc field pubmed-bibtex-key-functions)))
	       (filterfunction (cdr (assoc filter pubmed-bibtex-key-functions))))
	  ;; Capture text between parentheses and use the placeholder filter function
	  (when (string-match "\(\\([^)]+\\)\)" filter)
	    (setq arg3 (match-string 1 filter)
		  filterfunction #'pubmed-bibtex-key--placeholder))
	  (cond
	   ((and fieldfunction (not (string-empty-p arg1)) (not (string-empty-p arg2)))
	    (setq field-key (funcall fieldfunction summary (string-to-number arg1) (string-to-number arg2))))
	   ((and fieldfunction (not (string-empty-p arg1)))
	    (setq field-key (funcall fieldfunction summary (string-to-number arg1))))
	   (fieldfunction
	    (setq field-key (funcall fieldfunction summary)))
	   (t
	    (error "Not a valid keypattern")))
	  (cond
	   ((eq filterfunction 'pubmed-bibtex-key--placeholder)
	    (setq filter-key (funcall filterfunction field-key arg3)))
	   ((and filterfunction (not (string-empty-p arg3)) (not (string-empty-p arg4)))
	    (setq filter-key (funcall filterfunction field-key arg3 arg4)))
	   ((and filterfunction (not (string-empty-p arg3)))
	    (setq filter-key (funcall filterfunction field-key arg3)))
	   (filterfunction
	    (setq filter-key (funcall filterfunction field-key)))
	   (t
	    (setq filter-key field-key)))
	  (push filter-key citation-key)))
      (s-join "" (nreverse citation-key)))))

(defun pubmed-bibtex--unique-citation-keys ()
  "In the current buffer, ensure unique citation keys by appending a character from a-z."
  ;; Use remove-duplicates instead of delete-dups, to avoid the destructive properties of the latter
  (with-current-buffer (current-buffer)
    (let* ((string (buffer-substring-no-properties (point-min) (point-max)))
	   (all-keys (pubmed-bibtex--list-citation-keys string))
	   (unique-keys (cl-remove-duplicates all-keys :test #'string-equal)))
      (unless (eq all-keys unique-keys)
	(dolist (key unique-keys)
	  (let ((regexp (concat "\\(@article\\|@book\\|@incollection\\){\\(" key "\\),$"))
		(counter 0)
		(character 97))
            (save-excursion
	      (goto-char (point-min))
	      ;; Iterate over duplicate citation keys
	      (while (re-search-forward regexp nil t)
		(when (> counter 0)
		  ;; Try appending a character to the citation key until an unique key is found
		  (while (member (concat key (string character)) unique-keys)
		    (setq character (1+ character)))
		  (goto-char (match-end 2))
		  (insert character)
		  (push (concat key (string character)) unique-keys))
		(setq counter (1+ counter))))))))))

(defun pubmed-bibtex--list-citation-keys (string)
  "Return a list of all BibTeX citation keys in STRING."
  (let ((regexp "\\(@article\\|@book\\|@incollection\\){\\([^,]+\\),$")
	(pos 0)
        matches)
    (while (string-match regexp string pos)
      (push (match-string 2 string) matches)
      (setq pos (match-end 0)))
    (setq matches (reverse matches))
    matches))

(defun pubmed-bibtex--write (file &optional entries)
  "In PubMed, write the BibTeX references of the marked entries or current entry to file FILE.
If optional argument ENTRIES is a list of UIDs, write the BibTeX references of the entries."
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
      (let ((summaries (pubmed-bibtex--summaries entries)))
     	(with-current-buffer (find-file-noselect file)
     	  (goto-char (point-max))
     	  (mapc (lambda (summary) (pubmed-bibtex--insert summary)) summaries)
	  (pubmed-bibtex-unique--citation-keys)
	  (save-buffer))))
     (mark-list
      (let ((summaries (pubmed-bibtex--summaries (nreverse mark-list))))
	(with-current-buffer (find-file-noselect file)
	  (goto-char (point-max))
	  (mapc (lambda (summary) (pubmed-bibtex--insert summary)) summaries)
	  (pubmed-bibtex--unique-citation-keys)
	  (save-buffer))))
     ((tabulated-list-get-id)
      (let ((summaries (pubmed-bibtex--summaries (tabulated-list-get-id))))
	(with-current-buffer (find-file-noselect file)
	  (goto-char (point-max))
	  (mapc (lambda (summary) (pubmed-bibtex--insert summary)) summaries)
	  (pubmed-bibtex--unique-citation-keys)
	  (save-buffer))))
     (t
      (error "No entry selected")))))

;;;;; Author-related key patterns

(defun pubmed-bibtex-key--auth (summary &optional n m)
  "Return the [auth], [authN], or [authN_M] key pattern.
The last name of the first author. Optional argument N means the
first N characters of the first author's last name. Optional
argument M means the first N characters of the Mth author's last
name."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (cond
     ((and n m)
      (s-left n (plist-get (nth (1- m) authors) 'lastname)))
     (n
      (s-left n (plist-get (first authors) 'lastname)))
     (t
      (plist-get (first authors) 'lastname)))))

(defun pubmed-bibtex-key--authors (summary &optional n _m)
  "Return the [authors] or [authorsN] key pattern.
The last name of the authors. Optional argument N
means the last name of up to N authors. If there are more authors,
\"EtAl\" is appended."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (cond
     (n
      (let ((counter 0)
	    authorlist)
	(while (and (> n counter) (< counter (length authors)))
	  (push (plist-get (nth counter authors) 'lastname) authorlist)
	  (setq counter (1+ counter)))
	(when (< n (length authors))
	  (push "EtAl" authorlist))
	(s-join " " (nreverse authorlist))))
     (t
      (s-join " " (mapcar (lambda (author) (plist-get author 'lastname)) authors))))))

(defun pubmed-bibtex-key--authorlast (summary &optional _n _m)
  "Return the [authorLast] key pattern.
The last name of the last author."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (plist-get (car (last authors)) 'lastname)))

(defun pubmed-bibtex-key--authorsalpha (summary &optional _n _m)
  "Return the [authorsAlpha] key pattern.
Corresponds to the BibTeX style \"alpha\". One author: First
three letters of the last name. Two to four authors: First
letters of last names concatenated. More than four authors: First
letters of last names of first three authors concatenated. \"+\" at
the end."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary)))
	(counter 0)
	authorlist)
    (cond
     ((= (length authors) 1)
      (s-left 3 (plist-get (first authors) 'lastname)))
     ((and (>= (length authors) 2) (<= (length authors) 4))
      (while (< counter (length authors))
	(push (plist-get (nth counter authors) 'lastname) authorlist)
	(setq counter (1+ counter)))
      (mapconcat (lambda (x) (s-left 3 x)) (nreverse authorlist) ""))
     ((> (length authors) 4)
      (while (<= counter 2)
	(push (plist-get (nth counter authors) 'lastname) authorlist)
	(setq counter (1+ counter)))
      (if (> (length authors) 3)
	  (concat (mapconcat (lambda (x) (s-left 3 x)) (nreverse authorlist) "") "+")
	(mapconcat (lambda (x) (s-left 3 x)) (nreverse authorlist) ""))))))

(defun pubmed-bibtex-key--authini (summary n &optional _m)
  "Return the [authIniN] key pattern.
The beginning of each author's last name, using no more than N
characters."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (s-join " " (mapcar (lambda (author) (s-left n (plist-get author 'lastname))) authors))))

(defun pubmed-bibtex-key--authorini (summary &optional _n _m)
  "Return the [authorIni] key pattern.
The first 5 characters of the first author's last name, and the
last name initials of the remaining authors."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary)))
	(counter 0)
	authorlist)
    (while (< counter (length authors))
      (cond
       ((eq counter 0)
	(push (s-left 5 (plist-get (nth counter authors) 'lastname)) authorlist))
       (t
	(push (s-left 1 (plist-get (nth counter authors) 'lastname)) authorlist)))
      (setq counter (1+ counter)))
    (s-join " " (nreverse authorlist))))

(defun pubmed-bibtex-key--auth.auth.ea (summary &optional _n _m)
  "Return the [auth.auth.ea] key pattern.
The last name of the first two authors, and \".ea\" if there are
more than two."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary)))
	(counter 0)
	authorlist)
    (while (< counter 2)
      (push (plist-get (nth counter authors) 'lastname) authorlist)
      (setq counter (1+ counter)))
    (when (> (length authors) 2)
      (push "ea" authorlist))
    (s-join "." (nreverse authorlist))))

(defun pubmed-bibtex-key--auth.etal (summary &optional _n _m)
  "Return the [auth.etal] key pattern.
The last name of the first author, and the last name of
the second author if there are two authors or \".etal\" if there are
more than two."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (cond
     ((<= (length authors) 2)
      (concat (plist-get (first authors) 'lastname) "." (plist-get (nth 1 authors) 'lastname)))
     ((> (length authors) 2)
      (concat (plist-get (first authors) 'lastname) ".etal")))))

(defun pubmed-bibtex-key--authetal (summary &optional _n _m)
  "Return the [authEtAl] key pattern.
The last name of the first author, and the last name of
the second author if there are two authors or \"EtAl\" if there are
more than two. This is similar to auth.etal. The difference is that
the authors are not separated by \".\" and in case of more than 2
authors \"EtAl\" instead of \".etal\" is appended."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (cond
     ((<= (length authors) 2)
      (concat (plist-get (car authors) 'lastname) (plist-get (cadr authors) 'lastname)))
     ((> (length authors) 2)
      (concat (plist-get (first authors) 'lastname) "EtAl")))))

(defun pubmed-bibtex-key--authshort (summary &optional _n _m)
  "Return the [authshort] key pattern.
The last name if one author is given; the first character of up
to three authors' last names if more than one author is given. A
plus character is added, if there are more than three authors."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary)))
	(counter 0)
	authorlist)
    (cond
     ((= (length authors) 1)
      (plist-get (first authors) 'lastname))
     ((> (length authors) 1)
      (while (< counter 3)
	(push (s-left 1 (plist-get (nth counter authors) 'lastname)) authorlist)
	(setq counter (1+ counter)))
      (when (> (length authors) 3)
	(push "+" authorlist))
      (s-join " " (nreverse authorlist))))))

(defun pubmed-bibtex-key--authforeini (summary &optional _n _m)
  "Return the [authForeIni] key pattern.
The forename initial of the first author."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (plist-get (first authors) 'initials)))

(defun pubmed-bibtex-key--authorlastforeini (summary &optional _n _m)
  "Return the [authorLastForeIni] key pattern.
The forename initial of the last author."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)
		     (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (plist-get (car (last authors)) 'initials)))

(defun pubmed-bibtex-key--pureauth (summary &optional n m)
  "Return the [pureauth], [pureauthN], or [pureauthN_M] key pattern.
The last name of the first author. Optional argument N means the
first N characters of the first author's last name. Optional
argument M means the first N characters of the Mth author's last
name."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary))))
    (cond
     ((and n m)
      (s-left n (plist-get (nth (1- m) authors) 'lastname)))
     (n
      (s-left n (plist-get (first authors) 'lastname)))
     (t
      (plist-get (first authors) 'lastname)))))

(defun pubmed-bibtex-key--pureauthors (summary &optional n _m)
  "Return the [pureauthors] or [pureauthorsN] key pattern.
The last name of the authors. Optional argument N
means the last name of up to N authors. If there are more authors,
\"EtAl\" is appended."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary))))
    (cond
     (n
      (let ((counter 0)
	    authorlist)
	(while (and (> n counter) (< counter (length authors)))
	  (push (plist-get (nth counter authors) 'lastname) authorlist)
	  (setq counter (1+ counter)))
	(when (< n (length authors))
	  (push "EtAl" authorlist))
	(s-join " " (nreverse authorlist))))
     (t
      (s-join " " (mapcar (lambda (author) (plist-get author 'lastname)) authors))))))

(defun pubmed-bibtex-key--pureauthorlast (summary &optional _n _m)
  "Return the [pureauthorLast] key pattern.
The last name of the last author."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary))))
    (plist-get (car (last authors)) 'lastname)))

(defun pubmed-bibtex-key--pureauthorsalpha (summary &optional _n _m)
  "Return the [pureauthorsAlpha] key pattern.
Corresponds to the BibTeX style \"alpha\". One author: First
three letters of the last name. Two to four authors: First
letters of last names concatenated. More than four authors: First
letters of last names of first three authors concatenated. \"+\" at
the end."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)))
	(counter 0)
	authorlist)
    (cond
     ((= (length authors) 1)
      (s-left 3 (plist-get (first authors) 'lastname)))
     ((and (>= (length authors) 2) (<= (length authors) 4))
      (while (< counter (length authors))
	(push (plist-get (nth counter authors) 'lastname) authorlist)
	(setq counter (1+ counter)))
      (mapconcat (lambda (x) (s-left 3 x)) (nreverse authorlist) ""))
     ((> (length authors) 4)
      (while (<= counter 2)
	(push (plist-get (nth counter authors) 'lastname) authorlist)
	(setq counter (1+ counter)))
      (if (> (length authors) 3)
	  (concat (mapconcat (lambda (x) (s-left 3 x)) (nreverse authorlist) "") "+")
	(mapconcat (lambda (x) (s-left 3 x)) (nreverse authorlist) ""))))))

(defun pubmed-bibtex-key--pureauthini (summary n &optional _m)
  "Return the [pureauthIniN] key pattern.
The beginning of each author's last name, using no more than N
characters."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary))))
    (s-join " " (mapcar (lambda (author) (s-left n (plist-get author 'lastname))) authors))))

(defun pubmed-bibtex-key--pureauthorini (summary &optional _n _m)
  "Return the [pureauthorIni] key pattern.
The first 5 characters of the first author's last name, and the
last name initials of the remaining authors."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)))
	(counter 0)
	authorlist)
    (while (< counter (length authors))
      (cond
       ((eq counter 0)
	(push (s-left 5 (plist-get (nth counter authors) 'lastname)) authorlist))
       (t
	(push (s-left 1 (plist-get (nth counter authors) 'lastname)) authorlist)))
      (setq counter (1+ counter)))
    (s-join " " (nreverse authorlist))))

(defun pubmed-bibtex-key--pureauth.auth.ea (summary &optional _n _m)
  "Return the [pureauth.auth.ea] key pattern.
The last name of the first two authors, and \".ea\" if there are
more than two."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)))
	(counter 0)
	authorlist)
    (while (< counter 2)
      (push (plist-get (nth counter authors) 'lastname) authorlist)
      (setq counter (1+ counter)))
    (when (> (length authors) 2)
      (push "ea" authorlist))
    (s-join "." (nreverse authorlist))))

(defun pubmed-bibtex-key--pureauth.etal (summary &optional _n _m)
  "Return the [pureauth.etal] key pattern.
The last name of the first author, and the last name of
the second author if there are two authors or \".etal\" if there are
more than two."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary))))
    (cond
     ((<= (length authors) 2)
      (concat (plist-get (first authors) 'lastname) "." (plist-get (nth 1 authors) 'lastname)))
     ((> (length authors) 2)
      (concat (plist-get (first authors) 'lastname) ".etal")))))

(defun pubmed-bibtex-key--pureauthetal (summary &optional _n _m)
  "Return the [pureauthEtAl] key pattern.
The last name of the first author, and the last name of
the second author if there are two authors or \"EtAl\" if there are
more than two. This is similar to auth.etal. The difference is that
the authors are not separated by \".\" and in case of more than 2
authors \"EtAl\" instead of \".etal\" is appended."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary))))
    (cond
     ((<= (length authors) 2)
      (concat (plist-get (car authors) 'lastname) (plist-get (cadr authors) 'lastname)))
     ((> (length authors) 2)
      (concat (plist-get (first authors) 'lastname) "EtAl")))))

(defun pubmed-bibtex-key--pureauthshort (summary &optional _n _m)
  "Return the [pureauthshort] key pattern.
The last name if one author is given; the first character of up
to three authors' last names if more than one author is given. A
plus character is added, if there are more than three authors."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary)))
	(counter 0)
	authorlist)
    (cond
     ((= (length authors) 1)
      (plist-get (first authors) 'lastname))
     ((> (length authors) 1)
      (while (< counter 3)
	(push (s-left 1 (plist-get (nth counter authors) 'lastname)) authorlist)
	(setq counter (1+ counter)))
      (when (> (length authors) 3)
	(push "+" authorlist))
      (s-join " " (nreverse authorlist))))))

(defun pubmed-bibtex-key--pureauthforeini (summary &optional _n _m)
  "Return the [pureauthForeIni] key pattern.
The forename initial of the first author."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary))))
    (plist-get (first authors) 'initials)))

(defun pubmed-bibtex-key--pureauthorlastforeini (summary &optional _n _m)
  "Return the [pureauthorLastForeIni] key pattern.
The forename initial of the last author."
  (let ((authors (or (pubmed--summary-authors summary)
		     (pubmed--summary-book-authors summary))))
    (plist-get (car (last authors)) 'initials)))

;;;;; Editor-related key patterns

(defun pubmed-bibtex-key--edtr (summary &optional n m)
  "Return the [edtr], [edtrN], or [edtrN_M] key pattern.
The last name of the first editor. Optional argument N means the
first N characters of the first editor's last name. Optional
argument M means the first N characters of the Mth editor's last
name."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (cond
     ((and n m)
      (s-left n (plist-get (nth (1- m) editors) 'lastname)))
     (n
      (s-left n (plist-get (first editors) 'lastname)))
     (t
      (plist-get (first editors) 'lastname)))))

(defun pubmed-bibtex-key--editors (summary &optional n _m)
  "Return the [editors] or [editorsN] key pattern.
The last name of the editors. Optional argument N
means the last name of up to N editors. If there are more editors,
\"EtAl\" is appended."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (cond
     (n
      (let ((counter 0)
	    editorlist)
	(while (and (> n counter) (< counter (length editors)))
	  (push (plist-get (nth counter editors) 'lastname) editorlist)
	  (setq counter (1+ counter)))
	(when (< n (length editors))
	  (push "EtAl" editorlist))
	(s-join " " (nreverse editorlist))))
     (t
      (s-join " " (mapcar (lambda (editor) (plist-get editor 'lastname)) editors))))))

(defun pubmed-bibtex-key--editorlast (summary &optional _n _m)
  "Return the [editorLast] key pattern.
The last name of the last editor."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (plist-get (car (last editors)) 'lastname)))

(defun pubmed-bibtex-key--editorsalpha (summary &optional _n _m)
  "Return the [editorsAlpha] key pattern.
Corresponds to the BibTeX style \"alpha\". One editor: First
three letters of the last name. Two to four editors: First
letters of last names concatenated. More than four editors: First
letters of last names of first three editors concatenated. \"+\" at
the end."
  (let* ((editors (or (pubmed--summary-editors summary)
		      (pubmed--summary-book-editors summary)))
	 (counter 0)
	 editorlist)
    (cond
     ((= (length editors) 1)
      (s-left 3 (plist-get (first editors) 'lastname)))
     ((and (>= (length editors) 2) (<= (length editors) 4))
      (while (< counter (length editors))
	(push (plist-get (nth counter editors) 'lastname) editorlist)
	(setq counter (1+ counter)))
      (mapconcat (lambda (x) (s-left 3 x)) (nreverse editorlist) ""))
     ((> (length editors) 4)
      (while (<= counter 2)
	(push (plist-get (nth counter editors) 'lastname) editorlist)
	(setq counter (1+ counter)))
      (if (> (length editors) 3)
	  (concat (mapconcat (lambda (x) (s-left 3 x)) (nreverse editorlist) "") "+")
	(mapconcat (lambda (x) (s-left 3 x)) (nreverse editorlist) ""))))))

(defun pubmed-bibtex-key--edtrini (summary n &optional _m)
  "Return the [edtrIniN] key pattern.
The beginning of each editor's last name, using no more than N
characters."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (s-join " " (mapcar (lambda (editor) (s-left n (plist-get editor 'lastname))) editors))))

(defun pubmed-bibtex-key--editorini (summary &optional _n _m)
  "Return the [editorIni] key pattern.
The first 5 characters of the first editor's last name, and the
last name initials of the remaining editors."
  (let* ((editors (or (pubmed--summary-editors summary)
		      (pubmed--summary-book-editors summary)))
	 (counter 0)
	 editorlist)
    (while (< counter (length editors))
      (cond
       ((eq counter 0)
	(push (s-left 5 (plist-get (nth counter editors) 'lastname)) editorlist))
       (t
	(push (s-left 1 (plist-get (nth counter editors) 'lastname)) editorlist)))
      (setq counter (1+ counter)))
    (s-join " " (nreverse editorlist))))

(defun pubmed-bibtex-key--edtr.edtr.ea (summary &optional _n _m)
  "Return the [edtr.edtr.ea] key pattern.
The last name of the first two editors, and \".ea\" if there are
more than two."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary)))
	(counter 0)
	editorlist)
    (while (< counter 2)
      (push (plist-get (nth counter editors) 'lastname) editorlist)
      (setq counter (1+ counter)))
    (when (> (length editors) 2)
      (push "ea" editorlist))
    (s-join "." (nreverse editorlist))))

(defun pubmed-bibtex-key--edtr.etal (summary &optional _n _m)
  "Return the [edtr.etal] key pattern.
The last name of the first editor, and the last name of
the second editor if there are two editors or \".etal\" if there are
more than two."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (cond
     ((<= (length editors) 2)
      (concat (plist-get (first editors) 'lastname) "." (plist-get (nth 1 editors) 'lastname)))
     ((> (length editors) 2)
      (concat (plist-get (first editors) 'lastname) ".etal")))))

(defun pubmed-bibtex-key--edtretal (summary &optional _n _m)
  "Return the [edtrEtAl] key pattern.
The last name of the first editor, and the last name of
the second editor if there are two editors or \"EtAl\" if there are
more than two. This is similar to edtr.etal. The difference is that
the editors are not separated by \".\" and in case of more than 2
editors \"EtAl\" instead of \".etal\" is appended."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (cond
     ((<= (length editors) 2)
      (concat (plist-get (car editors) 'lastname) (plist-get (cadr editors) 'lastname)))
     ((> (length editors) 2)
      (concat (plist-get (first editors) 'lastname) "EtAl")))))

(defun pubmed-bibtex-key--edtrshort (summary &optional _n _m)
  "Return the [edtrshort] key pattern.
The last name if one editor is given; the first character of up
to three editors' last names if more than one editor is given. A
plus character is added, if there are more than three editors."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary)))
	(counter 0)
	editorlist)
    (cond
     ((= (length editors) 1)
      (plist-get (first editors) 'lastname))
     ((> (length editors) 1)
      (while (< counter 3)
	(push (s-left 1 (plist-get (nth counter editors) 'lastname)) editorlist)
	(setq counter (1+ counter)))
      (when (> (length editors) 3)
	(push "+" editorlist))
      (s-join " " (nreverse editorlist))))))

(defun pubmed-bibtex-key--edtrforeini (summary &optional _n _m)
  "Return the [edtrForeIni] key pattern.
The forename initial of the first editor."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (plist-get (first editors) 'initials)))

(defun pubmed-bibtex-key--editorlastforeini (summary &optional _n _m)
  "Return the [editorLastForeIni] key pattern.
The forename initial of the last editor."
  (let ((editors (or (pubmed--summary-editors summary)
		     (pubmed--summary-book-editors summary))))
    (plist-get (car (last editors)) 'initials)))

;;;;; Title-related key patterns

(defun pubmed-bibtex-key--shorttitle (summary &optional n m)
  "Return the [shorttitle] key pattern.
The first 3 words of the title, ignoring any function words (see
`pubmed-bibtex-function-words'. For example, An awesome paper on
JabRef becomes AwesomePaperJabref. The first N (default: 3) words
of the title, first M (default: 0) capitalized."
  (let* ((title (or (substring-no-properties (pubmed--summary-article-title summary))
		    (substring-no-properties (pubmed--summary-book-title summary))))
	 (words (s-split-words title))
	 (counter 0)
	 shorttitle)
    (dolist (word words)
      (when (and (< counter (or n 3)) (not (member (downcase word) pubmed-bibtex-function-words)))
	(if (> counter (or m 0))
	    (push word shorttitle)
	  (push (capitalize word) shorttitle))
	(setq counter (1+ counter))))
    (s-join " " (nreverse shorttitle))))

(defun pubmed-bibtex-key--veryshorttitle (summary &optional n m)
  "Return the [veryshorttitle] key pattern.
The first word of the title, ignoring any function words (see
`pubmed-bibtex-function-words'). For example, An awesome paper on
JabRef becomes Awesome. The first N (default: 1) words of the
title, first M (default: 0) capitalized ."
  (let* ((title (or (substring-no-properties (pubmed--summary-article-title summary))
		    (substring-no-properties (pubmed--summary-book-title summary))))
	 (words (s-split-words title))
	 (counter 0)
	 shorttitle)
    (dolist (word words shorttitle)
      (when (and (< counter (or n 1)) (not (member (downcase word) pubmed-bibtex-function-words)))
	(if (> counter (or m 0))
	    (push word shorttitle)
	  (push (capitalize word) shorttitle))
	(setq counter (1+ counter))))
    (s-join " " (nreverse shorttitle))))

(defun pubmed-bibtex-key--camel (summary &optional _n _m)
  "Return the [camel] key pattern.
Capitalize all the words of the title. For example, An awesome
paper on JabRef becomes An Awesome Paper On Jabref."
  (let* ((title (or (substring-no-properties (pubmed--summary-article-title summary))
		    (substring-no-properties (pubmed--summary-book-title summary))))
	 (words (s-split-words title))
	 shorttitle)
    (dolist (word words)
      (push (capitalize word) shorttitle))
    (s-join " " (nreverse shorttitle))))

(defun pubmed-bibtex-key--title (summary &optional _n _m)
  "Return the [title] key pattern.
Capitalize all the significant words of the title. For example,
An awesome paper on JabRef becomes An Awesome Paper on Jabref."
  (let* ((title (or (substring-no-properties (pubmed--summary-article-title summary))
		    (substring-no-properties (pubmed--summary-book-title summary))))
	 (words (s-split-words title))
	 (counter 0)
	 shorttitle)
    (dolist (word words)
      (if (or (eq counter 0) (not (member (downcase word) pubmed-bibtex-function-words)))
	  (push (capitalize word) shorttitle)
	(push (downcase word) shorttitle))
      (setq counter (1+ counter)))
    (s-join " " (nreverse shorttitle))))

;;;;; Other key patterns

(defun pubmed-bibtex-key--firstpage (summary &optional _n _m)
  "Return the [firstpage] key pattern.
The number of the first page of the publication. Caution: this
will return the lowest number found in the pages field, since
BibTeX allows 7,41,73--97 or 43+."
  (let ((pagination (pubmed--summary-pagination summary)))
    (first (s-split "[-,]" pagination))))

(defun pubmed-bibtex-key--pageprefix (summary &optional _n _m)
  "Return the [pageprefix] key pattern.
The non-digit prefix of pages (like \"L\" for L7) or \"\" if no
non-digit prefix exists (like \"\" for 7,41,73--97) ."
  (let ((pagination (pubmed--summary-pagination summary)))
    (when (string-match "\\(^[:alpha:]\\)[[:digit:]]+" pagination)
      (match-string 0 pagination))))

(defun pubmed-bibtex-key--keyword (summary n)
  "Return the [keywordN] key pattern.
Keyword number N from the \"keywords\" field."
  (let ((keywords (pubmed--summary-keywords summary)))
    (nth (1- n) keywords)))

(defun pubmed-bibtex-key--lastpage (summary &optional _n _m)
  "Return the [lastpage] key pattern.
The number of the last page of the publication. Caution: this
will return the highest number found in the pages field, since
BibTeX allows 7,41,73--97 or 43+."
  (let ((pagination (pubmed--summary-pagination summary)))
    (car (last (s-split "[-,]" pagination)))))

(defun pubmed-bibtex-key--shortyear (summary &optional _n _m)
  "Return the [shortyear] key pattern.
The last 2 digits of the publication year."
  (s-right 2 (pubmed-bibtex-key--year summary)))

(defun pubmed-bibtex-key--year (summary &optional _n _m)
  "Return a string with the publication year of the article SUMMARY."
  (let ((year (esxml-query "PubDate Year *" summary))
	(medlinedate (esxml-query "PubDate MedlineDate *" summary)))
    (cond
     (year
      year)
     ((string-match "\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)" medlinedate)
      (match-string 0 medlinedate))
     (t
      nil))))

(defun pubmed-bibtex-key--month (summary &optional _n _m)
  "Return a string with the journal publication month of the article SUMMARY."
  (let ((month (esxml-query "PubDate Month *" summary)))
    ;; If MONTH is a number
    (when (and month (string-match "[[:digit:]]+" month))
      ;; Convert the month number to the abbreviated month name
      (setq month (nth (1- (string-to-number month)) pubmed-months)))
    month))

(defun pubmed-bibtex-key--abbr (string)
  "Abbreviate STRING.
Only the first character and subsequent characters following white space will be included."
  (let* ((words (s-split-words string))
	 (counter 0)
	 selected)
    (dolist (word words)
      (push (substring word 0 1) selected)
      (setq counter (1+ counter)))
    (s-join " " (nreverse selected))))

(defun pubmed-bibtex-key--alphanum (string)
  "Remove non-alphanumeric characters from STRING."
  (replace-regexp-in-string "[^[:alnum:]]" "" string))

(defun pubmed-bibtex-key--ascii (string)
  "Remove non-ascii characters from STRING."
  (replace-regexp-in-string "[[:nonascii:]]" "" string))

(defun pubmed-bibtex-key--clean (string)
  "Remove invalid characters and transliterate unicode to ascii from STRING."
  (let ((regexp "[[:blank:]@',\#}{~%\\&\\^\\$]"))
    (unidecode (replace-regexp-in-string regexp "" string))))

(defun pubmed-bibtex-key--condense (string arg1)
  "Replace spaces in STRING with ARG1 in STRING."
  (replace-regexp-in-string "[[:blank:]]" arg1 string))

(defun pubmed-bibtex-key--nopunct (string)
  "Remove punctuation from STRING."
  (replace-regexp-in-string "[[:punct:]]" "" string))

(defun pubmed-bibtex-key--nopunctordash (string)
  "Remove punctuation and word-connecting dashes from STRING."
  (replace-regexp-in-string "[[:punct:]-]" "" string))

(defun pubmed-bibtex-key--postfix (string arg1)
  "Postfix STRING with ARG1."
  (when (string> string "")
    (concat string arg1)))

(defun pubmed-bibtex-key--prefix (string arg1)
  "Prefix STRING with ARG1."
  (when (string> string "")
    (concat arg1 string)))

(defun pubmed-bibtex-key--replace (string arg1 arg2)
  "Replace ARG1 with ARG2 in STRING."
  (when (string-match arg1 string)
    (replace-match arg2 t t string)))

(defun pubmed-bibtex-key--select (string arg1 arg2)
  "Select words from ARG1 to ARG2 from STRING."
  (let ((words (s-split-words string))
	(start (string-to-number arg1))
	(end (string-to-number arg2))
	(counter 0)
	selected)
    (dolist (word words)
      (when (and (>= counter (1- start)) (<= counter (1- end)))
	(push word selected))
      (setq counter (1+ counter)))
    (s-join " " (nreverse selected))))

(defun pubmed-bibtex-key--skipwords (string)
  "Return STRING, ignoring function words."
  (let* ((words (s-split-words string))
	 (counter 0)
	 selected)
    (dolist (word words)
      (unless (member (downcase word) pubmed-bibtex-function-words)
	(push word selected))
      (setq counter (1+ counter)))
    (s-join " " (nreverse selected))))

(defun pubmed-bibtex-key--substring (string arg1 &optional arg2)
  "Return a substring of STRING.
The returned string consists of the characters between ARG1 and
ARG2."
  (let ((start (string-to-number arg1))
	(number (string-to-number arg2)))
    (cond
     ((and arg1 arg2)
      (substring string (1- start) (1- (+ start number))))
     (arg1
      (substring string (1- start)))
     (t
      string))))

(defun pubmed-bibtex-key--titlecase (string)
  "Change the first character of all normal words to uppercase in STRING.
All function words (see `pubmed-bibtex-function-words') are
converted to lowercase."
  (let* ((words (s-split-words string))
	 titlecasestring)
    (dolist (word words)
      (if (not (member (downcase word) pubmed-bibtex-function-words))
	  (push (capitalize word) titlecasestring)
	(push (downcase word) titlecasestring)))
    (s-join " " (nreverse titlecasestring))))

(defun pubmed-bibtex-key--placeholder (string arg1)
  "Return STRING or ARG1 when STRING is NIL."
  (if string
      string
    arg1))

;;;; Footer

(provide 'pubmed-bibtex)

;;; pubmed-bibtex.el ends here
