# Emacs PubMed

Emacs-pubmed is a GNU Emacs interface to the PubMed database. It is in an early
state of development and far from complete, but already usable. Emacs-pubmed
uses the [NCBI E-utilities](https://www.ncbi.nlm.nih.gov/books/NBK25500/) to
query the PubMed database. The most powerful feature probably is the easy access
to fulltext PDFs of many articles.

## Installation

**MELPA**

Emacs-pubmed is not yet available from MELPA.

**Development Version**

To follow or contribute to emacs-pubmed development, you can browse or clone the
Git repository [on GitLab](https://gitlab.com/fvdbeek/emacs-pubmed):

```
git clone https://gitlab.com/fvdbeek/emacs-pubmed.git
```

If you clone the repository directly, then make sure that Emacs can find it by
adding the following line to your startup file:

```lisp
(add-to-list 'load-path "/path/to/emacs-pubmed")
```

## Usage

To use emacs-pubmed, include the following in your `init.el` or `.emacs` file:

```lisp
(require 'pubmed)
(require 'pubmed-advanced-search)
```

Then use `M-x pubmed-search` or `M-x pubmed-advanced-search` to search PubMed:

- `M-x pubmed-search` Simple PubMed query from the minibuffer.

- `M-x pubmed-advanced-search` Advanced Search Builder with history modelled
after <https://www.ncbi.nlm.nih.gov/pubmed/advanced>.

Entries are shown in a tabulated list in `pubmed-mode`.

**Keybindings**

The following keybindings are available:

- <kbd>a</kbd>: Append the BibTeX references of the marked entries or current
  entry to file.
- <kbd>RET</kbd>: Show the summary of the current entry.
- <kbd>f</kbd>: Try to fetch the fulltext PDF of the current entry.
- <kbd>m</kbd>: Mark the current entry.
- <kbd>n</kbd>: Show the summary of the next entry.
- <kbd>p</kbd>: Show the summary of the previous entry.
- <kbd>q</kbd>: Quit window.
- <kbd>s</kbd>: Search PubMed.
- <kbd>u</kbd>: Unmark the current entry.
- <kbd>U</kbd>: Unmark all entries.
- <kbd>w</kbd>: Write the BibTeX references of the marked entries or current
  entry to file.
- <kbd>TAB</kbd>: Show the BibTeX references of the marked entries or current
  entry.

**Completion**

The `pubmed-search` function includes completion for PubMed suggestions. The
default key to invoke completion is the `C-M-i` or `<TAB>` command.

In `pubmed-advanced-search`, the "All Fields", "Author" and "Journal" search
boxes include completion for PubMed suggestions. The default key to invoke
completion is the `C-M-i` or `M-<TAB>` command, bound to `completion-at-point`.
On graphical displays, the `M-<TAB>` key is usually reserved by the window
manager for switching graphical windows, so you should type `C-M-i` or `<ESC>
<TAB>` instead.

**NCBI API key**

Since May 1, 2018, NCBI limits access to the E-utilities unless you have an API
key. See
<https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/>.
If you don't have an API key, E-utilities will still work, but you may be
limited to fewer requests than allowed with an API key. Any computer (IP
address) that submits more than three E-utility requests per second will receive
an error message. This limit applies to any combination of requests to EInfo,
ESearch, ESummary, EFetch, ELink, EPost, ESpell, and EGquery.

To obtain an API key, you will need an NCBI account. If you don't have one
already, register at <https://www.ncbi.nlm.nih.gov/account/>.

To create the key, go to the "Settings" page of your NCBI account. (Hint: after
signing in, simply click on your NCBI username in the upper right corner of any
NCBI page.) You'll see a new "API Key Management" area. Click the "Create an API
Key" button, and copy the resulting key.

Use the key by customizing the variable `pubmed-api-key` or setting the value in
your `init.el` or `.emacs` file:

```lisp
(setq pubmed-api-key "1234567890abcdefghijklmnopqrstuvwxyz")
```

**Full text PDFs**

Full text PDFs can be found by using [PubMed Central®
(PMC)](https://www.ncbi.nlm.nih.gov/pmc/), [Open Access
Button](https://openaccessbutton.org/api)
[Unpaywall](https://unpaywall.org/products/api) or Sci-Hub:

- The PMC fulltext function is invoked by `M-x pubmed-get-pmc`. PubMed Central®
  (PMC) is a free full-text archive of biomedical and life sciences journal
  literature at the U.S. National Institutes of Health's National Library of
  Medicine (NIH/NLM).

- The Open Access Button fulltext function is invoked by `M-x
  pubmed-get-openaccessbutton`. Using Open Access Button is legal. Although most
  API operations do not require authorisation, obtaining your own API key is
  encouraged. To create the key, register at
  <https://openaccessbutton.org/account?next=/api>. Provide your API key by
  customizing the variable `pubmed-openaccessbutton-api-key` or setting the
  value in your `init.el` or `.emacs` file:

  ```lisp
  (setq pubmed-openaccessbutton-api-key "1234567890abcdefghijklmnopqrstuvwxyz")
  ```

- The Unpaywall fulltext function is invoked by `M-x pubmed-get-unpaywall`.
  Using Unpaywall is legal and requires you to provide your email address by
  customizing the variable `pubmed-unpaywall-email` or setting the value in your
  `init.el` or `.emacs` file:

```lisp
(setq pubmed-unpaywall-email "your_email@example.com")
```

- The Sci-Hub fulltext function is invoked by `M-x pubmed-get-scihub`. Using
Sci-Hub may not be legal and requires you to provide the url by customizing the
variable `pubmed-scihub-url` or setting the value in your `init.el` or `.emacs`
file:

```lisp
(require 'pubmed-scihub)
(setq pubmed-scihub-url "http://url-of-sci-hub.com/")
```

- The command `M-x pubmed-get-fulltext` (or <kbd>f</kbd>) tries to fetch the
  fulltext PDF of the marked entries or current entry, using multiple methods.
  The functions in `pubmed-fulltext-functions` are tried in order, until a
  fulltext PDF is found.

- The value of the variable `pubmed-fulltext-functions` should be a list of
  functions, which are tried in order by `pubmed-get-fulltext` to fetch fulltext
  articles. To change the behavior of `pubmed-get-fulltext`, remove, change the
  order of, or insert functions in this list. When the command
  `pubmed-get-fulltext` runs, it calls the functions in the list one by one,
  without any argument. Each function should return nil if it is unable to find
  a fulltext article of the entry at point. Otherwise it should return the
  buffer of the PDF and show the PDF in a new frame as a side effect. By
  default, only `pubmed-pmc` and `pubmed-openaccessbutton` are used. To add
  other fulltext functions, customize the variable `pubmed-fulltext-functions`
  or set the value in your `init.el` or `.emacs` file:

```lisp
(setq pubmed-fulltext-functions '(pubmed-pmc pubmed-openaccessbutton pubmed-unpaywall pubmed-scihub))
```

or

```lisp
(add-to-list 'pubmed-fulltext-functions 'pubmed-unpaywall t)
(add-to-list 'pubmed-fulltext-functions 'pubmed-scihub t)
```

**BibTeX**

The PubMed document summaries (DocSums) can be exported to BibTeX references.

- The command `M-x pubmed-show-bibtex` (or <kbd>TAB</kbd>) opens a new frame
  with the BibTeX references of the marked entries, or if there are none marked,
  the current entry in `bibtex-mode`.

- The command `M-x pubmed-write-bibtex` (or <kbd>w</kbd>) writes the BibTeX
  references to a file.

- The command `M-x pubmed-append-bibtex` (or <kbd>a</kbd>) appends the BibTeX
  references to a file.

***Choosing BibTeX field types***

PubMed includes citations for journal articles and a subset of books and book
chapters available on the NCBI Bookshelf. These are declared as @article, @book,
and @incollection, respectively.

The @article reference type is used for an article from a magazine or a journal.
Required fields are: author, title, journal, year. Optional fields are: volume,
number, pages, month, note.

The @book reference type is used for a published book. Required fields are:
author/editor, title, publisher, year. Optional fields are: volume/number,
series, address, edition, month, note.

The @incollection is used for a section of a book having its own title. Required
fields are: author, title, booktitle, publisher, year. Optional fields are:
editor, volume/number, series, type, chapter, pages, address, edition, month,
note.

Of the optional fields, the series, type, and chapter fields are not included in
the PubMed DocSums. When the note field is enabled, it is set to an empty value
for later use. Besides the required and optional fields, the following
non-standard fields can be included in the BibTeX entries: issn, pubmed, pii,
doi, url.

The optional and non-standard fields can be included by customizing the
corresponding variables or setting the values in your `init.el` or `.emacs`,
e.g.

```lisp
(setq pubmed-bibtex-article-number t)
```

***Choosing BibTeX citation key***

The first key of a BibTeX entry the citation key, or the BibTeX key. This key
must be unique for all entries in your bibliography. It is this identifier that
you will use within your document to cross-reference it to this entry (i.e., the
identifier you use in the \cite{} command in your LaTeX file).

By default, the author-year citation style is used that follows the loose
standard in which the author's surname is followed by the year of publication. A
formatter pattern language is provided that mostly follows the JabRef key
formatting syntax. The default author-year citation style is produced by the
[auth][year] key pattern. If the key is not unique in the current database, it
is made unique by adding one of the letters a-z until a unique key is found.

To change the default key pattern, you can customize the variable
`pubmed-bibtex-keypattern` or set the value in your `init.el` or `.emacs`:

```lisp
(setq pubmed-bibtex-keypattern "[auth][year]")
```

**** Special field markers

Several special field markers are offered, which extract only a specific part of
a field.

***** Author-related key patterns

- [auth]: The last name of the first author
- [authors]: The last name of all authors
- [authorLast]: The last name of the last author
- [authorsN]: The last name of up to N authors. If there are more authors,
  \"EtAl\" is appended.
- [authorsAlpha]: Corresponds to the BibTeX style \"alpha\". One author: First
  three letters of the last name. Two to four authors: First letters of last
  names concatenated. More than four authors: First letters of last names of
  first three authors concatenated. \"+\" at the end.
- [authIniN]: The beginning of each author's last name, using no more than N
  characters.
- [authorIni]: The first 5 characters of the first author's last name, and the
  last name initials of the remaining authors.
- [authN]: The first N characters of the first author's last name
- [authN_M]: The first N characters of the Mth author's last name
- [auth.auth.ea]: The last name of the first two authors, and \".ea\" if there
  are more than two.
- [auth.etal]: The last name of the first author, and the last name of the
  second author if there are two authors or \".etal\" if there are more than
  two.
- [authEtAl]: The last name of the first author, and the last name of the second
  author if there are two authors or \"EtAl\" if there are more than two. This
  is similar to auth.etal. The difference is that the authors are not separated
  by \".\" and in case of more than 2 authors \"EtAl\" instead of \".etal\" is
  appended.
- [authshort]: The last name if one author is given; the first character of up
  to three authors' last names if more than one author is given. A plus
  character is added, if there are more than three authors.
- [authForeIni]: The forename initial of the first author.
- [authorLastForeIni]: The forename initial of the last author.

Note: If there is no author (as in the case of an edited book), then all of the
above [auth...] markers will use the editor(s) (if any) as a fallback. Thus, the
editor(s) of a book with no author will be treated as the author(s) for
label-generation purposes. If you do not want this behaviour, i.e. you require a
marker which expands to nothing if there is no author, use pureauth instead of
auth in the above codes. For example, [pureauth], or [pureauthors3].

***** Editor-related key patterns

- [edtr]: The last name of the first editor
- [edtrIniN]: The beginning of each editor's last name, using no more than N
  characters
- [editors]: The last name of all editors
- [editorLast]: The last name of the last editor
- [editorIni]: The first 5 characters of the first editor's last name, and the
  last name initials of the remaining editors.
- [edtrN]: The first N characters of the first editor's last name
- [edtrN_M]: The first N characters of the Mth editor's last name
- [edtr.edtr.ea]: The last name of the first two editors, and \".ea\" if there
  are more than two.
- [edtrshort]: The last name if one editor is given; the first character of up
  to three editors' last names if more than one editor is given. A plus
  character is added, if there are more than three editors.
- [edtrForeIni]: The forename initial of the first editor.
- [editorLastForeIni]: The forename initial of the last editor.

***** Title-related key patterns

- [shorttitle]: The first 3 words of the title, ignoring any function words (see
  below). For example, An awesome paper on JabRef becomes AwesomePaperJabref.
- [veryshorttitle]: The first word of the title, ignoring any function words
  (see below). For example, An awesome paper on JabRef becomes Awesome.
- [camel]: Capitalize all the words of the title. For example, An awesome paper
  on JabRef becomes An Awesome Paper On Jabref.
- [title]: Capitalize all the significant words of the title. For example, An
  awesome paper on JabRef becomes An Awesome Paper on Jabref.

***** Other key patterns

- [firstpage]: The number of the first page of the publication (Caution: this
  will return the lowest number found in the pages field, since BibTeX allows
  7,41,73--97 or 43+.)
- [pageprefix]: The non-digit prefix of pages (like \"L\" for L7) or \"\" if no
  non-digit prefix exists (like \"\" for 7,41,73--97).
- [keywordN]: Keyword number N from the \"keywords\" field, assuming keywords
  are separated by commas or semicolons.
- [lastpage]: The number of the last page of the publication (See the remark on
  firstpage).
- [shortyear]: The last 2 digits of the publication year.

Note: the functions above all have the clean filter automatically applied to
them. If you want more control, you can disable this by configuring the
`pubmed-bibtex-apply-clean` variable.

***** Filters

A field name (or one of the above pseudo-field names) may optionally be followed
by one or more filters. Filters are applied in the order they are specified.

- abbr: Abbreviates the text produced by the field name or special field marker.
  Only the first character and subsequent characters following white space will
  be included. For example, [journal:abbr] would from the journal name \"Journal
  of Fish Biology\" produce \"JoFB\", [title:abbr] would from the title \"An
  awesome paper on JabRef\" produce \"AAPoJ\", [camel:abbr] would from the title
  \"An awesome paper on JabRef\" produce \"AAPOJ\".
- alphanum: clears out everything but unicode alphanumeric characters (unicode
  character classes L and N).
- ascii: removes all non-ascii characters.
- capitalize: Changes the first character of each word to uppercase, all other
  characters are converted to lowercase. For example, an example title will be
  converted to An Example Title.
- clean: transliterates the citation keys and removes unsafe characters.
- condense: this replaces spaces in the value passed in. You can specify what to
  replace it with by adding it as a parameter, e.g condense=_ will replace
  spaces with underscores. Parameters should not contain spaces unless you want
  the spaces in the value passed in to be replaced with those spaces in the
  parameter.
- fold: tries to replace diacritics with ascii look-alikes. Removes non-ascii
  characters it cannot match.
- lower: Forces the text inserted by the field marker to be in lowercase. For
  example, [auth:lower] expands the last name of the first author in lowercase.
- nopunct: Removes punctuation.
- nopunctordash: Removes punctuation and word-connecting dashes.
- postfix: postfixes with its parameter, so postfix=_ will add an underscore to
  the end if, and only if, the value it is supposed to postfix isn't empty.
- prefix: prefixes with its parameter, so prefix=_ will add an underscore to the
  front if, and only if, the value it is supposed to prefix isn't empty. If you
  want to use a reserved character (such as : or \), you'll need to add a
  backslash (\) in front of it.
- replace: replaces text, case insensitive; :replace=.etal,&etal will replace
  .EtAl with &etal.
- select: selects words from the value passed in. The format is
  select=start,number (1-based), so select=1,4 would select the first four
  words. If number is not given, all words from start to the end of the list are
  selected. It is important to note that 'select' works only on values that have
  the words separated by whitespace, so the caveat below applies.
- sentencecase: Changes the first character of the first word to uppercase, all
  remaining words are converted to lowercase. Example: an Example Title will be
  converted to An example title.
- skipwords: filters out common words like 'of', 'the', … the list of words can
  be seen and changed by customizing the variable
  `pubmed-bibtex-function-words`.
- substring: (substring=start,n) selects n (default: all) characters starting at
  start (default: 1).
- titlecase: Changes the first character of all normal words to uppercase, all
  function words (see `pubmed-bibtex-function-words`) are converted to
  lowercase. Example: example title with An function Word will be converted to
  Example Title with an Function Word.
- upper: Forces the text inserted by the field marker to be in uppercase. For
  example, [auth:upper] expands the last name of the first author in uppercase.
- (x): The string between the parentheses will be inserted if the field marker
  preceding this filter resolves to an empty value. The placeholder x may be any
  string. For instance, the marker [volume:(unknown)] will return the entry's
  volume if set, and the string unknown if the entry's volume field is not set.
  
***Configuring default BibTeX file***

If you have a master BibTeX file, e.g. bibliography.bib, and want it to serve as
the default file to append or write the BibTeX reference to, you can customize
the variable `pubmed-default-bibtex-file` or set the value in your `init.el` or
`.emacs`:

```lisp
(setq pubmed-default-bibtex-file "/path/to/bibliography.bib")
```
