# Emacs PubMed

Emacs-pubmed is a GNU Emacs interface to the PubMed database. It is in an
early state of development and far from complete, but already usable.
Emacs-pubmed uses the [NCBI
E-utilities](https://www.ncbi.nlm.nih.gov/books/NBK25500/) to query the PubMed
database. The most powerful feature probably is the easy access to fulltext
PDFs of many articles.

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

Use the key by customizing the variable `pubmed-api-key` or setting the
value in your `init.el` or `.emacs` file:

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
  customizing the variable `pubmed-openaccessbutton-api-key` or setting the value
  in your `init.el` or `.emacs` file:

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
  default, only `pubmed-pmc` is used. To add other fulltext functions, customize
  the variable `pubmed-fulltext-functions` or set the value in your `init.el` or
  `.emacs` file:

```lisp
(setq pubmed-fulltext-functions '(pubmed-pmc pubmed-openaccessbutton pubmed-unpaywall pubmed-scihub))
```

or

```lisp
(add-to-list 'pubmed-fulltext-functions 'pubmed-unpaywall t)
(add-to-list 'pubmed-fulltext-functions 'pubmed-openaccessbutton t)
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

The first key of a BibTeX entry the citation key, or the BibTeX key.
This key must be unique for all entries in your bibliography. It is
this identifier that you will use within your document to
cross-reference it to this entry (i.e., the identifier you use in the
\cite{} command in your LaTeX file). By default the key is composed of
a the PMID prefixed with the string "pmid". Alternatively, you could
use the "authoryear" citation style, that follow the loose standard in
which the author's surname followed by the year of publication is
used. This could be more convenient, because it enables you to guess
the key from looking at the reference. This protects you from
accidentally duplicating a reference in the BibTeX file and referring
to it under two different keys. However, multiple papers from the same
author and year will have duplicate citation keys. Untill this is
fixed, you have to correct this manually. To change the default
citation style to the "authoryear" scheme, you can customize of the
variable `pubmed-bibtex-citation-key` or set the value in your
`init.el` or `.emacs`:

```lisp
(setq pubmed-bibtex-citation-key "authoryear")
```

***Configuring default BibTeX file***

If you have a master BibTeX file, e.g. bibliography.bib, and want it
to serve as the default file to append or write the BibTeX reference
to, you can customize the variable `pubmed-default-bibtex-file` or set
the value in your `init.el` or `.emacs`:

```lisp
(setq pubmed-default-bibtex-file "/path/to/bibliography.bib")
```
