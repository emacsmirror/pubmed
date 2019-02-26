# Emacs PubMed

`Emacs-pubmed` is a GNU Emacs interface to the PubMed database. I started the
development to be able to search PubMed in my favorite environment. It is in an
early state of development and far from complete, but already usable.
`Emacs-pubmed` uses the [NCBI
E-utilities](https://www.ncbi.nlm.nih.gov/books/NBK25500/) to query the PubMed
database. The most powerful feature probably is the easy access to full text
PDFs of most articles.

## Installation

**MELPA**

`Emacs-pubmed` is not yet available from MELPA.

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

Entries are shown in a tabulated list in `PubMed mode'.

**Keybindings**

The following keybindings are available:

- <kbd>RET</kbd>: Show the current entry.
- <kbd>f</kbd>: Get the free, maybe not legal, full text PDF of the current
  entry.
- <kbd>g</kbd>: Try to fetch the fulltext PDF of the current entry.
- <kbd>m</kbd>: Mark the current entry.
- <kbd>n</kbd>: Show the next entry.
- <kbd>p</kbd>: Show the previous entry.
- <kbd>q</kbd>: Quit window.
- <kbd>s</kbd>: Search PubMed.
- <kbd>u</kbd>: Unmark the current entry.

**Completion**

If pubmed-search-completion is set to non-nil, the `pubmed-search` function
includes completion for PubMed suggestions. The default key to invoke completion
is `<TAB>'.

In `pubmed-advanced-search`, the "All Fields", "Author" and "Journal" search
boxes include completion for PubMed suggestions. The default key to invoke
completion is the `C-M-i' or `M-<TAB>' command, bound to `completion-at-point'.
On graphical displays, the `M-<TAB>' key is usually reserved by the window
manager for switching graphical windows, so you should type `C-M-i' or `<ESC>
<TAB>' instead.


**NCBI API key**

Since May 1, 2018, NCBI limits access to the E-utilities unless you have an API
key. See
<https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/>.
If you don't have an API key, E-utilities will still work, but you may be
limited to fewer requests than allowed with an API key. Any computer (IP
address) that submits more than three E-utility requests per second will receive
an error message. This limit applies to any combination of requests to EInfo,
ESearch, ESummary, EFetch, ELink, EPost, ESpell, and EGquery.

First, you will need an NCBI account. If you don't have one already, register at
<https://www.ncbi.nlm.nih.gov/account/>.

To create the key, go to the "Settings" page of your NCBI account. (Hint: after
signing in, simply click on your NCBI username in the upper right corner of any
NCBI page.) You'll see a new "API Key Management" area. Click the "Create an API
Key" button, and copy the resulting key.

Use the key by setting the value of PUBMED-API_KEY in your `init.el` or `.emacs`
file:

```lisp
(setq pubmed-api_key "1234567890abcdefghijklmnopqrstuvwxyz")
```

**Full text PDFs**

Full text PDFs can be found by using
[Unpaywall](https://unpaywall.org/products/api) or Sci-Hub:

- The Unpaywall fulltext function is invoked by `M-x
  pubmed-get-unpaywall`. Using Unpaywall is legal and requires you to
  provide your email address by setting the value of `unpaywall-email`
  in your `init.el` or `.emacs` file:

```lisp
(setq unpaywall-email "your_email@example.com")
```

- The Sci-Hub fulltext function is invoked by `M-x pubmed-get-scihub`.
Using Sci-Hub may not be legal and requires you to provide the url by
setting the value of `scihub-url` in your `init.el` or `.emacs` file:

```lisp
(require 'pubmed-scihub)
(setq scihub-url "http://url-of-sci-hub.com/")
```

- The command `M-x pubmed-get-fulltext` (or the <kbd>g</kbd>) tries to
  fetch the fulltext PDF of the current entry, using multiple methods.
  The functions in `pubmed-fulltext-functions` are tried in order,
  until a fulltext PDF is found.

- The value of the variable `pubmed-fulltext-functions` should be a
  list of functions, which are tried in order by `pubmed-get-fulltext`
  to fetch fulltext articles. To change the behavior of
  `pubmed-get-fulltext`, remove, change the order of, or insert
  functions in this list. When the command `pubmed-get-fulltext` runs,
  it calls the functions in the list one by one, without any argument.
  Each function should return `nil` if it is unable to find a fulltext
  article of the entry at point. Otherwise it should return the buffer
  of the PDF and show the PDF in a new frame as a side effect. By
  default, only `pubmed-get-unpaywall` is used. To add
  `pubmed-get-scihub`, set the value of `pubmed-fulltext-functions` in
  your `init.el` or `.emacs` file:

```lisp
(setq pubmed-fulltext-functions '(pubmed-get-unpaywall pubmed-get-scihub))
```

or

```lisp
(add-to-list 'pubmed-fulltext-functions 'pubmed-get-scihub t)
```
