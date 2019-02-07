Emacs-pubmed is obviously intended as a GNU Emacs interface to the PubMed database. I started the development to be able to search PubMed in my favorite environment. It is in an early development state and far from complete, but already usable. The most powerfull feature probably is the easy access to full text PDFs of most articles.

Emacs-pubmed uses the NCBI E-utilities to query the PubMed database. See <https://www.ncbi.nlm.nih.gov/books/NBK25500/>. Two methods are availabe to search PubMed:
M-x pubmed-search
     Simple PubMed query from the minibuffer.
M-x pubmed-advanced-search
     Advanced Search Builder with history modelled after <https://www.ncbi.nlm.nih.gov/pubmed/advanced>.

Entries are shown in a tabulated list in PubMed mode. The following keybindings are available:
"RET" Show the current entry
"f" Get the free, maybe not legal, full text PDF of the current entry.
"g" Get the free, legal, full text PDF of the current entry.
"n" Show the next entry
"p" Show the previous entry
"q" Quit window
"s" Search PubMed

Full text PDFs can be found by using the Unpaywall REST API or the Sci-Hub database.
- Using Unpaywall is legal and requires you to provide your email address by setting the value of UNPAYWALL-EMAIL in your .emacs:
(setq unpaywall-email "your_email@example.com")
- Using Sci-Hub may not be legal and requires you to provide the url by setting the value of SCIHUB-URL in your .emacs:
(setq scihub-url "http://url-of-sci-hub.com/")

Since May 1, 2018, NCBI limits access to the E-utilities unless you have an API key. See <https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/>. If you don't have an API key, E-utilities will still work, but you may be limited to fewer requests than allowed with an API key. Any computer (IP address) that submits more than three E-utility requests per second will receive an error message. This limit applies to any combination of requests to EInfo, ESearch, ESummary, EFetch, ELink, EPost, ESpell, and EGquery. 

First, you will need an NCBI account. If you don't have one already, register at <https://www.ncbi.nlm.nih.gov/account/>.

To create the key, go to the "Settings" page of your NCBI account. (Hint: after signing in, simply click on your NCBI username in the upper right corner of any NCBI page.) You'll see a new "API Key Management" area. Click the "Create an API Key" button, and copy the resulting key.

Use the key by setting the value of PUBMED-API_KEY in your .emacs:
(setq pubmed-api_key "1234567890abcdefghijklmnopqrstuvwxyz")
