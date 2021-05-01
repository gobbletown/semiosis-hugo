+++
title = "Prolog and Natural-Language Analysis"
author = ["Shane Mulligan"]
date = 2021-04-30T00:00:00+12:00
keywords = ["prolog", "nlp"]
draft = false
+++

Book in question
: <span class="underline">Prolog and Natural-Language Analysis</span> by Fernando C. N. Pereira and Stuart M. Shieber ([PDF](http://library.lol/main/534E15BB9D6CCB1E05CBA13E2BB8C849))


Notes on Semantic Nets and Frames
: <http://www.eecs.qmul.ac.uk/~mmh/AINotes/AINotes4.pdf>


## Review {#review}

This book has been interesting so far.

On reading the 1st chapter, I discovered how
to build Semantic Networks in prolog.

On reading the 2nd chapter I realised I would like to make a
NL parser in `prolog`. It would a fast way to
explore building my own programmatic (as
opposed to transformer-based) NL parser.

`writing this blog article`

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/Wm1oQDZHQCFRCUwDT40LPuGRo" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Wm1oQDZHQCFRCUwDT40LPuGRo.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/Wm1oQDZHQCFRCUwDT40LPuGRo.js" id="asciicast-Wm1oQDZHQCFRCUwDT40LPuGRo" async></script>


## Extend emacs to do write `prolog` more easily {#extend-emacs-to-do-write-prolog-more-easily}

-   Automate the writing of:
    -   axioms
    -   facts
-   Fuzzy search to build new rules
    -   Construct the database


### Build a prolog database system in emacs {#build-a-prolog-database-system-in-emacs}

{{< highlight prolog "linenos=table, linenostart=1" >}}
professor(terry).
professor(roger).
professor(bertrand).
professor(gottlob).

concerns(shrdlu, blocks).
concerns(lunar, rocks).
concerns(sam, stories).

concerns(principia, logic).
concerns(principia, mathematics).
concerns(begriffsschrift, logic).
{{< /highlight >}}


### Learn the built in functions first {#learn-the-built-in-functions-first}


#### remember {#remember}

| kb    | f                            |       |
|-------|------------------------------|-------|
| `M-e` | `prolog-end-of-clause`       | `nil` |
| `M-a` | `prolog-beginning-of-clause` | `nil` |


#### Rebind these {#rebind-these}

-   prolog-consult-region
-   prolog-consult-file
-   prolog-consult-file

<!--listend-->

{{< highlight text "linenos=table, linenostart=1" >}}
C-c C-z         run-prolog
C-c C-d         prolog-debug-on
C-c C-n         prolog-insert-predicate-template
C-c C-p         prolog-consult-predicate
C-c C-r         prolog-consult-region
C-c C-s         prolog-insert-predspec
C-c C-t         prolog-trace-on
C-c C-c b       prolog-compile-buffer
C-c C-c f       prolog-compile-file
C-c C-c p       prolog-compile-predicate
C-c C-c r       prolog-compile-region
C-c C-v C-s     prolog-view-predspec
C-c C-v a       prolog-variables-to-anonymous
{{< /highlight >}}

| kb        | f                       |       |
|-----------|-------------------------|-------|
| `C-c C-b` | `prolog-consult-buffer` | `nil` |


### new unit clauses {#new-unit-clauses}


#### new relation/clause (wrote) {#new-relation-clause--wrote}

{{< highlight prolog "linenos=table, linenostart=1" >}}
wrote(roger, sam).
{{< /highlight >}}


#### new facts {#new-facts}

{{< highlight prolog "linenos=table, linenostart=1" >}}
program(lunar).
{{< /highlight >}}


### Create rules {#create-rules}

{{< highlight prolog "linenos=table, linenostart=1" >}}
author(Person) :- book(Book), wrote(Person, Book).
{{< /highlight >}}


### query {#query}


#### yes/no (provable) queries {#yes-no--provable--queries}

{{< highlight prolog "linenos=table, linenostart=1" >}}
:- wrote(terry, shrdlu).
{{< /highlight >}}

{{< highlight prolog "linenos=table, linenostart=1" >}}
:- program(principia).
{{< /highlight >}}


#### Search queries (variable/wildcard) {#search-queries--variable-wildcard}

:- wrote(Who,shrdlu).


## Make some upgrades to my Prolog environment {#make-some-upgrades-to-my-prolog-environment}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(require 'ob-prolog)

;; Also see my-perl.el

;; TODO Start by making functions for constructing prolog databases

;; =atomic formula= wrote(X,Y)
;; wrote(roger, sam). -> wrote
(defun pro-get-relation-names ()
  (snc "sed -n 's/\\b\\([a-z]\\+\\)([a-z]\\+, \\?[a-z]\\+).*/\\1/p'" (buffer-string)))

;; Permute the atomic formulae with wildcards for querying


;; =base clause=, which represents a simple fact
(defun pro-get-base-clause-names ()
  (snc "sed -n 's/\\b\\([a-z]\\+\\)([a-z]\\+).*/\\1/p'" (buffer-string)))

(define-key prolog-mode-map (kbd "M-o b") 'prolog-consult-buffer)
(define-key prolog-mode-map (kbd "M-o f") 'prolog-consult-file)
(define-key prolog-mode-map (kbd "M-o r") 'prolog-consult-region)
(define-key prolog-mode-map (kbd "M-o M-d") 'prolog-debug-on)
(define-key prolog-mode-map (kbd "M-o M-n") 'prolog-insert-predicate-template)

(provide 'my-prolog)
{{< /highlight >}}