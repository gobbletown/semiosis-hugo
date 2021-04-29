+++
title = "Prolog and Natural-Language Analysis"
author = ["Shane Mulligan"]
date = 2021-04-30T00:00:00+12:00
keywords = ["prolog", "nlp"]
draft = false
+++

Book in question
: <span class="underline">Prolog and Natural-Language Analysis</span> by Fernando C. N. Pereira and Stuart M. Shieber


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


## Build a prolog database system in emacs {#build-a-prolog-database-system-in-emacs}

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


## Makee some upgrades to my Prolog environment {#makee-some-upgrades-to-my-prolog-environment}

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