+++
title = "Google ngrams in emacs"
author = ["Shane Mulligan"]
date = 2021-03-09T00:00:00+13:00
keywords = ["google", "nlp"]
draft = false
+++

## Summary {#summary}

I integrate the Google ngram viewer
functionality into emacs for suggesting words in context.


## Demonstration {#demonstration}

I demonstrate selecting alternative middle
word for the given context words.

<a title="asciinema recording" href="https://asciinema.org/a/w6DuW7w4gcbxVX8d8xEeIkljt" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/w6DuW7w4gcbxVX8d8xEeIkljt.svg" /></a>


## Code {#code}

<span class="underline">**custom.el configuration**</span>

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defcustom google-ngrams-corpus ""
  "Google ngrams corpus"
  :type 'string
  :group 'system-custom
  :initialize #'custom-initialize-default
  :options (list "15 # english 2012"
                 "16 # english fiction"
                 "26 # english 2019")
  :set (lambda (_sym value)
         (myrc-set (tr "-" "_" (sym2str _sym)) value)
         (set _sym (sor value)))
  ;; The default :initialize is custom-initialize-reset
  ;; And uses the :set function
  ;; :initialize (lambda
  :get (lambda (_sym)
         (let* ((yaml (yamlmod-read-file "/home/shane/notes/myrc.yaml"))
                (cfgval (sor (ht-get yaml (tr "-" "_" (sym2str _sym))))))

           (if cfgval
               (set _sym cfgval)
             "26 # english 2019"))))
{{< /highlight >}}

{{< figure src="/ox-hugo/ngram-custom.png" >}}

<span class="underline">**elisp**</span>

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun ngram-suggest (query)
  (str2lines (snc "ngram-complete" query)))

(defun gen-google-ngram-queries (s i)
  (-filter-not-empty-string
   (str2list
    (snc
     (concat
      "echo "
      (q s)
      " | google-ngram-query-combinations "
      (str i)
      " | perl -e 'print sort { length($b) <=> length($a) } <>'")))))

(defun ngram-query-replace-this ()
  (interactive)
  (if (not (selectionp))
      (let* ((line-str (chomp (current-line-string)))
             (col (current-column))
             (suggestions (ngram-suggest (fz (gen-google-ngram-queries line-str col)))))
        (if (-filter-not-empty-string suggestions)
            (let ((replacement (fz suggestions)))
              (if (sor replacement)
                  (nbfs replacement)))))))

(define-key my-mode-map (kbd "H-Q") 'ngram-query-replace-this)

(defun ngram-query-replace ()
  (interactive)
  (if (selectionp)
      (let* ((query (if (selection-p)
                        (selection)))
             (reformulated-query (if (string-match-p "\\*" query)
                                     query
                                   (let ((wildcard-word (fz (split-string query " " t))))
                                     (if wildcard-word
                                         (s-replace-regexp (eatify wildcard-word) "*" query)
                                       query))))
             (suggestions (ngram-suggest reformulated-query)))
        (if (-filter-not-empty-string suggestions)
            (let ((replacement (fz suggestions)))
              (if replacement
                  (progn
                    (cua-delete-region)
                    (insert replacement))))))))
{{< /highlight >}}

<span class="underline">**ngram-complete**</span>

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

postprocess() {
    sed "s/I 'm/I'm/g"
}

# corpus=15 # english 2012
# corpus=16 # english fiction
# corpus=26 # english 2019

: "${corpus:="$(myrc .google_ngrams_corpus | cut -d ' ' -f 1)"}"
: "${corpus:="26"}"

start=1800
end=2020
# end=1899

while [ $# -gt 0 ]; do opt="$1"; case "$opt" in
    "") { shift; }; ;;
    -c) {
        corpus="$2"
        shift
        shift
    }
    ;;

    -y) {
        start="$2"
        end="$2"
        shift
        shift
    }
    ;;

    -s) {
        start="$2"
        shift
        shift
    }
    ;;

    -e) {
        end="$2"
        shift
        shift
    }
    ;;

    *) break;
esac; done

awk1 | while IFS=$'\n' read -r line; do
    {
        phrase="$line"
        oci curl -s "https://books.google.com/ngrams/json?content=$(echo "$phrase" | urlencode | sed 's/%2A/*/g')&year_start=$start&year_end=$end&corpus=$corpus&smoothing=3" | jq -r .[].ngram | htmldecode.sh | postprocess
    } 0</dev/null
done | sed -u 1d | pavs
{{< /highlight >}}

The following python script generates ngram
queries for replacing the current word based
on the cursor index position.

<span class="underline">**google-ngram-query-combinations**</span>

{{< highlight python "linenos=table, linenostart=1" >}}
#!/usr/bin/env python3.6
# -*- coding: utf-8 -*-

import sys
s = sys.stdin.read()

len(sys.argv) > 1 or exit(1)

i = int(sys.argv[1])

r = s[i:]
l = s[:i]
l = " ".join(l.split())

try:
    if s[i - 1] == ' ':
        s = l + " " + r
    else:
        s = l + r

    i=len(l)

    tks = s.split()
    s = " ".join(tks)
    pos = s[:i].count(" ")

    tks[pos] = "*"

    for n in range(3, 6):
        subtks = tks[max(pos - (n-1), 0):min(pos + n,len(tks))]

        for l in list(zip(*(subtks[i:] for i in range(n)))):
            print(" ".join(l))
except:
    pass
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
echo "For the moment, let it accept 2 words to the left and 2 to the right" | google-ngram-query-combinations 20
{{< /highlight >}}

```bash
the moment, *
moment, * it
* it accept
For the moment, *
the moment, * it
moment, * it accept
* it accept 2
For the moment, * it
the moment, * it accept
moment, * it accept 2
* it accept 2 words
```

<a title="asciinema recording" href="https://asciinema.org/a/nKcNFBl9VjLfU1kFeBkx9TIBH" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/nKcNFBl9VjLfU1kFeBkx9TIBH.svg" /></a>