+++
title = "Universal file parser and query tool in emacs"
author = ["Shane Mulligan"]
date = 2021-04-29T00:00:00+12:00
keywords = ["antlr", "emacs", "semantic"]
draft = false
+++

## Summary {#summary}

I make a way to get a more abstract
representation of any type of file or buffer in emacs
utilising parsers, `antlr4` in particular.

I also want to be able to query the contents
of the source code.

I also want to be able to query prose programmatically.


### Motivation {#motivation}

In the past I have used `ctags` to get simple
lists of source code components such as
functions and classes. I would like to have
much finer control over queries to source
code.

The reason I want an automated way of parsing
and then querying source code and prose is so
I can build tooling which automates it and be
programming at a higher abstraction level.


### External parsers used {#external-parsers-used}

-   `antlr`
-   GitHub `semantic`
-   `json2hcl`
-   `ej`
-   `jq`


## Problems encountered {#problems-encountered}

`antlr` seems to be fairly unreliable at
parsing, or at least is too pedantic for this purpose.

I will attempt to do this with GitHub's
`semantic` parser as it is designed to be a
pragmatic parser.


## Demonstration {#demonstration}


### Going from `edn` to `json` to `json schema` {#going-from-edn-to-json-to-json-schema}

All I need to do is press `M-&` 2 times to do this.

The idea is that every time you press `M-&`,
it takes you to a more abstract representation
of what you are looking at.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/EYxLJ4n65VrgGGfmanUKNyrbz" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/EYxLJ4n65VrgGGfmanUKNyrbz.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/EYxLJ4n65VrgGGfmanUKNyrbz.js" id="asciicast-EYxLJ4n65VrgGGfmanUKNyrbz" async></script>


### Using the universal antlr parser {#using-the-universal-antlr-parser}

<script src="https://asciinema.org/a/q7QBII745RFxIUdsBwbwVDNfl.js" id="asciicast-q7QBII745RFxIUdsBwbwVDNfl" async></script>


## Steps {#steps}


### Set up antlr {#set-up-antlr}

Follow the setup instructions.

{{< highlight bash "linenos=table, linenostart=1" >}}
curl -O https://www.antlr.org/download/antlr-4.7.1-complete.jar
{{< /highlight >}}

Create the `antlr4` script.

`antlr4`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

export CLASSPATH=".:$HOME/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH"

sn="$(basename "$0")"

case "$sn" in
    grun) {
        java -Xmx500M -cp "/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig "$@"
    }
    ;;

    *) {
        java -Xmx500M -cp "/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.Tool "$@"
    }
    ;;
esac
{{< /highlight >}}

Create the `universal-antlr-parse` script.

`universal-antlr-parse`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

grammar_fp="$1"
src="$2"

stdin_exists() {
    ! [ -t 0 ] && ! test "$(readlink /proc/$$/fd/0)" = /dev/null
}

if test -f "$src"; then
    src="$(cat "$src")"
fi

if ! test -n "$src" && stdin_exists; then
    src="$(cat)"
fi

# grammar_fp=$MYGIT/antlr/grammars-v4/json/JSON.g4

cd $MYGIT/mullikine/universal-antlr-clojure-visitor-interpreter
lein run "$grammar_fp" "$src" | pavs
{{< /highlight >}}


### Obtain the grammars {#obtain-the-grammars}

<https://github.com/antlr/grammars-v4>


### Create a parsing utility in clojure {#create-a-parsing-utility-in-clojure}

Code
: <http://github.com/mullikine/universal-antlr-clojure-visitor-interpreter>

<!--listend-->

{{< highlight clojure "linenos=table, linenostart=1" >}}
(ns universal-antlr-clojure-visitor-interpreter.core
  (:gen-class)
  (:require [clj-antlr.core :as antlr]
            [clojure.pprint :as pp]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; Use :throw? to ignore errors
  (def parser (antlr/parser (first args) {:throw? false}))
  (pp/pprint (parser (slurp (second args)))))
{{< /highlight >}}

`demo`: Creating a parse tree with `antlr`.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/QFTXJWTmZupXuLcLGpirm4NIl" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/QFTXJWTmZupXuLcLGpirm4NIl.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/QFTXJWTmZupXuLcLGpirm4NIl.js" id="asciicast-QFTXJWTmZupXuLcLGpirm4NIl" async></script>


### Create an interface in emacs {#create-an-interface-in-emacs}

`my-file-parsers.el`

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
;; Mode is not enough information to determine

(defun antlr-list-all-grammars ()
  (snc "cd $MYGIT/antlr/grammars-v4/; find . -name '*.g4' | xargs -l basename | sed 's/\..*//'"))

(defun antlr-grammar-path-from-name (name)
  (let* ((cmd (format
               "cd $MYGIT/antlr/grammars-v4/; find . -iname '%s.g4' | sed 's/.\\///' | head -n 1"
               name))
         (result (snc cmd)))
    (if (sor result)
        (umn (concat "$MYGIT/antlr/grammars-v4/"
                     result)))))

;; $MYGIT/antlr/grammars-v4/

(defun get-buffer-python-version ()
  (if (string-equal (detect-language) "python")
      (snc (cmd "vermin" (tf "python" (buffer-string))))))

(defun antlr-detect-language ()
  ;; Detecting the language is not good enough
  ;; Sometimes I also need to know the language version, such as python3
  (let ((lang (detect-language)))
    (cond ((string-equal lang "python")
           (concat "python" (get-buffer-python-version)))
          (t lang))))

;; universal-antlr-parse "$MYGIT/antlr/grammars-v4/json/JSON.g4" "[4, 5, 6]"
(defset file-parser-2-tuples
  '(((major-mode-p 'terraform-mode)
     . "json2hcl -reverse")
    ((major-mode-p 'json-mode)
     . "zh -j")
    ((and (major-mode-p 'clojure-mode)
          (string-equal "edn" (f-ext (get-path))))
     . "ej | jq .")
    ((or (and (major-mode-p 'c++-mode)
              (string-equal "cpp" (f-ext (get-path))))
         (and (major-mode-p 'python-mode)
              (string-equal "py" (f-ext (get-path))))
         (and (major-mode-p 'java-mode)
              (string-equal "java" (f-ext (get-path))))
         (and (major-mode-p 'haskell-mode)
              (string-equal "hs" (f-ext (get-path)))))
     . (snc (cmd "semantic-parse" (get-path))))
    ((sor (antlr-grammar-path-from-name (f-ext (get-path))))
     . (snc (cmd "universal-antlr-parse" (antlr-grammar-path-from-name (f-ext (get-path))) (tf "code" (selection-or-buffer-string)))))
    ((sor (antlr-grammar-path-from-name (antlr-detect-language)))
     . (snc (cmd "universal-antlr-parse" (antlr-grammar-path-from-name (antlr-detect-language)) (tf "code" (selection-or-buffer-string)))))))

(defun assoc-collect-true (al)
  (-distinct
   (-flatten
    (cl-loop
     for
     kv
     in
     al
     collect
     (if (eval (car kv)) (cdr kv))))))

(defun assoc-get-first-true (al)
  (car (assoc-collect-true al)))


;; (defun get-parse-for-file (path)
;;   (interactive (list (current-path)))

;;   (assoc 'terraform-mode file-parser-2-tuples))

(defun parse-current-buffer ()
  (interactive)

  (let ((parser
         (assoc-get-first-true file-parser-2-tuples)))
    (if parser
        (let ((parse
               (if (stringp parser)
                   (snc (concat parser " 2>&1") (buffer-string))
                 (eval parser))))
          (if (sor parse)
              (with-current-buffer
                  (nbfs
                   parse)
                (detect-language-set-mode))
            (message "No parse created")))))

  ;; (let* (;; (p (current-path))
  ;;        (cm major-mode)
  ;;        (parser (assoc cm file-parser-2-tuples)))
  ;;   (if parser
  ;;       (let ((parse (snc (concat (cdr parser) " 2>&1") (buffer-string))))
  ;;         (if (sor parse)
  ;;             (with-current-buffer
  ;;                 (nbfs
  ;;                  parse)
  ;;               (detect-language-set-mode))
  ;;           (message "No parse created")))))
  )

(define-key my-mode-map (kbd "H-&") 'parse-current-buffer)

(provide 'my-file-parsers)
{{< /highlight >}}


## Parser scripts {#parser-scripts}


### `semantic-parse` (GitHub Semantic) {#semantic-parse--github-semantic}

`semantic-parse`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

fp="$1"
test -f "$fp" || exit 1

rp="$(realpath "$fp")"
bn="$(basename "$fp")"
dn="$(dirname "$rp")"

cwdslug="$(p "$dn" | slugify)"

unbuffer docker run \
    --rm \
    -v "$dn:/$cwdslug" \
    -w "/$cwdslug" \
    -ti \
    --entrypoint= \
    docker.pkg.github.com/github/semantic/semantic:sha_248a1b3646643613960e444fe8ab6623224d47b1 \
    /usr/local/bin/semantic \
    parse \
    "$bn" |
        erase-trailing-whitespace |
        pa -E "tf lisp | xa orspe"
{{< /highlight >}}


## Parser language support {#parser-language-support}


### GitHub Semantic {#github-semantic}

| Language   | Support |
|------------|---------|
| Ruby       | ✓       |
| JavaScript | ✓       |
| TypeScript | ✓       |
| Python     | ✓       |
| Go         | ✓       |
| PHP        | ✓       |
| Java       | ✗       |
| JSON       | ✓       |
| JSX        | ✓       |
| TSX        | ✓       |
| CodeQL     | ✓       |
| Haskell    | ✗       |