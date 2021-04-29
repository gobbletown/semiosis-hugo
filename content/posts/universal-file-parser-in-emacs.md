+++
title = "universal file parser in emacs"
author = ["Shane Mulligan"]
date = 2021-04-29T00:00:00+12:00
keywords = ["antlr", "emacs"]
draft = false
+++

## Summary {#summary}

I make a way to get a more abstract
representation of any buffer in emacs
utilising parsers, `antlr4` in particular.


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


### Obtain the grammars {#obtain-the-grammars}

<https://github.com/antlr/grammars-v4>


### Create a parsing utility in clojure {#create-a-parsing-utility-in-clojure}

Code
: <http://github.com/mullikine/universal-antlr-clojure-visitor-interpreter>


### Create an interface in emacs {#create-an-interface-in-emacs}

`my-file-parsers.el`

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
;; Mode is not enough information to determine

;; universal-antlr-parse "$MYGIT/antlr/grammars-v4/json/JSON.g4" "[4, 5, 6]"
(defset file-parser-2-tuples
  '(((major-mode-p 'terraform-mode)
     . "json2hcl -reverse")
    ((major-mode-p 'json-mode)
     . "zh -j")
    ((and (major-mode-p 'clojure-mode)
          (string-equal "edn" (f-ext (get-path))))
     . "ej | jq .")))

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
        (let ((parse (snc (concat parser " 2>&1") (buffer-string))))
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