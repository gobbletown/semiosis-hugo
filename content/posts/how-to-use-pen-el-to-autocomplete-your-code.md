+++
title = "How to use Pen.el to autocomplete your code"
author = ["Shane Mulligan"]
date = 2021-07-31T00:00:00+12:00
keywords = ["gpt", "emacs", "openai", "pen"]
draft = false
+++

## Summary {#summary}

I will just do some demonstrations on actually
using `Pen.el` from within emacs.


## Config {#config}


### Pen map and Acolyte-mode map {#pen-map-and-acolyte-mode-map}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(define-key pen-map (kbd "M-1") #'pen-company-filetype-word)
(define-key pen-map (kbd "M-2") #'pen-company-filetype-words)
(define-key pen-map (kbd "M-3") #'pen-company-filetype-line)
;; company-mode long
(define-key pen-map (kbd "M-4") #'pen-company-filetype-long)
;; This uses ivy instead of company to present multiple lines
(define-key pen-acolyte-minor-mode-map (kbd "M-l") 'pen-complete-long)
{{< /highlight >}}


## Pen autocompletion modes {#pen-autocompletion-modes}


### Word {#word}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro pen-word-complete (&rest body)
  "This wraps around pen function calls to make them complete long"
  `(eval
    `(let ((max-tokens 1)
           (stop-sequence "##long complete##")
           (stop-sequences '("##long complete##"))
           (n-collate 1)
           (n-completions 40))
       ,',@body)))
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/S9dRnpubg37QIPETgssrL7YKr" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/S9dRnpubg37QIPETgssrL7YKr.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/S9dRnpubg37QIPETgssrL7YKr.js" id="asciicast-S9dRnpubg37QIPETgssrL7YKr" async></script>


### Words {#words}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro pen-words-complete (&rest body)
  "This wraps around pen function calls to make them complete long"
  `(eval
    `(let ((max-tokens 5)
           (stop-sequence "##long complete##")
           (stop-sequences '("##long complete##"))
           (n-collate 1)
           (n-completions 20))
       ,',@body)))
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/kbLWcmwHMNYxIeC2AUfgmqZqN" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/kbLWcmwHMNYxIeC2AUfgmqZqN.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/kbLWcmwHMNYxIeC2AUfgmqZqN.js" id="asciicast-kbLWcmwHMNYxIeC2AUfgmqZqN" async></script>


### Line {#line}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro pen-line-complete (&rest body)
  "This wraps around pen function calls to make them complete line only"
  `(eval
    `(let ((max-tokens 100)
           (stop-sequence "\n")
           (stop-sequences '("\n")))
       ,',@body)))
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/X9oeQhXxmRZYf1isnfXXnu56O" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/X9oeQhXxmRZYf1isnfXXnu56O.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/X9oeQhXxmRZYf1isnfXXnu56O.js" id="asciicast-X9oeQhXxmRZYf1isnfXXnu56O" async></script>


### Long {#long}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro pen-long-complete (&rest body)
  "This wraps around pen function calls to make them complete long"
  `(eval
    `(let ((max-tokens 200)
           (stop-sequence "##long complete##")
           (stop-sequences '("##long complete##")))
       ,',@body)))
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/PNDrLo9KQ5MdKzGPCEb46Fqn6" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/PNDrLo9KQ5MdKzGPCEb46Fqn6.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/PNDrLo9KQ5MdKzGPCEb46Fqn6.js" id="asciicast-PNDrLo9KQ5MdKzGPCEb46Fqn6" async></script>